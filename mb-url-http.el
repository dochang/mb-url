;;; mb-url-http.el --- Backends for `url-http'  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2016, 2018, 2021, 2022 ZHANG Weiyi

;; Author: ZHANG Weiyi <dochang@gmail.com>
;; Keywords: comm, data, processes

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Backends for `url-http'.

;;; Code:

(require 'cl-lib)
(require 'url-http)

(require 'mb-url)

(defcustom mb-url-http-backend nil
  "Backend for `url-http'."
  :type '(choice (function :tag "curl" mb-url-http-curl)
                 (function :tag "httpie" mb-url-http-httpie)
                 (const :tag "None" nil))
  :group 'mb-url)

;; TODO Add test code
(defcustom mb-url-http-stderr nil
  "Where the process writes the standard error to.

nil means the process writes the standard error to the \"*Messages*\" buffer.

t means mb-url creates a new buffer for every process.

A string means the process writes the standard error to the buffer named by the
string (create if not exists).

A function means the process writes the standard error to the return value of
that function.  The function must return a buffer, a pipe process or a
string (buffer name)."
  :type '(choice (const :tag "The \"*Messages*\" buffer" nil)
                 (const :tag "Create new buffer every time" t)
                 (string :tag "Buffer Name")
                 (symbol :tag "Function name")
                 (function :tag "Function"))
  :group 'mb-url)

(defun mb-url-http--goto-next-body ()
  "Goto next part of body."
  (re-search-forward "^\r?\n"))

(defun mb-url-http--delete-proxy-response ()
  "Delete the response message returned by proxy.

Some proxies return a message like this:

    HTTP/1.1 200 Connection established
    Proxy-Header: foo
    ...

    HTTP/1.1 200 OK
    Header: bar
    ...

    body...

This function deletes the first block (from proxy)."
  ;; [RFC draft][1] & [Privoxy code][2] use "Connection established".  But
  ;; [polipo][] & [cow][] use "Tunnel established".  I use `[^\r\n]` here for
  ;; compatibility.
  ;;
  ;; [1]: https://tools.ietf.org/html/draft-luotonen-web-proxy-tunneling-01#section-3.2
  ;; [2]: http://ijbswa.cvs.sourceforge.net/viewvc/ijbswa/current/jcc.c?view=markup
  ;; [polipo]: https://github.com/jech/polipo/blob/master/tunnel.c#L302
  ;; [cow]: https://github.com/cyfdecyf/cow/blob/master/proxy.go#L1160
  (goto-char (point-min))
  (when (looking-at-p "HTTP/[0-9]+\\.[0-9]+ 2[0-9][0-9] [^\r\n]* established\r?\n")
    (delete-region (point) (progn (mb-url-http--goto-next-body) (point)))))

(defun mb-url-http--delete-carriage-return ()
  "Delete carriage return from the header part."
  (save-excursion
    (let* (rnrn-end-of-headers
           nn-end-of-headers
           end-of-headers)
      (goto-char (point-min))
      (setq rnrn-end-of-headers (re-search-forward "\r\n\r\n" nil t))
      (goto-char (point-min))
      (setq nn-end-of-headers (re-search-forward "\n\n" nil t))
      (setq end-of-headers
            (cond ((not nn-end-of-headers)
                   rnrn-end-of-headers)
                  ((not rnrn-end-of-headers)
                   nil)
                  ((< rnrn-end-of-headers nn-end-of-headers)
                   rnrn-end-of-headers)))
      (when end-of-headers
        (save-restriction
          (narrow-to-region (point-min) end-of-headers)
          (goto-char (point-min))
          (while (re-search-forward "\r\n" nil t)
            (replace-match "\n")))))))

(defun mb-url-http--fix-header (header fix-function &optional last all list)
  "Fix all HEADER lines from response message.

FIX-FUNCTION should be a function of one argument.  It is called with header
values as its argument and returns the new value of HEADER.  If it returns a
string, the value of HEADER will be replaced with the string.  If it returns
nil, all HEADER lines will be removed.  If it returns t, all HEADER lines
keeps unchanged.

LAST, ALL, LIST will be passed to `mail-fetch-field'."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (and (not (buffer-narrowed-p))
           (mail-narrow-to-head))
      (goto-char (point-min))
      (let* ((hvals (mail-fetch-field header last all list nil))
             (fixed (funcall fix-function hvals)))
        (cond ((eq fixed t))
              ((null fixed)
               (goto-char (point-min))
               (mail-fetch-field header last all list t)
               (flush-lines "^[ \t]*$" (point-min) (point-max)))
              (t
               (goto-char (point-min))
               (mail-fetch-field header last all list t)
               (flush-lines "^[ \t]*$" (point-min) (point-max))
               (goto-char (point-max))
               (insert (concat header ": " fixed "\n"))))))))

(defun mb-url-http--delete-content-encoding ()
  "Delete \"Content-Encoding\" from headers.

Starting from Emacs 27, if `Content-Encoding' is `gzip' and Emacs
has zlib support, `url-handle-content-transfer-encoding', which
is called by `url-http-parse-headers', will decompress response
bodies and will delete `Content-Encoding' from headers.

External HTTP clients automatically decompress response bodies
but keep `Content-Encoding' undeleted.  This confuses
`url-handle-content-transfer-encoding' and `url-store-in-cache'.
Delete `Content-Encoding' manually."
  ;; [1]: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=e310843d9dc106187d0e45ef7f0b9cd90a881eec
  ;; [2]: https://github.com/emacs-mirror/emacs/commit/e310843d9dc106187d0e45ef7f0b9cd90a881eec
  ;; [3]: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36773
  (save-excursion
    (goto-char (point-min))
    (let ((end-of-headers (re-search-forward "\n\n" nil t))
          (case-fold-search t))
      (save-restriction
        (narrow-to-region (point-min) end-of-headers)
        (goto-char (point-min))
        (while (re-search-forward "Content-Encoding: .*\n" nil t)
          (replace-match ""))))))

(defun mb-url-http--reset-end-of-headers ()
  "Reset url-http-end-of-headers."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\r?\n" nil t)
      (backward-char 1)
      (set-marker (if (and (boundp 'url-http-end-of-headers)
                           (markerp url-http-end-of-headers))
                      url-http-end-of-headers
                    (setq url-http-end-of-headers (make-marker)))
                  (point)))))

(defun mb-url-http-sentinel (proc evt)
  "Sentinel used to fix built-in sentinel.

PROC is the process.

EVT describes the type of event."
  (when (string= evt "finished\n")
    (with-current-buffer (process-buffer proc)
      (mb-url-http--delete-carriage-return)
      (mb-url-http--delete-content-encoding)
      (mb-url-http--reset-end-of-headers)
      (goto-char (point-min))
      (url-http-end-of-document-sentinel proc evt))))

(defun mb-url-http-header-field-to-argument (header)
  "Convert HEADER to command line arguments."
  (let ((name (car header))
        (value (cdr header)))
    (cond ((null value)
           (format "%s:" name))
          ((string-match-p "\\`[ \t\n\r]*\\'" value)
           (format "%s;" name))
          (t
           (format "%s:%s" name value)))))

(defun mb-url-http-extra-headers (url)
  "Get extra HTTP headers for URL."
  (let ((no-cache (cdr-safe (assoc "Pragma" url-request-extra-headers)))
        (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization" url-request-extra-headers))
                            (not url-using-proxy))
                        nil
                      (let ((url-basic-auth-storage 'url-http-proxy-basic-auth-storage))
                        (url-get-authentication url-using-proxy nil 'any nil))))
        (real-fname (url-filename url))
        (host (url-host url))
        (auth (if (cdr-safe (assoc "Authorization" url-request-extra-headers))
                  nil
                (url-get-authentication (or (and (boundp 'proxy-info)
                                                 proxy-info)
                                            url) nil 'any nil)))
        (ref-url (and (fboundp 'url-http--get-referer)
                      (url-http--encode-string (url-http--get-referer url)))))
    (if (equal "" real-fname)
        (setq real-fname "/"))
    (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
    (append
     (seq-filter (lambda (header)
                   (and (not (assoc-string (car header) url-request-extra-headers t))
                        (cdr header)))
                 (list
                  (cons "From" url-personal-mail-address)
                  (cons "Accept-Encoding" url-mime-encoding-string)
                  (cons "Accept-Charset" (and url-mime-charset-string
                                              (url-http--encode-string url-mime-charset-string)))
                  (cons "Accept-Language" url-mime-language-string)
                  (cons "Accept" (or url-mime-accept-string "*/*"))
                  (cons "Proxy-Authorization" proxy-auth)
                  (cons "Authorization" auth)
                  (cons "If-Modified-Since" (and (not no-cache)
                                                 (member url-request-method '("GET" nil))
                                                 (let ((tm (url-is-cached url)))
                                                   (and tm
                                                        (url-get-normalized-date tm)))))
                  (cons "Referer" ref-url)
                  (cons "Content-Length" (and url-request-data
                                              (number-to-string
                                               (string-bytes url-request-data))))))
     (when (and (url-use-cookies url)
                (assoc-string "Cookie" url-request-extra-headers t))
       (mapcar (lambda (line)
                 (with-temp-buffer
                   (insert line)
                   (goto-char (point-min))
                   (search-forward "Cookie: ")
                   (replace-match "")
                   (cons "Cookie" (buffer-string))))
               (split-string
                (url-http--encode-string
                 (url-cookie-generate-header-lines
                  host real-fname
                  (equal "https" (url-type url))))
                "\r\n"
                t)))
     url-request-extra-headers)))

(defun mb-url-http-process-send-url-request-data (proc)
  "Send request data, in binary form, to PROC."
  (unless (mb-url-string-empty-p url-request-data)
    (set-process-coding-system proc 'binary 'binary)
    (process-send-string proc url-request-data))
  (process-send-eof proc)
  proc)

(defun mb-url-http--generate-name (url)
  "Generate process name based on URL."
  (format "*mb-url-http-%s-%s" url-request-method (url-recreate-url url)))

;;;###autoload
(defun mb-url-http (url callback cbargs &optional retry-buffer gateway-method)
  "Retrieve URL via `mb-url-http-backend'.

URL, CALLBACK, CBARGS, RETRY-BUFFER and GATEWAY-METHOD are the same arguments
of `url-http'."
  (let* ((url-request-method (or url-request-method "GET"))
         (name (mb-url-http--generate-name url))
         (buf (generate-new-buffer name))
         (mime-accept-string url-mime-accept-string)
         (nsm-noninteractive (or url-request-noninteractive
                                 (and (boundp 'url-http-noninteractive)
                                      url-http-noninteractive)))
         (referer (and (fboundp 'url-http--get-referer)
                       (url-http--encode-string (url-http--get-referer url))))
         (proc (funcall mb-url-http-backend
                        name url buf #'mb-url-http-sentinel)))
    ;; stuff ripped out of url-http
    (with-current-buffer buf
      (mm-disable-multibyte)
      (setq url-current-object url
            mode-line-format "%b [%s]")
      (dolist (var '(url-http-end-of-headers
                     url-http-content-type
                     url-http-content-length
                     url-http-transfer-encoding
                     url-http-after-change-function
                     url-http-response-version
                     url-http-response-status
                     url-http-chunked-length
                     url-http-chunked-counter
                     url-http-chunked-start
                     url-callback-function
                     url-callback-arguments
                     url-show-status
                     url-http-process
                     url-http-method
                     url-http-extra-headers
                     url-http-noninteractive
                     url-http-data
                     url-http-target-url
                     url-http-no-retry
                     url-http-connection-opened
                     url-mime-accept-string
                     url-http-proxy
                     url-http-referer))
        (set (make-local-variable var) nil))
      (setq url-http-method url-request-method
            url-http-extra-headers url-request-extra-headers
            url-http-noninteractive url-request-noninteractive
            url-http-data url-request-data
            ;; `url-http' will close the connection if:
            ;;
            ;;   - There is not a "Connection: keep-alive" header (HTTP/1.0)
            ;;   - There is a "Connection: close" header (HTTP/1.1 and greater)
            ;;
            ;; That means `url-http-process' cannot be `nil'.  We have to
            ;; assign it to a process object.
            url-http-process proc
            url-http-chunked-length nil
            url-http-chunked-start nil
            url-http-chunked-counter 0
            url-callback-function callback
            url-callback-arguments cbargs
            url-http-after-change-function 'url-http-wait-for-headers-change-function
            url-http-target-url url-current-object
            url-http-no-retry retry-buffer
            url-http-connection-opened nil
            url-mime-accept-string mime-accept-string
            url-http-proxy url-using-proxy
            url-http-referer referer))
    buf))

;;;###autoload
(defun mb-url-http-around-advice
    (fn url callback cbargs &optional retry-buffer &rest rest-args)
  "Around advice for `url-http'.

FN is the original function.

URL must be a parsed URL.  See `url-generic-parse-url' for details.

When retrieval is completed, the multibyte flag of the retrieval buffer is set
to nil, then execute the function CALLBACK.

CBARGS, RETRY-BUFFER and REST-ARGS are arguments for FN."
  ;; `rest-args' is required because `url-http' adds an argument called
  ;; `gateway-method' since Emacs 25.
  (apply (if mb-url-http-backend #'mb-url-http fn)
         url
         (lambda (&rest args)
           (set-buffer-multibyte nil)
           (apply callback args))
         cbargs retry-buffer rest-args))

(defun mb-url-http--stderr-buffer-name (url)
  "Generate process stderr buffer name based on URL."
  (format "*mb-url-http-%s-%s-ERROR*" url-request-method (url-recreate-url url)))

(defun mb-url-http-make-pipe-process (url name buffer command &optional sentinel)
  "Make a pipe process.

URL is used to create the process stderr buffer if needed.

Pass NAME, BUFFER, COMMAND and SENTINEL to `make-process' as is.

If SENTINEL is nil, `mb-url-http-sentinel' will be used."
  (let* ((stderr (cond ((null mb-url-http-stderr)
                        (messages-buffer))
                       ((eq mb-url-http-stderr t)
                        (mb-url-http--stderr-buffer-name url))
                       ((or (symbolp mb-url-http-stderr)
                            (functionp mb-url-http-stderr))
                        (funcall mb-url-http-stderr url))
                       (t
                        mb-url-http-stderr)))
         (proc (make-process :name name
                             :buffer buffer
                             :command command
                             :connection-type 'pipe
                             :stderr stderr
                             :sentinel (or sentinel #'mb-url-http-sentinel))))
    (mb-url-http-process-send-url-request-data proc)
    proc))


(defcustom mb-url-http-curl-program "curl"
  "Curl program."
  :type 'string
  :group 'mb-url)

(defcustom mb-url-http-curl-switches '()
  "List of strings specifying switches to be passed to Curl."
  :type '(repeat (string))
  :group 'mb-url)

(defun mb-url-http--curl-command-list (url)
  "Return curl command list for URL."
  `(,mb-url-http-curl-program
    "--silent" "--show-error" "--include" "--compressed"
    ,@(if (string= "HEAD" url-request-method)
          (list "--head")
        (list "--request" url-request-method))
    "--data-binary" "@-"
    ,@(apply #'append
             (mapcar (lambda (arg) (list "--header" arg))
                     (mapcar #'mb-url-http-header-field-to-argument
                             (mb-url-http-extra-headers url))))
    ,(url-recreate-url url)
    ,@mb-url-http-curl-switches))

(defun mb-url-http-sentinel--curl (proc evt)
  "Sentinel for Curl.

Curl return the proxy response before the actual remote server response.
It makes Emacs hard to parse the response message.  Delete the proxy response
first.

PROC is the process.

EVT describes the type of event."
  (when (string= evt "finished\n")
    (with-current-buffer (process-buffer proc)
      (mb-url-http--delete-proxy-response)))
  (mb-url-http-sentinel proc evt))

;;;###autoload
(defun mb-url-http-curl (name url buffer default-sentinel)
  "\"cURL\" backend for `mb-url-http'.

NAME is the process name.

URL is the url which curl sends the request data to.

BUFFER is the process buffer.

DEFAULT-SENTINEL is the default sentinel of mb-url.  But curl backend uses its
own sentinel instead."
  (mb-url-http-make-pipe-process
   url name buffer
   (mb-url-http--curl-command-list url)
   #'mb-url-http-sentinel--curl))


(defcustom mb-url-http-httpie-program "http"
  "HTTPie program."
  :type 'string
  :group 'mb-url)

(defcustom mb-url-http-httpie-switches '()
  "List of strings specifying switches to be passed to HTTPie."
  :type '(repeat (string))
  :group 'mb-url)

(defun mb-url-http--httpie-extra-headers (url)
  "Get extra HTTP headers for URL.

Remove internal headers for HTTPie from the headers."
  (seq-remove (lambda (pair)
                (member (car-safe pair)
                        ;; https://httpie.io/docs/cli/redirected-input
                        '("Content-Length")))
              (mb-url-http-extra-headers url)))

(defun mb-url-http--httpie-command-list (url)
  "Return httpie command list for URL."
  `(,mb-url-http-httpie-program
    "--print" "hb" "--pretty" "none"
    ,url-request-method ,(url-recreate-url url)
    ,@(mapcar #'mb-url-http-header-field-to-argument
              (mb-url-http--httpie-extra-headers url))
    ,@mb-url-http-httpie-switches))

;;;###autoload
(defun mb-url-http-httpie (name url buffer default-sentinel)
  "HTTPie backend for `mb-url-http'.

NAME is the process name.

URL is the url which httpie sends the request data to.

BUFFER is the process buffer.

DEFAULT-SENTINEL is the default sentinel of mb-url."
  (mb-url-http-make-pipe-process
   url name buffer
   (mb-url-http--httpie-command-list url)
   default-sentinel))

(provide 'mb-url-http)

;;; mb-url-http.el ends here
