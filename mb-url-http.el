;;; mb-url-http.el --- Backends for `url-http'

;; Copyright (C) 2015, 2016, 2018 ZHANG Weiyi

;; Author: ZHANG Weiyi <dochang@gmail.com>

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
  "Backend for url-http"
  :type 'function
  :group 'mb-url)

(defun mb-url-http--goto-next-body ()
  (re-search-forward "^\r\n"))

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
  (when (looking-at-p "HTTP/[0-9]+\\.[0-9]+ 2[0-9][0-9] [^\r\n]* established\r\n")
    (delete-region (point) (progn (mb-url-http--goto-next-body) (point)))))

(defun mb-url-http-sentinel (proc evt)
  (when (string= evt "finished\n")
    (with-current-buffer (process-buffer proc)
      (let ((url-http-end-of-headers
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "\r\n\r\n" nil t))))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "\r\n" url-http-end-of-headers t)
            (replace-match "\n")))
        (url-http-end-of-document-sentinel proc evt)))))

(defun mb-url-http-header-field-to-argument (header)
  (let ((name (car header))
        (value (cdr header)))
    (cond ((null value)
           (format "%s:" name))
          ((string-match-p "\\`[ \t\n\r]*\\'" value)
           (format "%s;" name))
          (t
           (format "%s:%s" name value)))))

(defun mb-url-http-process-send-url-request-data (proc)
  (unless (mb-url-string-empty-p url-request-data)
    (set-process-coding-system proc 'binary 'binary)
    (process-send-string proc url-request-data))
  (process-send-eof proc)
  proc)

(defun mb-url-http--generate-name (url)
  (format "*mb-url-http-%s-%s" url-request-method (url-recreate-url url)))

;;;###autoload
(defun mb-url-http (url callback cbargs &optional retry-buffer gateway-method)
  "Retrieve URL via `mb-url-http-backend'.

URL, CALLBACK, CBARGS, RETRY-BUFFER and GATEWAY-METHOD are the same arguments
of `url-http'."
  (let* ((url-request-method (or url-request-method "GET"))
         (name (mb-url-http--generate-name url))
         (buf (generate-new-buffer name))
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
                     url-http-data
                     url-http-target-url
                     url-http-no-retry
                     url-http-connection-opened
                     url-http-proxy))
        (set (make-local-variable var) nil))
      (setq url-http-method url-request-method
            url-http-extra-headers url-request-extra-headers
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
            url-http-proxy url-using-proxy))
    buf))

;;;###autoload
(defun mb-url-http-around-advice
    (fn url callback cbargs &optional retry-buffer &rest rest-args)
  "Around advice for `url-http'.

FN is the original function.

URL, CALLBACK, CBARGS, RETRY-BUFFER and REST-ARGS are arguments for FN."
  ;; `rest-args' is required because `url-http' adds an argument called
  ;; `gateway-method' since Emacs 25.
  (apply (if mb-url-http-backend #'mb-url-http fn)
         url callback cbargs retry-buffer rest-args))

(defun mb-url-http-make-pipe-process (name buffer command &optional sentinel)
  "Make a pipe process."
  (let ((proc (let ((process-connection-type nil))
                (apply #'start-process name buffer command))))
    (set-process-sentinel proc (or sentinel #'mb-url-http-sentinel))
    (mb-url-http-process-send-url-request-data proc)
    proc))


(defcustom mb-url-http-curl-program "curl"
  "Curl program."
  :group 'mb-url)

(defcustom mb-url-http-curl-switches '()
  "List of strings specifying switches to be passed to Curl."
  :group 'mb-url)

(defun mb-url-http--curl-command-list (url)
  `(,mb-url-http-curl-program
    "--silent" "--include"
    ,@(if (string= "HEAD" url-request-method)
          (list "--head")
        (list "--request" url-request-method))
    ,@(if (mb-url-string-empty-p url-request-data)
          '()
        (list "--data-binary" "@-"))
    ,@(apply #'append
             (mapcar (lambda (arg) (list "--header" arg))
                     (mapcar #'mb-url-http-header-field-to-argument
                             url-request-extra-headers)))
    ,(url-recreate-url url)
    ,@mb-url-http-curl-switches))

(defun mb-url-http-sentinel--curl (proc evt)
  "Curl returns the proxy response before the actual remote server response.
It makes Emacs hard to parse the response message.  Delete the proxy response
first."
  (when (string= evt "finished\n")
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (point-min))
        (mb-url-http--delete-proxy-response))))
  (mb-url-http-sentinel proc evt))

;;;###autoload
(defun mb-url-http-curl (name url buffer default-sentinel)
  "cURL backend for `mb-url-http'."
  (mb-url-http-make-pipe-process
   name buffer
   (mb-url-http--curl-command-list url)
   #'mb-url-http-sentinel--curl))


(defcustom mb-url-http-httpie-program "http"
  "HTTPie program."
  :group 'mb-url)

(defcustom mb-url-http-httpie-switches '()
  "List of strings specifying switches to be passed to HTTPie."
  :group 'mb-url)

(defun mb-url-http--httpie-command-list (url)
  `(,mb-url-http-httpie-program
    "--print" "hb" "--pretty" "none"
    ,url-request-method ,(url-recreate-url url)
    ,@(mapcar #'mb-url-http-header-field-to-argument
              url-request-extra-headers)
    ,@mb-url-http-httpie-switches))

;;;###autoload
(defun mb-url-http-httpie (name url buffer default-sentinel)
  "HTTPie backend for `mb-url-http'."
  (mb-url-http-make-pipe-process
   name buffer
   (mb-url-http--httpie-command-list url)
   default-sentinel))

(provide 'mb-url-http)

;;; mb-url-http.el ends here
