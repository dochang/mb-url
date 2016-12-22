;;; mb-url-http.el --- Backends for `url-http'

;; Copyright (C) 2015 ZHANG Weiyi

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

;;;###autoload
(cl-defmacro define-mb-url-http-backend (backend-name &rest body)
  (let ((fn-name (intern (format "mb-url-http-%s" backend-name)))
        (docstring '()))
    (when (stringp (car body))
      (setq docstring (list (car body))
            body (cdr body)))
    (cl-destructuring-bind
        (&key buffer-name-function command-list-function sentinel) body
      (setq sentinel (or sentinel ''mb-url-http-sentinel))
      `(defun ,fn-name (url callback cbargs &optional retry-buffer)
         ,@docstring
         (let* ((url-request-method (or url-request-method "GET"))
                (buffer-name (funcall ,buffer-name-function url))
                (args (funcall ,command-list-function url))
                (proc (let ((process-connection-type nil))
                        (apply 'start-process buffer-name
                               (generate-new-buffer buffer-name) args))))
           (when url-request-data
             (set-process-coding-system proc 'binary 'binary)
             (process-send-string proc url-request-data))
           (process-send-eof proc)
           ;; stuff ripped out of url-http
           (with-current-buffer (process-buffer proc)
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
           (set-process-sentinel proc ,sentinel)
           (process-buffer proc))))))


(defun mb-url-http--curl-buffer-name (url)
  (format "*curl-%s-%s*" (url-recreate-url url) url-request-method))

(defun mb-url-http--curl-pair-to-args (pair)
  (list "--header" (format "%s: %s" (car pair) (cdr pair))))

(defcustom mb-url-http-curl-command "curl"
  "Executable for Curl command."
  :group 'mb-url)

(defun mb-url-http--curl-command-list (url)
  `(,mb-url-http-curl-command
    "--silent" "--include"
    ,@(if (string= "HEAD" url-request-method)
          (list "--head")
        (list "--request" url-request-method))
    ,@(if url-request-data (list "--data-binary" "@-") '())
    ,@(apply 'append
             (mapcar 'mb-url-http--curl-pair-to-args
                     url-request-extra-headers))
    ,(url-recreate-url url)))

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

;;;###autoload (autoload 'mb-url-http-curl "mb-url-http" nil t)
(define-mb-url-http-backend curl
  "Curl backend for `url-http'."
  :buffer-name-function 'mb-url-http--curl-buffer-name
  :command-list-function 'mb-url-http--curl-command-list
  :sentinel 'mb-url-http-sentinel--curl)


(defun mb-url-http--httpie-buffer-name (url)
  (format "*httpie-%s-%s*" (url-recreate-url url) url-request-method))

(defun mb-url-http--httpie-pair-to-args (pair)
  (format "%s:%s" (car pair) (cdr pair)))

(defcustom mb-url-http-httpie-command "http"
  "Executable for HTTPie command."
  :group 'mb-url)

(defun mb-url-http--httpie-command-list (url)
  `(,mb-url-http-httpie-command
    "--print" "hb" "--pretty" "none"
    ,url-request-method ,(url-recreate-url url)
    ,@(mapcar 'mb-url-http--httpie-pair-to-args
              url-request-extra-headers)))

;;;###autoload (autoload 'mb-url-http-httpie "mb-url-http" nil t)
(define-mb-url-http-backend httpie
  "HTTPie backend for `url-http'."
  :buffer-name-function 'mb-url-http--httpie-buffer-name
  :command-list-function 'mb-url-http--httpie-command-list)

(provide 'mb-url-http)

;;; mb-url-http.el ends here
