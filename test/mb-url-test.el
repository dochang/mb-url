;;; mb-url-test.el --- Tests for mb-url

;; Copyright (C) 2015, 2016, 2018, 2019 ZHANG Weiyi

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

;; Tests for mb-url

;;; Code:

(require 'ert)
(require 'cl)
(require 'json)
(require 'url)
(require 'url-http)

(require 's)

(require 'mb-url)
(require 'mb-url-http)

;; Do not truncate the backtrace.  This makes ERT easy to debug.
(setq ert-batch-backtrace-right-margin 256)

(setq mb-url-test--httpbin-prefix
      (let ((prefix (getenv "MB_URL_TEST__HTTPBIN_PREFIX")))
        (if (mb-url-string-empty-p prefix)
            "https://httpbin.org"
          prefix)))

(cl-defstruct (mb-url-test-response
               (:constructor mb-url-test-make-response)
               (:copier mb-url-test-copy-response))
  raw-string version status-code headers body json)

(defun mb-url-test-response-header (field-name response)
  (cdr (assoc-string field-name (mb-url-test-response-headers response) t)))

(defun mb-url-test-parse-response (&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((resp (mb-url-test-make-response)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (setf (mb-url-test-response-raw-string resp) (buffer-string))
        (re-search-forward "^HTTP/")
        (setf (mb-url-test-response-version resp)
              (buffer-substring (point)
                                (progn (skip-chars-forward "0-9.") (point))))
        (setf (mb-url-test-response-status-code resp) (read (current-buffer)))
        (re-search-forward "\n")
        (while (not (looking-at-p "^\n"))
          (re-search-forward "^\\([^:]*\\):\\(.*\\)\n")
          (let ((key (match-string 1))
                (val (s-trim (match-string 2))))
            (setf (mb-url-test-response-headers resp)
                  (cons (cons key val) (mb-url-test-response-headers resp)))))
        (re-search-forward "^\n")
        (setf (mb-url-test-response-body resp)
              (buffer-substring (point) (point-max)))
        (when (string= "application/json"
                       (mb-url-test-response-header "Content-Type" resp))
          (setf (mb-url-test-response-json resp)
                (json-read-from-string (mb-url-test-response-body resp))))))
    resp))

(ert-deftest mb-url-test-010-parse-response ()
  (let* ((headers "HTTP/1.1 200 OK
Server: nginx
Date: Wed, 02 Dec 2015 16:23:47 GMT
Content-Type: application/json
Content-Length: 230
Connection: keep-alive
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true

")
         (body "{
  \"args\": {},
  \"headers\": {
    \"Accept\": \"*/*\",
    \"Accept-Encoding\": \"gzip, deflate\",
    \"Host\": \"httpbin.org\"
  },
  \"url\": \"https://httpbin.org/get\"
}
")
         (raw-string (concat headers body)))
    (with-temp-buffer
      (insert raw-string)
      (goto-char (point-min))
      (let ((resp (mb-url-test-parse-response)))
        (should (string= (mb-url-test-response-raw-string resp) raw-string))
        (should (string= (mb-url-test-response-version resp) "1.1"))
        (should (= (mb-url-test-response-status-code resp) 200))
        (should (string= (mb-url-test-response-header "Content-Length" resp)
                         "230"))
        (should (string= (mb-url-test-response-body resp) body))
        (should (string= (assoc-default 'url (mb-url-test-response-json resp))
                         "https://httpbin.org/get"))))))

(ert-deftest mb-url-test-010-header-field-to-argument ()
  (mapc (lambda (case)
          (let ((field (car case))
                (expected (cdr case)))
            (string= (mb-url-http-header-field-to-argument field) expected)))
        '((("X-Foo1" . "bar") . "X-Foo1:bar")
          (("X-Foo2" . "") . "X-Foo2;")
          (("X-Foo3" . nil) . "X-Foo3"))))

(ert-deftest mb-url-test-050-http ()
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (mapc (lambda (backend)
                (let* ((mb-url-http-backend backend)
                       (url (format "%s/get?foo=bar" mb-url-test--httpbin-prefix))
                       (buffer (url-retrieve-synchronously url t t)))
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (let* ((resp (mb-url-test-parse-response))
                           (json (mb-url-test-response-json resp)))
                      (should (= (mb-url-test-response-status-code resp) 200))
                      (should (string=
                               (mb-url-test-response-header "Content-Type" resp)
                               "application/json"))
                      (should (string=
                               (assoc-default 'foo (assoc-default 'args json))
                               "bar"))))))
              (list 'mb-url-http-curl
                    #'mb-url-http-curl
                    'mb-url-http-httpie
                    #'mb-url-http-httpie))
        (mapc (lambda (backend)
                (let ((mb-url-http-backend backend)
                      (url (format "%s/get?foo=bar" mb-url-test--httpbin-prefix)))
                  (should-error (url-retrieve-synchronously url t t))))
              (list 'mb-url-test--foobar
                    #'mb-url-test--foobar)))
    (advice-remove 'url-http 'mb-url-http-around-advice)))

(ert-deftest mb-url-test-051-sentinal ()
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (mapc (lambda (backend)
                (let* ((mb-url-http-backend backend)
                       (url (format "%s/image/png" mb-url-test--httpbin-prefix))
                       (buffer (url-retrieve-synchronously url t t)))
                  (with-current-buffer buffer
                    (set-buffer-multibyte nil)
                    (goto-char (point-min))
                    (let ((end-of-headers
                           (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "\n\n" nil t))))
                      (should
                       (string=
                        (buffer-substring end-of-headers (+ end-of-headers 8))
                        (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)))))))
              (list 'mb-url-http-curl
                    #'mb-url-http-curl
                    'mb-url-http-httpie
                    #'mb-url-http-httpie)))
    (advice-remove 'url-http 'mb-url-http-around-advice)))

(provide 'mb-url-test)

;;; mb-url-test.el ends here
