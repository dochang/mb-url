;;; test-mb-url.el --- Tests for mb-url

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

;; Tests for mb-url

;;; Code:

(require 'ert)
(require 'cl)
(require 'json)
(require 'url)
(require 'url-http)

(require 's)

(require 'mb-url-http)

(cl-defstruct response
  raw-string version status-code headers body json)

(defun response-header (field-name response)
  (cdr (assoc-string field-name (response-headers response) t)))

(defun test-mb-url--parse-response (&optional buffer)
  (setq buffer (current-buffer))
  (let ((resp (make-response)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (setf (response-raw-string resp) (buffer-string))
        (re-search-forward "^HTTP/")
        (setf (response-version resp) (buffer-substring (point) (progn (skip-chars-forward "0-9.") (point))))
        (setf (response-status-code resp) (read (current-buffer)))
        (re-search-forward "\n")
        (while (not (looking-at-p "^\n"))
          (re-search-forward "^\\([^:]*\\):\\(.*\\)\n")
          (let ((key (match-string 1))
                (val (s-trim (match-string 2))))
            (setf (response-headers resp)
                  (cons (cons key val) (response-headers resp)))))
        (re-search-forward "^\n")
        (setf (response-body resp) (buffer-substring (point) (point-max)))
        (when (string= "application/json" (response-header "Content-Type" resp))
          (setf (response-json resp)
                (json-read-from-string (response-body resp))))))
    resp))

(ert-deftest test-010-test-mb-url--parse-response ()
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
      (let ((resp (test-mb-url--parse-response)))
        (should (string= (response-raw-string resp) raw-string))
        (should (string= (response-version resp) "1.1"))
        (should (= (response-status-code resp) 200))
        (should (string= (response-header "Content-Length" resp) "230"))
        (should (string= (response-body resp) body))
        (should (string= (assoc-default 'url (response-json resp))
                         "https://httpbin.org/get"))))))

(defmacro define-mb-url-http-backend-test (backend)
  (let ((function-name (intern (format "mb-url-http-%s" backend))))
    `(ert-deftest ,(intern (format "test-050-mb-url-http-%s-GET" backend)) ()
       ,(format "Test GET for %s" function-name)
       (unwind-protect
           (progn
             (advice-add 'url-http :override ',function-name)
             (let ((buffer (url-retrieve-synchronously "https://httpbin.org/get?foo=bar" t t)))
               (with-current-buffer buffer
                 (goto-char (point-min))
                 (let* ((resp (test-mb-url--parse-response))
                        (json (response-json resp)))
                   (should (= (response-status-code resp) 200))
                   (should (string= (response-header "Content-Type" resp)
                                    "application/json"))
                   (should (string= (assoc-default 'foo (assoc-default 'args json))
                                    "bar"))))))
         (advice-remove 'url-http ',function-name)))))

(define-mb-url-http-backend-test "curl")

(define-mb-url-http-backend-test "httpie")

(provide 'test-mb-url)

;;; test-mb-url.el ends here
