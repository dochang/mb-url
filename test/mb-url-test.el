;;; mb-url-test.el --- Tests for mb-url  -*- lexical-binding: t; -*-

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
(require 'ert-async)
(require 'promise)

(require 'mb-url)
(require 'mb-url-http)

;; Do not truncate the backtrace.  This makes ERT easy to debug.
(setq ert-batch-backtrace-right-margin 256)

(setq ert-async-timeout 60)

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

(ert-deftest mb-url-test-030-http--goto-next-body ()
  (mapc (lambda (case)
          (let ((text (nth 0 case))
                (char (nth 1 case))
                (errtype (nth 2 case)))
            (with-temp-buffer
              (insert text)
              (goto-char (point-min))
              (cond (errtype
                     (should-error (mb-url-http--goto-next-body) :type errtype))
                    (t
                     (mb-url-http--goto-next-body)
                     (should (= (following-char) char)))))))
        '(("a\r\nb\r\n\r\nc" ?c nil)
          ("a\nb\n\nc" ?c nil)
          ("a\rb\r\rc" ?c search-failed))))

(ert-deftest mb-url-test-031-http--delete-proxy-response ()
  (mapc (lambda (case)
          (let ((before (car case))
                (after (cdr case)))
            (with-temp-buffer
              (insert before)
              (goto-char (point-min))
              (mb-url-http--delete-proxy-response)
              (should (string= (buffer-string) after)))))
        '(("HTTP/1.1 200 Connection established\r\nProxy-Header: foo\r\n\r\nHTTP/1.1 200 OK\r\nHeader: bar\r\n\r\nbody...\r\n" . "HTTP/1.1 200 OK\r\nHeader: bar\r\n\r\nbody...\r\n")
          ("HTTP/1.1 200 Connection established\nProxy-Header: foo\n\nHTTP/1.1 200 OK\nHeader: bar\n\nbody...\n" . "HTTP/1.1 200 OK\nHeader: bar\n\nbody...\n")
          ("HTTP/1.1 200 Connection established\rProxy-Header: foo\r\rHTTP/1.1 200 OK\rHeader: bar\r\rbody...\r" . "HTTP/1.1 200 Connection established\rProxy-Header: foo\r\rHTTP/1.1 200 OK\rHeader: bar\r\rbody...\r"))))

(ert-deftest mb-url-test-032-http--delete-carriage-return ()
  (mapc (lambda (case)
          (let ((before (car case))
                (after (cdr case)))
            (with-temp-buffer
              (insert before)
              (goto-char (point-min))
              (mb-url-http--delete-carriage-return (current-buffer))
              (should (string= (buffer-string) after)))))
        '(("HTTP/1.1 200 OK\r\nHeader: bar\r\n\r\nbody...\r\n" . "HTTP/1.1 200 OK\nHeader: bar\n\nbody...\r\n")
          ("HTTP/1.1 200 OK\nHeader: bar\n\nbody...\n" . "HTTP/1.1 200 OK\nHeader: bar\n\nbody...\n")
          ("HTTP/1.1 200 OK\nHeader: bar\n\nline1...\r\nline2...\r\n" . "HTTP/1.1 200 OK\nHeader: bar\n\nline1...\r\nline2...\r\n")
          ("HTTP/1.1 200 OK\rHeader: bar\r\rbody...\r" . "HTTP/1.1 200 OK\rHeader: bar\r\rbody...\r"))))

(ert-deftest mb-url-test-050-http ()
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (mapc (lambda (mb-url-http-backend)
                ;; GET
                (let* ((url (format "%s/get?foo=bar" mb-url-test--httpbin-prefix))
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
                               "bar")))))
                ;; POST with request data
                (let* ((url (format "%s/post" mb-url-test--httpbin-prefix))
                       (url-request-method "POST")
                       (url-request-extra-headers '(("Content-Type" . "text/plain")))
                       (url-request-data "foobar")
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
                               (assoc-default 'Content-Type (assoc-default 'headers json))
                               "text/plain"))
                      (should (string= (assoc-default 'data json) url-request-data))))))
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

(ert-deftest mb-url-test-052-unibyte ()
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (mapc (lambda (backend)
                (let* ((mb-url-http-backend backend)
                       (url (format "%s/post" mb-url-test--httpbin-prefix))
                       (url-request-method "POST")
                       (url-request-extra-headers '(("Content-Type" . "text/plain")))
                       (url-request-data "你好，世界")
                       (buffer (url-retrieve-synchronously url t t)))
                  (with-current-buffer buffer
                    (goto-char (point-min))
                    (let* ((resp (mb-url-test-parse-response))
                           (json (mb-url-test-response-json resp)))
                      (should (= (mb-url-test-response-status-code resp) 200))
                      (should (string= (mb-url-test-response-header "Content-Type" resp) "application/json"))
                      (should (string= (assoc-default 'Content-Type (assoc-default 'headers json)) "text/plain"))
                      (should (string= (assoc-default 'data json) url-request-data))))))
              (list 'mb-url-http-curl
                    #'mb-url-http-curl
                    'mb-url-http-httpie
                    #'mb-url-http-httpie)))
    (advice-remove 'url-http 'mb-url-http-around-advice)))

(defun mb-url-test--url-retrieve-synchronously (url &optional silent inhibit-cookies timeout)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL.

If SILENT is non-nil, don't do any messaging while retrieving.
If INHIBIT-COOKIES is non-nil, refuse to store cookies.  If
TIMEOUT is passed, it should be a number that says (in seconds)
how long to wait for a response before giving up."
  (url-do-setup)

  (let ((retrieval-done nil)
	(start-time (current-time))
        (url-asynchronous nil)
        (asynch-buffer nil))
    (print "1111111111111111111111111111")
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))
			nil silent inhibit-cookies))
    (print "222222222222222222222222222222222")
    (print asynch-buffer)
    (if (null asynch-buffer)
        ;; We do not need to do anything, it was a mailto or something
        ;; similar that takes processing completely outside of the URL
        ;; package.
        nil
      (let ((proc (get-buffer-process asynch-buffer)))
	;; If the access method was synchronous, `retrieval-done' should
	;; hopefully already be set to t.  If it is nil, and `proc' is also
	;; nil, it implies that the async process is not running in
	;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
	;; url-file.el should probably set something like a `url-process'
	;; buffer-local variable so we can find the exact process that we
	;; should be waiting for.  In the mean time, we'll just wait for any
	;; process output.
        (print "333333333333333")
        (print proc)
	(while (and (not retrieval-done)
                    (or (not timeout)
			(time-less-p (time-since start-time) timeout)))
          (print "444444444444444444444444444")
	  (url-debug 'retrieval
		     "Spinning in url-retrieve-synchronously: %S (%S)"
		     retrieval-done asynch-buffer)
          (print "55555555555555555555555555555")
          (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
              (setq proc (get-buffer-process
                          (setq asynch-buffer
                                (buffer-local-value 'url-redirect-buffer
                                                    asynch-buffer))))
            (print "6666666666666666666666")
            (print (and proc (memq (process-status proc)
                                   '(closed exit signal failed))
                        ;; Make sure another process hasn't been started.
                        (eq proc (or (get-buffer-process asynch-buffer) proc))))
            (print proc)
            (when proc
              (print (memq (process-status proc)
                           '(closed exit signal failed)))
              (print (process-status proc))
              (print (process-command proc))
              (print (eq proc (or (get-buffer-process asynch-buffer) proc))))
            (if (and proc (memq (process-status proc)
                                '(closed exit signal failed))
                     ;; Make sure another process hasn't been started.
                     (eq proc (or (get-buffer-process asynch-buffer) proc)))
                ;; FIXME: It's not clear whether url-retrieve's callback is
                ;; guaranteed to be called or not.  It seems that url-http
                ;; decides sometimes consciously not to call it, so it's not
                ;; clear that it's a bug, but even then we need to decide how
                ;; url-http can then warn us that the download has completed.
                ;; In the mean time, we use this here workaround.
		;; XXX: The callback must always be called.  Any
		;; exception is a bug that should be fixed, not worked
		;; around.
		(progn ;; Call delete-process so we run any sentinel now.
		  (delete-process proc)
		  (setq retrieval-done t)))
            (print "777777777777777777777")
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (or (with-local-quit
			  (accept-process-output proc 1))
			(null proc))
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).  If we got
	      ;; a quit, just stop.
	      (when quit-flag
		(delete-process proc))
              (setq proc (and (not quit-flag)
			      (get-buffer-process asynch-buffer))))
            (print "888888888888888888"))))
      (print "99999999999999999")
      asynch-buffer)))

(ert-deftest-async mb-url-test-053-stderr (done)
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (promise-chain
         (promise-all
          (vconcat
           (mapcar (lambda (errbuf)
                     (let* ((mb-url-http-backend
                             (lambda (name url buffer default-sentinel)
                               (mb-url-http-make-pipe-process
                                url name buffer
                                '("/bin/sh" "-c" "echo \"DEADBEEF\" >&2 ; exit 1")
                                default-sentinel)))
                            (url (format "%s/get" mb-url-test--httpbin-prefix))
                            (mb-url-http-stderr (lambda (url) errbuf))
                            (begin (with-current-buffer errbuf (buffer-string))))
                       (promise-new
                        (lambda (resolve reject)
                          (url-retrieve url (lambda (&rest args)))
                          (run-with-idle-timer 5 nil
                                               (lambda (before)
                                                 (print "3333333333333333")
                                                 (with-local-quit
                                                   (while (accept-process-output (get-buffer-process errbuf))))
                                                 (let ((after (with-current-buffer errbuf (buffer-string))))
                                                   (should (string-prefix-p (concat before "DEADBEEF\n") after)))
                                                 (funcall resolve errbuf))
                                               before)))))
                   (list (generate-new-buffer "mb-url-test-053-stderr")))))
         (then (lambda (&rest args)
                 (funcall done)))))
    (advice-remove 'url-http 'mb-url-http-around-advice)))

(ert-deftest mb-url-test-054-stderr ()
  (unwind-protect
      (progn
        (advice-add 'url-http :around 'mb-url-http-around-advice)
        (mapc (lambda (errbuf)
                (let* ((mb-url-http-backend
                        (lambda (name url buffer default-sentinel)
                          (let ((proc (mb-url-http-make-pipe-process
                                       url name buffer
                                       '("/bin/sh" "-c" "sleep 5s ; echo \"DEADBEEF\" >&2 ; exit 1")
                                       default-sentinel)))
                            ;; ;; Dump stdout, otherwise emacs will stuck.
                            ;; ;;
                            ;; ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Accepting-Output.html
                            ;; (while (accept-process-output proc))
                            ;; (while (accept-process-output (get-buffer-process errbuf)))
                            (print "###############")
                            (print (process-status proc))
                            proc)))
                       (url (format "%s/get" mb-url-test--httpbin-prefix))
                       (mb-url-http-stderr (lambda (url) errbuf))
                       (message-log-max t))
                  (with-current-buffer errbuf
                    (let (before after)
                      (setq before (buffer-string))
                      ;; (print "2222222222222")
                      ;; (while (accept-process-output (get-buffer-process errbuf)))
                      ;; (while (process-live-p (get-buffer-process errbuf))
                      ;;   (sleep-for 1))
                      ;; (print "1111111111111")
                      (with-timeout (60 (error "Timeout"))
                        (mb-url-test--url-retrieve-synchronously url t t))
                      ;; (if (>= emacs-major-version 26)
                      ;;     (url-retrieve-synchronously url t t 60)
                      ;;   (with-timeout (60 (error "Timeout"))
                      ;;     (url-retrieve-synchronously url t t)))
                      ;; (print "333333333333333")
                      (setq after (buffer-string))
                      ;; (print "44444444444444")
                      (should (string-prefix-p (concat before "DEADBEEF\n") after))))))
              (list (generate-new-buffer "mb-url-test-053-stderr")
                    ;; (messages-buffer)
                    )))
    (advice-remove 'url-http 'mb-url-http-around-advice)))

(provide 'mb-url-test)

;;; mb-url-test.el ends here
