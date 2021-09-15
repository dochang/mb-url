;;; mb-url.el --- Multiple Backends for Emacs URL package

;; Copyright (C) 2015, 2016, 2018, 2019 ZHANG Weiyi

;; Author: ZHANG Weiyi <dochang@gmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: comm, data, processes, hypermedia
;; URL: https://github.com/dochang/mb-url

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

;; <!-- markdown-link-check-disable -->
;; [![MELPA](http://melpa.org/packages/mb-url-badge.svg)](http://melpa.org/#/mb-url)
;; [![MELPA Stable](http://stable.melpa.org/packages/mb-url-badge.svg)](http://stable.melpa.org/#/mb-url)
;; [![Build Status](https://cloud.drone.io/api/badges/dochang/mb-url/status.svg)](https://cloud.drone.io/dochang/mb-url)
;; [![Build Status](https://travis-ci.org/dochang/mb-url.svg?branch=master)](https://travis-ci.org/dochang/mb-url)
;; [![Average time to resolve an issue](http://isitmaintained.com/badge/resolution/dochang/mb-url.svg)](http://isitmaintained.com/project/dochang/mb-url "Average time to resolve an issue")
;; [![Percentage of issues still open](http://isitmaintained.com/badge/open/dochang/mb-url.svg)](http://isitmaintained.com/project/dochang/mb-url "Percentage of issues still open")
;; [![Issues](https://img.shields.io/github/issues/dochang/mb-url.svg)](https://github.com/dochang/mb-url)
;; [![Pull Requests](https://img.shields.io/github/issues-pr/dochang/mb-url.svg)](https://github.com/dochang/mb-url)
;; [![GitHub](https://img.shields.io/github/license/dochang/mb-url)](https://github.com/dochang/mb-url/blob/master/LICENSE)
;; [![Say Thanks!](https://img.shields.io/badge/say-thanks-green)](https://saythanks.io/to/dochang)
;; <!--
;; See the following issues for details.
;;
;; <https://github.com/BlitzKraft/saythanks.io/issues/60>
;; <https://github.com/BlitzKraft/saythanks.io/issues/103>
;; -->
;; <!-- markdown-link-check-enable -->
;;
;; Multiple Backends for URL package.
;;
;; This package provides several backends for `url-retrieve' &
;; `url-retrieve-synchronously', which replace the internal implementation.
;;
;; The motivation of this package is I can't connect HTTPS url behind proxy
;; (Related bugs: [#11788][], [#12636][], [#18860][], [msg00756][], [#10][]).
;;
;; [#11788]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=11788
;; [#12636]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12636
;; [#18860]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=18860
;; [msg00756]: https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00756.html
;; [#10]: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=10

;; Notice:
;;
;; As the URL package has supported HTTPS over proxies supporting CONNECT since
;; Emacs 26, this package is no longer recommended.  But it can still be used
;; in Emacs < 26.

;; Installation:
;;
;; `mb-url' is available on [MELPA] and [el-get].
;;
;; [MELPA]: https://melpa.org/
;; [el-get]: https://github.com/dimitri/el-get
;;
;; To install `mb-url' from git repository, clone the repo, then add the repo
;; dir into `load-path'.
;;
;; `mb-url' depends on `cl-lib';  The test code also depends on `s'.
;;
;; NOTE: the test code requires GNU Emacs 24.4 and above because it uses the
;; new `nadvice' package.  `mb-url' may support GNU Emacs 24.3 and below but
;; it's not tested with those versions.

;; Backends:
;;
;; Currently only support `url-http'.
;;
;; `url-http`:
;;
;; Install `mb-url-http-around-advice' to use `mb-url-http' backends.
;;
;; ```elisp
;; (advice-add 'url-http :around 'mb-url-http-around-advice)
;; ```
;;
;; All backend functions receive `(name url buffer default-sentinel)', return a
;; process.
;;
;; `mb-url-http-backend' indicates the current backend.  If the backend is
;; `nil', which means no backend, `url-http' will be called.
;;
;; E.g.,
;;
;; ```elisp
;; (setq mb-url-http-backend 'mb-url-http-curl)
;; ```
;;
;; #### [cURL][]
;;
;; [cURL]: http://curl.haxx.se/
;;
;; ##### `mb-url-http-curl'
;;
;; cURL backend for `url-http'.
;;
;; ##### `mb-url-http-curl-program'
;;
;; cURL program.
;;
;; ##### `mb-url-http-curl-switches'
;;
;; cURL switches.
;;
;; #### [HTTPie][]
;;
;; [HTTPie]: http://httpie.org/
;;
;; ##### `mb-url-http-httpie'
;;
;; HTTPie backend for `url-http'.
;;
;; ##### `mb-url-http-httpie-program'
;;
;; HTTPie program.
;;
;; ##### `mb-url-http-httpie-switches'
;;
;; HTTPie switches.

;; License:
;;
;; GPLv3

;; Acknowledgements:
;;
;; <https://github.com/nicferrier/curl-url-retrieve>

;;; Code:

(require 'cl-lib)

(defgroup mb-url ()
  "Multiple Backends for URL package."
  :prefix "mb-url-"
  :group 'url)

(defmacro mb-url-with-gensyms (symbols &rest body)
  "Bind SYMBOLS to unique temporary symbols then execute BODY in the context."
  (declare (indent 1))
  `(let ,(mapcar (lambda (sym) `(,sym (cl-gensym))) symbols)
     ,@body))

(defun mb-url-string-empty-p (string)
  "Check whether STRING is nil or empty."
  (or (null string)
      (string= string "")))

(provide 'mb-url)

;;; mb-url.el ends here
