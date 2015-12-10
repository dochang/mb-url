;;; mb-url.el --- Multiple Backends for Emacs URL package

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

;; [![MELPA](http://melpa.org/packages/mb-url-badge.svg)](http://melpa.org/#/mb-url)
;; [![MELPA Stable](http://stable.melpa.org/packages/mb-url-badge.svg)](http://stable.melpa.org/#/mb-url)
;; [![wercker status](https://app.wercker.com/status/7649d473775bd7832c7754b62f859897/m "wercker status")](https://app.wercker.com/project/bykey/7649d473775bd7832c7754b62f859897)
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
;; All backend functions have the same type signature of `url-http'.  Any
;; backend can be used as an override advice of `url-http'.
;;
;; E.g.,
;;
;; ```elisp
;; (advice-add 'url-http :override 'mb-url-http-curl)
;; ```
;;
;; `url-http`:
;;
;; #### [Curl][]
;;
;; [Curl]: http://curl.haxx.se/
;;
;; ##### `mb-url-http-curl'
;;
;; Advice for `url-http'.
;;
;; ##### `mb-url-http-curl-command'
;;
;; Executable for Curl command.
;;
;; #### [HTTPie][]
;;
;; [HTTPie]: http://httpie.org/
;;
;; ##### `mb-url-http-httpie'
;;
;; Advice for `url-http'.
;;
;; ##### `mb-url-http-httpie-command'
;;
;; Executable for HTTPie command.

;; License:
;;
;; GPLv3

;; Acknowledgements:
;;
;; https://github.com/nicferrier/curl-url-retrieve

;;; Code:

(defgroup mb-url ()
  "Multiple Backends for URL package."
  :prefix "mb-url-"
  :group 'url)

(provide 'mb-url)

;;; mb-url.el ends here
