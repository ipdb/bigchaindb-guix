;;; guix.scm -- Guix package definition built from local source-tree
;; Copyright © 2020 David Dashyan <mail@davie.li>
;;
;; This file is part of bigchaindb-guix
;;
;; bigchaindb-guix is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; bigchaindb-guix is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with bigchaindb-guix.  If not, see <http://www.gnu.org/licenses/>.

(use-modules ((guix licenses) #:prefix license:)
             (guix build-system gnu)
             (guix gexp)
             (guix packages)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages autotools)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages package-management)
             (guix utils)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-1))

(set! %load-path (cons (current-source-directory) %load-path))
(use-modules (bigchaindb-guix packages bigchaindb))

(define %srcdir
  (current-source-directory))

(define %gitignore
  (filter
   (lambda (x) (not (equal? x "")))
   (string-split (get-string-all
                  (open-input-pipe "git check-ignore *"))
    #\newline)))

(define (keep-file? file _)
  "Check whether FILE is in current git tree."
  (not (any (lambda (x) (string-suffix? x file))
            %gitignore)))

(define (guix-hash)
  (substring
   (get-string-all (open-input-pipe
                    (format #f "guix hash -r ~a" %srcdir)))
   0 7))

(define-public bigchaindb-guix-local
  (package
    (name "bigchaindb-guix-local")
    (version (guix-hash))
    (source (local-file %srcdir
                       #:recursive? #t
                       #:select? keep-file?))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("libtool" ,libtool)))
    (inputs
     `(("guile" ,guile-3.0-latest)
       ("guile3.0-guix" ,guix)
       ("guile-json" ,guile-json)
       ("guile-parted" ,guile-parted)))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))

bigchaindb-guix-local
