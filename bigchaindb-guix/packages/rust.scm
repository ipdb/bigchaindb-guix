;; -*- mode: scheme; coding: utf-8; -*-
;; Copyright © 2021 David Dashyan <mail@davie.li>
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
(define-module (bigchaindb-guix packages rust)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public rust-assert-approx-eq-1
  (package
    (name "rust-assert-approx-eq")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "assert_approx_eq" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zagfwfad5wssmr830gk3489f97ppczv6xs627jxniwm6ssdl1rw"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page
     "https://github.com/ashleygwilliams/assert_approx_eq.git")
    (synopsis "assert approximately equal")
    (description "assert approximately equal")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-macros-backend-0.13
  (package
    (name "rust-pyo3-macros-backend")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros-backend" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rjxayd78l10hnyphk03bcvhm0jpsvnzn07lczhy7jsgv3jrgc47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Code generation for PyO3 package")
    (description "Code generation for PyO3 package")
    (license license:asl2.0)))

(define-public rust-pyo3-macros-0.13
  (package
    (name "rust-pyo3-macros")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3-macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fxi5lx5dl7xh469gr5xckyjy3r3c5dqypzxcj0fbhzf1hq2qzx4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-pyo3-macros-backend"
         ,rust-pyo3-macros-backend-0.13)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Proc macros for PyO3 package")
    (description "Proc macros for PyO3 package")
    (license license:asl2.0)))

(define-public rust-inventory-impl-0.1
  (package
    (name "rust-inventory-impl")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "inventory-impl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0lgs8kia3284s34g7078j820cn2viyb6cij86swklwhn93lr9h3m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page
     "https://github.com/dtolnay/inventory")
    (synopsis
     "Implementation of macros for the `inventory` crate")
    (description
     "Implementation of macros for the `inventory` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-ghost-0.1
  (package
    (name "rust-ghost")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ghost" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0yalg3g1g3cz63n3phy7cdhh7p2qd220mrpxy96alwxbpqdwynqs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/dtolnay/ghost")
    (synopsis "Define your own PhantomData")
    (description "Define your own PhantomData")
    (license (list license:expat license:asl2.0))))

(define-public rust-inventory-0.1
  (package
    (name "rust-inventory")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "inventory" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zzz5sgrkxv1rpim4ihaidzf6jgha919xm4svcrmxjafh3xpw3qg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-ctor" ,rust-ctor-0.1)
        ("rust-ghost" ,rust-ghost-0.1)
        ("rust-inventory-impl" ,rust-inventory-impl-0.1))))
    (home-page
     "https://github.com/dtolnay/inventory")
    (synopsis
     "Typed distributed plugin registration")
    (description
     "Typed distributed plugin registration")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-impl-0.3
  (package
    (name "rust-indoc-impl")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indoc-impl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1w58yg249kmzsn75kcj34qaxqh839l1hsaj3bzggy3q03wb6s16f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-proc-macro-hack"
         ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1)
        ("rust-unindent" ,rust-unindent-0.1))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
    (license (list license:expat license:asl2.0))))

(define-public rust-indoc-0.3
  (package
    (name "rust-indoc")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indoc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n2fd2wm1h005hd7pjgx4gv5ymyq4sxqn8z0ssw6xchgqs5ilx27"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build?
       #t
       #:cargo-inputs
       (("rust-indoc-impl" ,rust-indoc-impl-0.3)
        ("rust-proc-macro-hack"
         ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Indented document literals")
    (description "Indented document literals")
    (license (list license:expat license:asl2.0))))

(define-public rust-pyo3-0.13
  (package
    (name "rust-pyo3")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pyo3" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hq965lgi25dn578fpn9hjva6zjr1c8rl7lxywijq44aw7lbhds8"))))
    (native-inputs
     `(("python" ,python)))
    (build-system cargo-build-system)
    (arguments
     `(#:tests?
       #f ;; FIXME tests fail
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-1)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-hashbrown" ,rust-hashbrown-0.9)
        ("rust-indoc" ,rust-indoc-0.3)
        ("rust-inventory" ,rust-inventory-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-complex" ,rust-num-complex-0.3)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-paste" ,rust-paste-0.1)
        ("rust-pyo3-macros" ,rust-pyo3-macros-0.13)
        ("rust-serde" ,rust-serde-1)
        ("rust-unindent" ,rust-unindent-0.1))
       #:cargo-development-inputs
       (("rust-assert-approx-eq"
         ,rust-assert-approx-eq-1)
        ("rust-proptest" ,rust-proptest-0.10)
        ("rust-rustversion" ,rust-rustversion-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/pyo3/pyo3")
    (synopsis "Bindings to Python interpreter")
    (description "Bindings to Python interpreter")
    (license license:asl2.0)))
