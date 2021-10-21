;; -*- mode: scheme; coding: utf-8; -*-
;;
;; Copyright © 2019 IPDB Foundation
;; Copyright © 2020, 2021 David Dashyan <mail@davie.li>
;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (bigchaindb-packages bigchaindb)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  ;; #:use-module (gnu packages rust)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))


;;; Bigchaindb-abci
(define-public python-colorlog-for-bigchaindb-abci
  (package
    (inherit python-colorlog)
    (version "5.0.1")
    (source (origin
              (inherit (package-source python-colorlog))
              (uri (pypi-uri "colorlog" version))
              (sha256
               (base32
                "1sl8wps7d8xl2larlb7wkvr7sa3bpvyprq4y8ks04awn0qx02z7i"))))))

(define-public python-protobuf-for-bigchaindb-abci
  (package
    (inherit python-protobuf)
    (version "3.17.2")
    (source
     (origin
       (inherit (package-source python-protobuf))
       (uri (pypi-uri "protobuf" version))
       (sha256
        (base32
         "03fl72ayf5crfpn2qfdfnfsz31kvzfnzg8q29x56wwa6y2n50d2s"))))))

(define-public python-bigchaindb-abci
  (package
    (name "python-bigchaindb-abci")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bigchaindb-abci" version))
       (sha256
        (base32
         "1m177mprnl999dyykmad0bhspbqhh7m1ygf94ks9vh1zq346w8jl"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-colorlog" ,python-colorlog-for-bigchaindb-abci)
       ("python-gevent" ,python-gevent)
       ("python-protobuf" ,python-protobuf-for-bigchaindb-abci)))
    (home-page
     "https://github.com/davebryson/py-abci")
    (synopsis
     "Python based ABCI Server for Tendermint")
    (description
     "Python based ABCI Server for Tendermint")
    (license license:asl2.0)))


;;; Cryptoconditions
(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools-scm" version))
       (sha256
        (base32
         "14lfq6k4j1i2zfr0bsdc930j76h0vgxbjxd22sab5s87rdlmm4ni"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools" ,python-setuptools)))
    (home-page
     "https://github.com/pypa/setuptools_scm/")
    (synopsis
     "the blessed package to manage your versions by scm tags")
    (description
     "the blessed package to manage your versions by scm tags")
    (license license:expat)))

(define-public python-setuptools-rust
  (package
    (name "python-setuptools-rust")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools-rust" version))
       (sha256
        (base32
         "03jp7lnj7pmb3m8lq15rxi3x56m6hjhl209ygyf47bph4klhjw34"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-wheel" ,python-wheel)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-semantic-version" ,python-semantic-version)
       ("python-toml" ,python-toml)
       ("python-setuptools" ,python-setuptools)))
    (home-page
     "https://github.com/PyO3/setuptools-rust")
    (synopsis "Setuptools Rust extension plugin")
    (description "Setuptools Rust extension plugin")
    (license license:expat)))

(define-public python-cryptography-for-cryptoconditions
  (package
    (inherit python-cryptography)
    (version "3.4.7")
    (source
     (origin
       (inherit (package-source python-cryptography))
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "04x7bhjkglxpllad10821vxddlmxdkd3gjvp35iljmnj2s0xw41x"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'disable-rust
           (lambda* (#:key tests? #:allow-other-keys)
             (setenv "CRYPTOGRAPHY_DONT_BUILD_RUST" "1"))))))
    (native-inputs
     (append
      `(;; FIXME enable rust?
        ;; ("cargo" ,rust "cargo")
        ;; ("rust" ,rust)
        ;; ("rust-pyo3" ,rust-pyo3-0.13)
        ("python-setuptools-rust" ,python-setuptools-rust))
      (package-native-inputs python-cryptography)))))

(define-public python-base58-for-cryptoconditions
  (package
    (inherit python-base58)
    (version "2.1.0")
    (source
     (origin
       (inherit (package-source python-base58))
       (uri (pypi-uri "base58" version))
       (sha256
        (base32
         "18h1h1v3awwxcii7apirqijf6xgcg9pll8h772pf2q9w99xm86hp"))))
    (arguments ;; moved to pytest
     '(#:tests? #f))))

(define-public python-cryptoconditions
  (package
    (name "python-cryptoconditions")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptoconditions" version))
       (sha256
        (base32
         "162gr29g8mnj8p49j5cifh6pakdpwq03qjkm9pbyp017kh3nl7cd"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))
    (native-inputs
     `(("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-base58"       ,python-base58-for-cryptoconditions)
       ("python-cryptography" ,python-cryptography-for-cryptoconditions)
       ("python-pyasn1"       ,python-pyasn1)
       ("python-pynacl"       ,python-pynacl)))
    (home-page
     "https://github.com/bigchaindb/cryptoconditions/")
    (synopsis
     "Multi-algorithm, multi-level, multi-signature format for
expressing conditions and fulfillments according to the Interledger
Protocol (ILP).")
    (description
     "Multi-algorithm, multi-level, multi-signature format for
expressing conditions and fulfillments according to the Interledger
Protocol (ILP).")
    (license license:expat)))


;;; Bigchaindb
(define-public python-setproctitle-for-bigchaindb
  (package
    (inherit python-setproctitle)
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setproctitle" version))
       (sha256
        (base32
         "1pwp1lb9mf0kgg3sbf1z8dkfnmwcqvixc0by00s3sh2ji0n4gyvx"))))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build python-build-system)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (let ((cwd (getcwd)))
                 (setenv "PYTHONPATH"
                         (string-append
                          cwd "/build/"
                          (find (cut string-prefix? "lib" <>)
                                (scandir (string-append cwd "/build")))
                          ":"
                          (getenv "PYTHONPATH")))
                 (substitute* "tests/conftest.py"
                   (("\\[\"cc\"\\]")
                    "[\"gcc\"]"))
                 (invoke "python" "-m" "pytest"))))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("procps" ,procps)))))

(define-public python-logstats
  (package
    (name "python-logstats")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "logstats" version))
       (sha256
        (base32
         "1f0rls1l81bwqxh9ncjfx1c7dy5k4gdkr6qrn0kqfrdgddpdwrs1"))))
    (build-system python-build-system)
    (home-page "https://github.com/vrde/logstats")
    (synopsis
     "A module to collect and display stats for long running processes")
    (description
     "A util to output stats out of long running processes. Super useful when
you have daemons, or long running scripts, that need to output some data every
now and then. It supports the multiprocessing modules, so you can collect stats
from your child processes as well!")
    (license license:expat)))

(define-public python-requests-for-bigchaindb
  (package
    (inherit python-requests)
    (version "2.25.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "requests" version))
              (sha256
               (base32
                "015qflyqsgsz09gnar69s6ga74ivq5kch69s4qxz3904m7a3v5r7"))))))

(define-public python-rapidjson-for-bigchaindb
  (package
    (inherit python-rapidjson)
    (version "1.0")
    (source
     (origin
       (inherit (package-source python-rapidjson))
       (uri (pypi-uri "python-rapidjson" version))
       (sha256
        (base32
         "0771wik6nxmlwqa97xqxvqjfffybvlpj8i44wylmpf5h84gac7x6"))))
    (arguments
     `(#:configure-flags
       (list (string-append "--rj-include-dir="
                            (assoc-ref %build-inputs "rapidjson")
                            "/include/rapidjson"))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "python" "setup.py" "build"
                     (string-append "--rj-include-dir="
                                    (assoc-ref %build-inputs "rapidjson")
                                    "/include/rapidjson"))))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             ;; Some tests are broken.
             (delete-file "tests/test_base_types.py")
             (delete-file "tests/test_validator.py")
             (delete-file "tests/test_unicode.py") ;; FIXME
             (invoke "python" "-m" "pytest" "tests"))))))))

(define-public python-pyyaml-for-bigchaindb
  (package
    (inherit python-pyyaml)
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyYAML" version))
       (sha256
        (base32
         "0pm440pmpvgv5rbbnm8hk4qga5a292kvlm1bh3x2nwr8pb5p8xv0"))))))

(define-public python-pymongo-for-bigchaindb
  (package
    (inherit python-pymongo)
    (version "3.11.4")
    (source (origin
              (inherit (package-source python-pymongo))
              (uri (pypi-uri "pymongo" version))
              (sha256
               (base32
                "1rijz4irjirjqb40qcg51bkhdrvzhnmmlgn5k5lh4mvbn6qlr7ak"))))))

(define-public python-packaging-for-bigchaindb
  (package
    (inherit python-packaging)
    (version "20.9")
    (source
     (origin
       (inherit (package-source python-packaging))
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       (patches '())
       (sha256
        (base32
         "1rgadxvzvhac6wqa512bfj313ww6q3n18i9glzf67j0d6b0plcjv"))))
    (arguments ;; FIXME run tests
     `(#:tests? #f))))

(define-public python-flask-for-bigchaindb
  (package
    (inherit python-flask)
    (version "2.0.1")
    (source (origin
              (inherit (package-source python-flask))
              (uri (pypi-uri "Flask" version))
              (sha256
               (base32
                "0mcgwq7b4qd99mf5bsvs3wphchxarf8kgil4hwww3blj31xjak0w"))))
    (propagated-inputs
     `(("python-jinja2"
        ,(package
           (inherit python-jinja2)
           (version "3.0.1")
           (source
            (origin
              (inherit (package-source python-jinja2))
              (uri (pypi-uri "Jinja2" version))
              (sha256
               (base32
                "197ms1wimxql650245v63wkv04n8bicj549wfhp51bx68x5lhgvh"))))
           (propagated-inputs
            `(("python-markupsafe"
               ,(package
                  (inherit python-markupsafe)
                  (version "2.0.1")
                  (source
                   (origin
                     (inherit (package-source python-markupsafe))
                     (uri (pypi-uri "MarkupSafe" version))
                     (sha256
                      (base32
                       "02k2ynmqvvd0z0gakkf8s4idyb606r7zgga41jrkhqmigy06fk2r"))))))))))
       ("python-itsdangerous"
        ,(package
           (inherit python-itsdangerous)
           (version "2.0.1")
           (source
            (origin
              (inherit (package-source python-itsdangerous))
              (uri (pypi-uri "itsdangerous" version))
              (sha256
               (base32
                "1w6gfb2zhbcmrfj6digwzw1z68w6zg1q87rm6la2m412zil4swly"))))))
       ("python-werkzeug"
        ,(package
           (inherit python-werkzeug)
           (version "2.0.1")
           (source
            (origin
              (method url-fetch)
              (uri (pypi-uri "Werkzeug" version))
              (sha256
               (base32
                "0hlwawnn8c41f254qify5jnjj8xb97n294h09bqimzqhs0qdpq8x"))))
           (arguments ;; FIXME run tests
            `(#:tests? #f))))
       ("python-click" ,python-click)))
    (arguments ;; FIXME run tests
     `(#:tests? #f))))

(define-public python-flask-restful-for-bigchaindb
  (package
    (inherit python-flask-restful)
    (version "0.3.9")
    (source
     (origin
       (inherit (package-source python-flask-restful))
       (uri (pypi-uri "Flask-RESTful" version))
       (patches '())
       (sha256
        (base32
         "0gm5dz088v3d2k1dkcp9b3nnqpkk0fp2jly870hijj2xhc5nbv6c"))))
    (build-system python-build-system)))

(define-public python-flask-cors-for-bigchaindb
  (package
    (inherit python-flask-cors)
    (version "3.0.10")
    (source (origin
              (inherit (package-source python-flask-cors))
              (uri (pypi-uri "Flask-Cors" version))
              (sha256
               (base32
                "1pl16615fn1pc5n0vdrqlxm45mqsdjjxqv3gfkrs111v7wwkj25n"))))))

(define-public bigchaindb
  (package
    (name "bigchaindb")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BigchainDB/bigchaindb/")
             (commit "dependency-update"))) ;; (string-append "v" version)
       (file-name (git-file-name name version))
       (sha256
        (base32 "0apx3dpw0ayw6k3v2c80vmkiribmfwl47s94cc30fk4hkg3vrns0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; FIXME Tests
    (propagated-inputs
     `(("Python-aiohttp"          ,python-aiohttp)
       ("python-bigchaindb-abci"  ,python-bigchaindb-abci)
       ("python-cryptoconditions" ,python-cryptoconditions)
       ("python-flask"            ,python-flask-for-bigchaindb)
       ("python-flask-cors"       ,python-flask-cors-for-bigchaindb)
       ("python-flask-restful"    ,python-flask-restful-for-bigchaindb)
       ("gunicorn"                ,gunicorn)
       ("python-jsonschema"       ,python-jsonschema)
       ("python-logstats"         ,python-logstats)
       ("python-packaging"        ,python-packaging-for-bigchaindb)
       ("python-pymongo"          ,python-pymongo-for-bigchaindb)
       ("python-pyyaml"           ,python-pyyaml-for-bigchaindb)
       ("python-rapidjson"        ,python-rapidjson-for-bigchaindb)
       ("python-requests"         ,python-requests-for-bigchaindb)
       ("python-setproctitle"     ,python-setproctitle-for-bigchaindb)))
    (inputs
     `(("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/BigchainDB/bigchaindb/")
    (synopsis "BigchainDB: The Blockchain Database")
    (description "BigchainDB: The Blockchain Database")
    (license license:asl2.0)))
