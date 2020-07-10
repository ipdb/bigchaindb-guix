;; -*- mode: scheme; coding: utf-8; -*-
;;
;; Copyright © 2019 IPDB Foundation
;; Copyright © 2020 David Dashyan <mail@davie.li>
;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;; Thanks to Marius Bakke and Ricardo Wurmus for the python-gevent package declaration.
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

(define-module (bigchaindb-guix packages bigchaindb)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public python-colorlog
  (package
    (name "python-colorlog")
    (version "4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colorlog" version))
       (sha256
        (base32
         "0hlv7x4qnb4jmccyv12087m0v5a0p3rjwn7g3z06xy68rcjipwrw"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail
     '(#:tests? #f))
    ;; Python-colorama required on win32 systems only.
    ;; (propagated-inputs
    ;;  `(("python-colorama" ,python-colorama)))
    (home-page
     "https://github.com/borntyping/python-colorlog")
    (synopsis "Log formatting with colors!")
    (description "colorlog.ColoredFormatter is a formatter for use with Python's
logging module that outputs records using terminal colors.")
    (license license:expat)))

;; ~v2.3 required by cryptoconditions
(define-public python-cryptography-2.3
  (package
    (inherit python-cryptography)
    (name "python-cryptography-2.3")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "1mnzf168vlxirq7fw9dm9zbvma7z8phc9cl5bffw5916m0y1244d"))))
    (arguments
     ;; FIXME: Tests fail
     '(#:tests? #f))))

;; ~v1.1 required by cryptoconditions
(define-public python-pynacl-1.1
  (package
    (inherit python-pynacl)
    (name "python-pynacl-1.1")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyNaCl" version))
       (modules '((guix build utils)))
       ;; Remove bundled libsodium.
       (snippet '(begin (delete-file-recursively "src/libsodium") #t))
       (sha256
        (base32
         "135gz0020fqx8fbr9izpwyq49aww202nkqacq0cw61xz99sjpx9j"))))))

(define-public python-pytest-pythonpath
  (package
    (name "python-pytest-pythonpath")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-pythonpath" version))
       (sha256
        (base32
         "0qhxh0z2b3p52v3i0za9mrmjnb1nlvvyi2g23rf88b3xrrm59z33"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail
     '(#:tests? #f))
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page
     "https://github.com/bigsassy/pytest-pythonpath")
    (synopsis
     "pytest plugin for adding to the PYTHONPATH from command line or configs")
    (description
     "This is a py.test plugin for adding to the PYTHONPATH from the pytests.ini
file before tests run.")
    (license license:expat)))

(define-public python-gevent-1.3
  (package
    (inherit python-gevent)
    (name "python-gevent")
    (version "1.3.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gevent" version))
              (sha256
               (base32
                "0b0fr04qdk1p4sniv87fh8z5psac60x01pv054kpgi94520g81iz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; unbunding libev and c-ares
                  (delete-file-recursively "deps")
                  #t))))
    (arguments
     `(#:modules ((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-26)
                  (guix build utils)
                  (guix build python-build-system))
       #:phases (modify-phases %standard-phases
                  (add-before 'patch-source-shebangs 'patch-hard-coded-paths
                    (lambda _
                      (substitute* "src/gevent/subprocess.py"
                        (("/bin/sh") (which "sh")))
                      (for-each (lambda (file)
                                  (substitute* file
                                    (("/bin/sh") (which "sh"))
                                    (("/bin/true") (which "true"))))
                                (find-files "src/greentest" "\\.py$"))
                      #t))
                  (add-before 'build 'do-not-use-bundled-sources
                    (lambda _
                      (setenv "GEVENTSETUP_EMBED" "0")

                      ;; Prevent building bundled libev.
                      (substitute* "setup.py"
                        (("run_make=_BUILDING")
                         "run_make=False"))
                      #t))
                  (add-before 'build 'add-greenlet-on-C_INCLUDE_PATH
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((greenlet (string-append
                                       (assoc-ref inputs "python-greenlet")
                                       "/include")))
                        (match (scandir greenlet
                                        (lambda (item)
                                          (string-prefix? "python" item)))
                          ((python)
                           (setenv "C_INCLUDE_PATH"
                                   (string-append greenlet "/" python ":"
                                                  (or (getenv "C_INCLUDE_PATH")
                                                      ""))))))
                      #t))
                  (add-before 'check 'pretend-to-be-CI
                    (lambda _
                      ;; A few tests are skipped due to network constraints or
                      ;; get longer timeouts when running in a CI environment.
                      ;; Piggy-back on that, as we need the same adjustments.
                      (setenv "TRAVIS" "1")
                      (setenv "APPVEYOR" "1")
                      #t))
                  (add-before 'check 'adjust-tests
                    (lambda _
                      (let ((disabled-tests
                             '(;; These tests rely on networking which is not
                               ;; available in the build container.
                               "test_urllib2net.py"
                               "test__server.py"
                               "test__server_pywsgi.py"
                               "test_socket.py"
                               "test__socket.py"
                               "test__socket_ssl.py"
                               "test__socket_dns.py"
                               "test__socket_dns6.py"
                               "test___example_servers.py"
                               "test__getaddrinfo_import.py"
                               "test__examples.py"
                               "test_httplib.py"
                               "test_https.py"
                               "test_urllib2_localnet.py"
                               "test_ssl.py"
                               "test__ssl.py"
                               ;; XXX: These tests borrow functionality from the
                               ;; Python builtin 'test' module, but it is not
                               ;; installed with the Guix Python distribution.
                               "test_smtpd.py"
                               "test_wsgiref.py"
                               "test_urllib2.py"
                               "test_thread.py"
                               "test_threading.py"
                               "test__threading_2.py"
                               ;; These tests rely on KeyboardInterrupts which do not
                               ;; work inside the build container for some reason
                               ;; (lack of controlling terminal?).
                               "test_subprocess.py"
                               "test__issues461_471.py"
                               ;; TODO: Patch out the tests that use getprotobyname, etc
                               ;; instead of disabling all the tests from these files.
                               "test__resolver_dnspython.py"
                               "test__doctests.py"
                               "test__all__.py"
                               "test___config.py"
                               "test__execmodules.py")))
                        (call-with-output-file "skipped_tests.txt"
                          (lambda (port)
                            (format port "~a~%"
                                    (string-join disabled-tests "\n"))))
                        #t)))
                  (replace 'check
                    (lambda _ #t)))))))

(define-public python-bigchaindb-abci
  (package
    (name "python-bigchaindb-abci")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bigchaindb-abci" version))
       (sha256
        (base32
         "08c027sj6k19pigw2g1nj5m08z1kvl5wvj0zjsiwdv2xlmm5j0xb"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail
     '(#:tests? #f))
    (propagated-inputs
     `(("python-colorlog" ,python-colorlog)
       ("python-gevent-1.3" ,python-gevent-1.3)
       ("python-protobuf-3.6" ,python-protobuf-3.6)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-pythonpath" ,python-pytest-pythonpath)))
    (home-page
     "https://github.com/davebryson/py-abci")
    (synopsis
     "Python based ABCI Server for Tendermint")
    (description
     "Python based ABCI Server for Tendermint")
    (license license:asl2.0)))

;; required by python-cryptoconditions
(define-public python-pytest-forked
  (package
    (name "python-pytest-forked")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-forked" version))
       (sha256
        (base32
         "000i4q7my2fq4l49n8idx2c812dql97qv6qpm2vhrrn9v6g6j18q"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page
     "https://github.com/pytest-dev/pytest-forked")
    (synopsis
     "run tests in isolated forked subprocesses")
    (description
     "run tests in isolated forked subprocesses")
    (license license:expat)))

(define-public python-cryptoconditions
  (package
    (name "python-cryptoconditions")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptoconditions" version))
       (sha256
        (base32
         "1g5b4gbagnx4d830d9dh22isdnapykxqpmxjivp5083a1kr3z3bb"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-base58" ,python-base58)
       ("python-cryptography-2.3" ,python-cryptography-2.3)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pynacl-1.1" ,python-pynacl-1.1)))
    (native-inputs
     `(;; Tests requirements:
       ("python-coverage" ,python-coverage)
       ("python-hypothesis" ,python-hypothesis)
       ("python-pep8" ,python-pep8)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pylint" ,python-pylint)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-forked" ,python-pytest-forked)
       ("python-pytest-xdist" ,python-pytest-xdist)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page
     "https://github.com/bigchaindb/cryptoconditions/")
    (synopsis
     "Multi-algorithm, multi-level, multi-signature format for expressing
conditions and fulfillments according to the Interledger Protocol (ILP).")
    (description "Cryptoconditions provide a mechanism to describe a signed
message such that multiple actors in a distributed system can all verify the
same signed message and agree on whether it matches the description." )
    (license license:expat)))

(define-public python-flask-cors
  (package
    (name "python-flask-cors")
    (version "3.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Cors" version))
       (sha256
        (base32
         "05id72xwvhni23yasdvpdd8vsf3v4j6gzbqqff2g04j6xcih85vj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-flask" ,python-flask)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (arguments
     '(#:tests? #f))
    ;; Flask-Cors-3.0.8/tests/decorator/test_exception_interception.py",
    ;; line 30, in test_acl_uncaught_exception_500
    (home-page
     "https://github.com/corydolphin/flask-cors")
    (synopsis
     "A Flask extension adding a decorator for CORS support")
    (description
     "A Flask extension for handling Cross Origin Resource Sharing (CORS),
making cross-origin AJAX possible. This package has a simple philosophy, when
you want to enable CORS, you wish to enable it for all use cases on a domain.
This means no mucking around with different allowed headers, methods, etc. By
default, submission of cookies across domains is disabled due to the security
implications, please see the documentation for how to enable credential'ed
requests, and please make sure you add some sort of CSRF protection before doing
so!")
    (license license:expat)))


(define-public python-gunicorn
  (package
    (name "python-gunicorn")
    ;; NOTE: new version available
    (version "19.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gunicorn" version))
       (sha256
        (base32
         "1wzlf4xmn6qjirh5w81l6i6kqjnab1n1qqkh7zsj1yb6gh4n49ps"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-setuptools" ,python-setuptools)))
  ;; XXX Package has defined docs requirements and extras
  ;; extras_require = {
  ;;     'gevent':  ['gevent>=0.13'],
  ;;     'eventlet': ['eventlet>=0.9.7'],
  ;;     'tornado': ['tornado>=0.2'],
  ;;     'gthread': [],
  ;;     'setproctitle': ['setproctitle'],
  ;; }
  (arguments
   ;; FIXME: Tests require unpackaged inputs
   '(#:tests? #f))
  (home-page "http://gunicorn.org")
  (synopsis "WSGI HTTP Server for UNIX")
  (description "WSGI HTTP Server for UNIX")
  (license license:expat)))

(define-public python-jsonschema-2.5
  ;; NOTE New version is available
  (package
    (name "python-jsonschema-2.5")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jsonschema" version))
              (sha256
               (base32
                "0hddbqjm4jq63y8jf44nswina1crjs16l9snb6m3vvgyg31klrrn"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail
     '(#:tests? #f))
    (native-inputs
     `(("python-vcversioner" ,python-vcversioner)))
    ;; NOTE: has extra requerements
    (home-page "https://github.com/Julian/jsonschema")
    (synopsis "Implementation of JSON Schema for Python")
    (description
     "Jsonschema is an implementation of JSON Schema for Python.")
    (license license:expat)))

(define-public python-logstats
  ;; NOTE New version is available
  (package
    (name "python-logstats")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "logstats" version))
       (sha256
        (base32
         "02b7bk99023j1bgs1as7qmndcwknxmq3m3lvhs7kqnqg2pkp34m1"))))
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

(define-public python-packaging-18.0
  ;; NOTE New version is available
  (package
    (inherit python-packaging)
    (name "python-packaging-18.0")
    (version "18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       (sha256
        (base32
         "01wq9c53ix5rz6qg2c98gy8n4ff768rmanifm8m5jpjiaizj51h8"))))))

(define-public python-pymongo-3.6
  ;; NOTE New version is available
  (package
    (inherit python-pymongo)
    (name "python-pymongo-3.6")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pymongo" version))
              (sha256
               (base32
                "15j7jxbag863axwqghb3ms1wkadyi5503ndj9lvl1vk2d62cpszp"))))))

(define-public python-pyyaml-5.1
  ;; NOTE This package definition supersedes guix's one.
  (package
    (name "python-pyyaml")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyYAML" version))
       (sha256
        (base32
         "1r5faspz73477hlbjgilw05xsms0glmsa371yqdd26znqsvg1b81"))))
    (build-system python-build-system)
    (inputs
     `(("libyaml" ,libyaml)))
    (home-page "http://pyyaml.org/wiki/PyYAML")
    (synopsis "YAML parser and emitter for Python")
    (description
     "PyYAML is a YAML parser and emitter for Python. PyYAML features a complete
YAML 1.1 parser, Unicode support, pickle support, capable extension API, and
sensible error messages. PyYAML supports standard YAML tags and provides
Python-specific tags that allow to represent an arbitrary Python object.")
    (license license:expat)))

(define-public python-rapidjson
  (package
    (name "python-rapidjson")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-rapidjson" version))
       (sha256
        (base32
         "11d0bld459pxly56c54q6br83qcqsd8likiq0rn2pgnr273jjxqa"))))
    ;; NOTE: There is rapidjson package in guix repo which is actually a
    ;; submodule of git repo of this wrapper. It maight be possible to include
    ;; it as input.
    (build-system python-build-system)
    (home-page
     "https://github.com/python-rapidjson/python-rapidjson")
    (synopsis "Python wrapper around rapidjson")
    (description "RapidJSON is an extremely fast C++ JSON parser and
serialization library: this module wraps it into a Python 3 extension, exposing
its serialization/deserialization (to/from either bytes, str or file-like
instances) and JSON Schema validation capabilities.")
    (license license:expat)))

(define-public bigchaindb
  (package
    (name "bigchaindb")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "BigchainDB" version))
       (sha256
        (base32
         "09aqc7iv89w9gjlakdi5zcv2g5lsq054xar8pj6qlv3zvjx6ddh3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX nasty hack. Need to provide flask or better release updated BigchaiDB
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "setup.py"
               (("flask~=0.12.4")
                "flask>=1.0.0"))
             #t)))
       ;; FIXME: Tests
       #:tests? #f))
    (propagated-inputs
     ;; NOTE pysha3 is python < 3.6 dependency
     `(("python-aiohttp" ,python-aiohttp)
       ("python-bigchaindb-abci" ,python-bigchaindb-abci)
       ("python-cryptoconditions" ,python-cryptoconditions)
       ("python-flask" ,python-flask)
       ("python-flask-cors" ,python-flask-cors)
       ("python-flask-restful" ,python-flask-restful)
       ("python-gunicorn" ,python-gunicorn)
       ("python-jsonschema-2.5" ,python-jsonschema-2.5)
       ("python-logstats" ,python-logstats)
       ("python-packaging-18.0" ,python-packaging-18.0)
       ("python-pymongo-3.6" ,python-pymongo-3.6)
       ("python-pyyaml-5.1" ,python-pyyaml-5.1)
       ("python-rapidjson" ,python-rapidjson)
       ("python-requests-2.20" ,python-requests-2.20)
       ("python-setproctitle" ,python-setproctitle)))
    (inputs
     `(("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/BigchainDB/bigchaindb/")
    (synopsis "BigchainDB: The Blockchain Database")
    (description "BigchainDB: The Blockchain Database")
    (license license:asl2.0)))

(define-public bigchaindb-guix
  (package
    (name "bigchaindb-guix")
    ;; NOTE not released yet
    (version "XXX")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ipdb/bigchaindb-guix.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                ;; NOTE not released yet
                "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'check 'pathch-load-extension-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "bigchaindb-guix/deployment/tendermint-init.scm"
               (("libtendermintinit")
                (format #f
                        "~a/lib/guile/3.0/extensions/libtendermintinit"
                        (assoc-ref outputs "out"))))
             #t)))))

    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("libtool" ,libtool)
       ("guile3.0-guix" ,guile3.0-guix)
       ("guile-json" ,guile-json)
       ("libsodium" ,libsodium)))
    (inputs
     `(("guile" ,guile-3.0)
       ("libsodium" ,libsodium)))
    (synopsis "")
    (description "")
    (home-page "")
    (license license:gpl3+)))
 
