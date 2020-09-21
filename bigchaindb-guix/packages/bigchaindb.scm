;; -*- mode: scheme; coding: utf-8; -*-
;;
;; Copyright © 2019 IPDB Foundation
;; Copyright © 2020 David Dashyan <mail@davie.li>
;; Copyright © 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
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
       ("python-gevent" ,python-gevent)
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

(define-public python-logstats
  ;; NOTE New version is available
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

(define-public bigchaindb
  (package
    (name "bigchaindb")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "BigchainDB" version))
       (sha256
        (base32
         "0r2xd0x9bph49zjbppiv6f9wjb9ahcki2imlh2cvdb3lnvn5j76f"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; FIXME Tests
    (propagated-inputs
     ;; NOTE pysha3 is python < 3.6 dependency
     `(("python-aiohttp" ,python-aiohttp)
       ("python-bigchaindb-abci" ,python-bigchaindb-abci)
       ("python-cryptoconditions" ,python-cryptoconditions)
       ("python-flask" ,python-flask)
       ("python-flask-cors" ,python-flask-cors)
       ("python-flask-restful" ,python-flask-restful)
       ("gunicorn" ,gunicorn)
       ("python-jsonschema" ,python-jsonschema)
       ("python-logstats" ,python-logstats)
       ("python-packaging" ,python-packaging)
       ("python-pymongo" ,python-pymongo)
       ("python-pyyaml" ,python-pyyaml)
       ("python-rapidjson" ,python-rapidjson)
       ("python-requests" ,python-requests)
       ("python-setproctitle" ,python-setproctitle)))
    (inputs
     `(("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/BigchainDB/bigchaindb/")
    (synopsis "BigchainDB: The Blockchain Database")
    (description "BigchainDB: The Blockchain Database")
    (license license:asl2.0)))


