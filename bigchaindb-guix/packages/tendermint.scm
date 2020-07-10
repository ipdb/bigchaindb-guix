;; -*- mode: scheme; coding: utf-8; -*
;; Copyright © 2019 IPDB Foundation
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

;; XXX Source based tendermint build
;; TODO Cleanup modules
;; TODO maybe Go kit ubrella package?
;; TODO go-github-com-fortytw2-leaktest test fails
;;      src/github.com/fortytw2/leaktest/leaktest_test.go:19:11: missing ...
;;      in args forwarded to printf-like function

(define-module (bigchaindb-guix packages tendermint)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))


(define-public tendermint-bin
  ;; XXX Warning zipbomb!
  (package
    (name "tendermint-bin")
    (version "0.31.5")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/tendermint/tendermint"
                    "/releases/download/v" version
                    "/tendermint_v" version "_linux_amd64.zip"))
              (sha256
               (base32
                "124k80vadyfx4ddh8n2bradvvfmzcfwamyfr94c5n0jb6ql5v1hw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (install-file
          (string-append (assoc-ref %build-inputs "source") "/tendermint")
          (string-append %output "/bin")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://tendermint.com/")
    (synopsis "Tendermint Core (BFT Consensus) in Go")
    (description "Tendermint Core is Byzantine Fault Tolerant (BFT) middleware
that takes a state transition machine - written in any programming language -
and securely replicates it on many machines.")
    (license license:asl2.0)))


;; Libraries for tendermint source based package

(define-public go-github-com-btcsuite-btcd-btcec-tendermint
  (let ((commit "ed77733ec07dfc8a513741138419b8d9d3de9d2d")
        (revision "0"))
    (package
      (inherit go-github-com-btcsuite-btcd-btcec)
      (name "tendermint-go-github-com-btcsuite-btcd-btcec")
      (version (git-version "0.12.0-beta" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/btcsuite/btcd.git")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0v8qb9qcqqn2s6zbxy2ywlqywrlbdb4pa2bfmypz0k1cjaz8mrcz")))))))

(define-public go-github-com-fortytw2-leaktest
  (package
    (name "go-github-com-fortytw2-leaktest")
    (version "1.2.0") ;; NOTE version 1.3.0 available
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fortytw2/leaktest.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lf9l6zgzjbcc7hmcjhhg3blx0y8icyxvjmjqqwfbwdk502803ra"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fortytw2/leaktest"
       #:tests? #f))
    (synopsis " Goroutine Leak Detector")
    (description "Refactored, tested variant of the goroutine leak detector
found in both net/http tests and the cockroachdb source tree. Takes a snapshot
of running goroutines at the start of a test, and at the end - compares the two
and voila. Ignores runtime/sys goroutines. Doesn't play nice with t.Parallel()
right now, but there are plans to do so.")
    (home-page "https://github.com/fortytw2/leaktest")
    (license license:bsd-3)))

(define-public go-github-com-go-kit-kit-log
  (package
    (name "go-github-com-go-kit-kit-log")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-kit/kit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09038mnw705h7isbjp8dzgp2i04bp5rqkmifxvwc5xkh75s00qpw"))))
    (build-system go-build-system)
    (propagated-inputs
     `(("go-github-com-go-logfmt-logfmt" ,go-github-com-go-logfmt-logfmt)))
    (arguments
     `(#:unpack-path "github.com/go-kit/kit"
       #:import-path "github.com/go-kit/kit/log"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Go kit microservices library log package")
    (description "`Go kit' log package provides a minimal interface for
structured logging in services. It may be wrapped to encode conventions, enforce
type-safety, provide leveled logging, and so on. It can be used for both typical
application log events, and log-structured data streams.")
    (home-page "https://gokit.io")
    (license license:expat)))

(define-public go-github-com-go-kit-kit-metrics
  (package
    (name "go-github-com-go-kit-kit-metrics")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-kit/kit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09038mnw705h7isbjp8dzgp2i04bp5rqkmifxvwc5xkh75s00qpw"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/go-kit/kit"
       #:import-path "github.com/go-kit/kit/metrics"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Go kit microservices library metrics package")
    (description "`Go kit' metrics package provides a set of uniform interfaces
for service instrumentation. It has counters, gauges, and histograms, and
provides adapters to popular metrics packages, like expvar, StatsD, and
Prometheus.")
    (home-page "https://gokit.io")
    (license license:expat)))

(define-public go-github-com-go-logfmt-logfmt
  (package
    (name "go-github-com-go-logfmt-logfmt")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-logfmt/logfmt.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06smxc112xmixz78nyvk3b2hmc7wasf2sl5vxj1xz62kqcq9lzm9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-logfmt/logfmt"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "Package logfmt marshals and unmarshals logfmt messages")
    (description "Package logfmt implements utilities to marshal and unmarshal
data in the logfmt format. It provides an API similar to encoding/json and
encoding/xml.")
    (home-page "https://github.com/go-logfmt/logfmt")
    (license license:expat)))

(define-public go-github-com-gorilla-websocket
  (package
    (name "go-github-com-gorilla-websocket")
    (version "1.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gorilla/websocket.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06smxc112xmixz78nyvk3b2hmc7wasf2sl5vxj1xz62kqcq9lzm9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/websocket"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "A fast, well-tested and widely used WebSocket implementation for Go")
    (description "Gorilla WebSocket is a Go implementation of the WebSocket
protocol. The Gorilla WebSocket package provides a complete and tested
implementation of the WebSocket protocol. The package API is stable.")
    (home-page "https://github.com/gorilla/websocket")
    (license license:expat)))


(define-public go-github-com-jmhodges-levigo
  (package
    (name "go-github-com-jmhodges-levigo")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jmhodges/levigo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06smxc112xmixz78nyvk3b2hmc7wasf2sl5vxj1xz62kqcq9lzm9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/websocket"
       ;; Source-only package
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (synopsis "levigo is a Go wrapper for LevelDB")
    (description "Gorilla WebSocket is a Go implementation of the WebSocket
protocol. The Gorilla WebSocket package provides a complete and tested
implementation of the WebSocket protocol. The package API is stable.")
    (home-page "https://github.com/gorilla/websocket")
    (license license:expat)))
