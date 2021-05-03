(define-module (tests cloud-init)
  #:use-module (tests fixtures)
  #:use-module (bigchaindb-guix services cloud-init)
  #:use-module (gnu services base)
  #:use-module (gnu tests)
  #:use-module (guix utils)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri))


;; Tests
(test-begin "cloud-init")

(test-equal
    "test"
  (let* ((query-metadata      (@@ (bigchaindb-guix services cloud-init)
                                  query-metadata))
         (host-parameter      (@@ (bigchaindb-guix services cloud-init)
                                  %metadata-host))
         (port-parameter      (@@ (bigchaindb-guix services cloud-init)
                                  %metadata-port))
         (mock-server-thread  (call-with-new-thread
                               (lambda ()
                                 (run-digitalocean-metadata-server
                                  #:port 8080))))
         (query-metadata-gexp (parameterize ((host-parameter "localhost")
                                             (port-parameter 8080))
                                (query-metadata)))
         (result              (eval-in-store query-metadata-gexp)))
    (cancel-thread mock-server-thread)
    result)
  %metadata-scm)

(test-end)
