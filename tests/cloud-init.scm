(define-module (tests cloud-init)
  #:use-module (bigchaindb-guix services cloud-init)
  #:use-module (gnu services base)
  #:use-module (guix utils)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (json)
  #:use-module (srfi srfi-64)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri))

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define %metadata-json-string
  (with-input-from-file (string-concatenate
                         `(,(current-source-directory) "/v1.json"))
    (lambda () (get-string-all (current-input-port)))))

(define %metadata-scm
  (json-string->scm %metadata-json-string))

(define (digitalocean-metadata-mock-handler request body)
  (if (equal? (request-path-components request) '("metadata" "v1.json"))
      (values '((content-type . (text/plain)))
              %metadata-json-string)
      (not-found request)))

(test-begin "cloud-init")

(test-equal
    "test"
  (let* ((query-metadata (@@ (bigchaindb-guix services cloud-init)
                             query-metadata))
         (local-server-uri (build-uri 'http
                                      #:port 8080
                                      #:host "localhost"
                                      #:path "/metadata/v1.json"))
         (mock-server-thread (call-with-new-thread
                              (lambda ()
                                (run-server digitalocean-metadata-mock-handler))))
         (result (query-metadata #:uri local-server-uri)))
    (cancel-thread mock-server-thread)
    result)
  %metadata-scm)

(test-equal
    "Test digitalocean metadata to static-networking-configuration
record conversion"
  (let ((conversion-function (@@ (bigchaindb-guix services cloud-init)
                                 metadata->static-networking-configuration)))
    (conversion-function 'digitalocean %metadata-scm))
  (list
   (static-networking (interface "eth0")
                      (ip "207.188.191.111")
                      (netmask "255.255.240.0")
                      (gateway "207.155.240.1")
                      (name-servers '("67.207.67.2" "67.207.67.3")))))

(test-end)
