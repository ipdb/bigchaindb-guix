(define-module (tests system)
  #:use-module (bigchaindb-guix services cloud-init)
  #:use-module (gnu build marionette)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix self)
  #:use-module (guix derivations)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (json)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-43)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:use-module (tests fixtures)
  #:export (%test-cloud-init))


;;;
;;; Cloud init system test
;;;

(define select?
  (match-lambda
    (('guix 'config)             #f)
    (('guix rest ...)            #t)
    (('gnu 'herd rest ...)       #t)
    (('tests rest ...)           #t)
    (('bigchaindb-guix rest ...) #t)
    (_ #f)))

(define metadata-server-script
  (program-file
   "start-metadata-server"
   (with-extensions
       (list guile-json-4 guile-gcrypt)
     (with-imported-modules `(((guix config) => ,(make-config.scm))
                              (web uri)
                              (web request)
                              ,@(source-module-closure
                                 '((tests fixtures))
                                 #:select? select?))
       #~(begin
           (use-modules (tests fixtures)
                        (web server))
           (run-digitalocean-metadata-server #:port 80))))))

(define %cloud-init-metadata-server-os
  (operating-system
    (inherit
     (simple-operating-system
      ;; simple service that spawns up http metadata server on localhost
      (simple-service
       'digitalocean-metadata-server
       shepherd-root-service-type
       (list (shepherd-service
              (provision '(metadata-server))
              (start     #~(make-forkexec-constructor
                            (list #$metadata-server-script)))
              (stop      #~(make-kill-destructor))
              (respawn?  #f))))
      (service cloud-init-service-type
               (cloud-init-config
                (metadata-host "localhost")
                (requirements '(metadata-server))))))
    (packages (append (list curl) %base-packages))
    (kernel-arguments (cons "net.ifnames=0" %default-kernel-arguments))))

(define (run-cloud-init-test)
  (define os
    (marionette-operating-system
     %cloud-init-metadata-server-os
     #:imported-modules '((gnu services herd))
     #:requirements '(metadata-server)))

  (define vm-script
    (run-derivation->output-path
     (system-qemu-image/shared-store-script
      os
      #:options (vector->list
                 (vector-map
                  (lambda (_ i)
                    (string-append "-nic user,model=e1000,mac="
                                   (assoc-ref i "mac")))
                  (nested-assoc-ref %metadata-scm "interfaces" "public"))))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (web request)
                             (web response)
                             (web server)
                             (web uri))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (ice-9 textual-ports)
                       (srfi srfi-64)
                       (web request)
                       (web response)
                       (web server)
                       (web uri))

          (define marionette
            (make-marionette (list #$vm-script)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "cloud-init")

          (test-assert "Waiting for metadat mock server"
            (wait-for-tcp-port 80 marionette))

          (test-assert "Running cloud-init service"
            (marionette-eval
             '(begin (use-modules (gnu services herd))
                     (start-service 'cloud-init))
             marionette))

          (test-assert "get text"
            (marionette-screen-text marionette
                                    #:ocrad
                                    #$(file-append ocrad "/bin/ocrad")))


          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "cloud-init-test" test))

(define %test-cloud-init
  (system-test
   (name "cloud-init")
   (description "Test a running guile-cloud-init configuration.")
   (value (run-cloud-init-test))))
