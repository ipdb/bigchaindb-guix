(use-modules (bigchaindb-guix packages bigchaindb)
             (bigchaindb-guix packages tendermint)
             (gnu packages commencement)
             (gnu packages guile)
             (gnu packages make-bootstrap)
             (gnu packages python-xyz)
             (gnu packages)
             (gnu system install)
             (gnu system vm)
             (gnu system)
             (guix config)
             (guix derivations)
             (guix grafts)
             (guix monads)
             ((guix licenses)
              #:select (gpl3+ license-name license-uri license-comment))
             (guix packages)
             ((guix scripts system) #:select (read-operating-system))
             (guix store)
             ((guix utils) #:select (%current-system))
             (ice-9 match)
             (srfi srfi-1))

(define (license->alist lcs)
  "Return LCS <license> object as an alist."
  ;; Sometimes 'license' field is a list of licenses.
  (if (list? lcs)
      (map license->alist lcs)
      `((name . ,(license-name lcs))
        (uri . ,(license-uri lcs))
        (comment . ,(license-comment lcs)))))

(define (package-metadata package)
  "Convert PACKAGE to an alist suitable for Hydra."
  `((#:description . ,(package-synopsis package))
    (#:long-description . ,(package-description package))
    (#:license . ,(license->alist (package-license package)))
    (#:home-page . ,(package-home-page package))
    (#:maintainers . ("mail@davie.li"))
    (#:max-silent-time . ,(or (assoc-ref (package-properties package)
                                         'max-silent-time)
                              19))      ;1 hour by default
    (#:timeout . ,(or (assoc-ref (package-properties package) 'timeout)
                      72000))))           ;20 hours by default

(define (package-job store job-name package system)
  "Return a job called JOB-NAME that builds PACKAGE on SYSTEM."
  (lambda ()
    `((#:job-name . ,job-name)
      (#:derivation . ,(derivation-file-name
                        (parameterize ((%graft? #f))
                          (package-derivation store package system
                                              #:graft? #f))))
      ,@(package-metadata package))))

(define %jobs-module
  (current-module))

(define (cuirass-jobs store arguments)
  (map
   (lambda (package-to-build)
     (let ((name (assq-ref package-to-build #:package-name))
           (system (or (assq-ref package-to-build #:system) "x86_64-linux")))
       (package-job store
                    (string-append (symbol->string name) "-package")
                    (module-ref %jobs-module name)
                    system)))
   (last arguments)))
