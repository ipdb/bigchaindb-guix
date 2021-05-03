(define-module (bigchaindb-guix services mongodb)
  #:use-module (bigchaindb-guix packages mongodb)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (guix build-system trivial)
  #:use-module (guix build union)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (mongodb-configuration
            mongodb-configuration?
            mongodb-configuration-mongodb
            mongodb-configuration-config-file
            mongodb-configuration-data-directory
            mongodb-service-type))

(define %default-mongodb-configuration-file
  (plain-file
   "mongodb.yaml"
   "# GNU Guix: MongoDB default configuration file
processManagement:
  pidFilePath: /var/run/mongodb/pid
storage:
  dbPath: /var/lib/mongodb
"))


(define-record-type* <mongodb-configuration>
  mongodb-configuration make-mongodb-configuration
  mongodb-configuration?
  (mongodb             mongodb-configuration-mongodb
                       (default mongodb))
  (config-file         mongodb-configuration-config-file
                       (default %default-mongodb-configuration-file))
  (data-directory      mongodb-configuration-data-directory
                       (default "/var/lib/mongodb")))

(define %mongodb-accounts
  (list (user-group (name "mongodb") (system? #t))
        (user-account
         (name "mongodb")
         (group "mongodb")
         (system? #t)
         (comment "Mongodb server user")
         (home-directory "/var/lib/mongodb")
         (shell (file-append shadow "/sbin/nologin")))))

(define mongodb-activation
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     #~(begin
         (use-modules (guix build utils))
         (let ((user (getpwnam "mongodb")))
           (for-each
            (lambda (directory)
              (mkdir-p directory)
              (chown directory
                     (passwd:uid user) (passwd:gid user)))
            '("/var/run/mongodb" #$data-directory)))))))

(define mongodb-shepherd-service
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     (shepherd-service
      (provision '(mongodb))
      (documentation "Run the Mongodb daemon.")
      (requirement '(user-processes loopback))
      (start #~(make-forkexec-constructor
                `(,(string-append #$mongodb "/bin/mongod")
                  "--config"
                  ,#$config-file)
                #:user "mongodb"
                #:group "mongodb"
                #:pid-file "/var/run/mongodb/pid"
                #:log-file "/var/log/mongodb.log"))
      (stop #~(make-kill-destructor))))))

(define mongodb-service-type
  (service-type
   (name 'mongodb)
   (description "Run the MongoDB document database server.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list
                                      mongodb-shepherd-service))
          (service-extension activation-service-type
                             mongodb-activation)
          (service-extension account-service-type
                             (const %mongodb-accounts))))
   (default-value
     (mongodb-configuration))))

;; (define select?
;;   (match-lambda
;;     (('guix  'config)   #f)
;;     (('guix  rest ...)  #t)
;;     (('gnu   rest ...)  #t)
;;     (_ #f)))


;; (with-imported-modules
;;     `(((guix config) => ,(make-config.scm))
;;       ,@(source-module-closure
;;          '((gnu packages base))
;;          #:select? (match-lambda
;;                      (('guix  'config)   #f)
;;                      (('guix  rest ...)  #t)
;;                      (('gnu   rest ...)  #t)
;;                      (_ #f))))
;;   #~(pk 'hello))


;; (with-store store
;;   (let* ((exp (with-extensions (list guile-gcrypt)
;;                 (with-imported-modules
;;                     `(((guix config) => ,(make-config.scm))
;;                       ,@(source-module-closure
;;                          '((gnu packages base)) #:select? (match-lambda
;;     (('guix  'config)   #f)
;;     (('guix  rest ...)  #t)
;;     (('gnu   rest ...)  #t)
;;     (_ #f))))
;;                   #~(pk 'hello))))
;;          (drv (run-with-store store
;;                 (mlet %store-monad ((drv (gexp->script "error" exp)))
;;                   (return drv)))))
;;     (build-derivations store (list drv))))
