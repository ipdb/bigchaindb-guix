(define-module (tests fixtures)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (srfi srfi-13)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri)
  #:export (%metadata-scm
            %metadata-json-string
            run-digitalocean-metadata-server
            ;; store
            eval-in-store
            run-derivation->output-path
            ;; utils
            nested-assoc-ref))


;;; Metadata mock sever

(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define %metadata-json-string
  "{
  \"droplet_id\": 213129999,
  \"hostname\": \"test-test\",
  \"vendor_data\": \"Content-Type: loadsof/randomstuff\"
  \"public_keys\": [
    \"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCUnbO+g3Gc8zaxCLfpVegABaCyrKQwq3l7D\
asMqcVXdFFdDkaR26RfZkS13T0XQuxeM+2o7HGwFebkts56vWe/FLDMdNjAvViNo8hlX6bUaR+IPRf3\
KJSyy0aS7Ht7qtWHdDcuxtaDS8YebOw3GE9AG1P+nQMVRR12TesJnuq632gLdv/UlzIH32Qk7rMR3/9\
oDIbO24kFx9dTU/5Mdj2b+GQzfstux3uLMvlNxztRLTqFNSwQcc09vMAS0WZUkB4trT/WlkoDka/L46\
7k5gA0KjHtIprnP73oKlMBLu7NFq+UANUqncUzGbb99XbXQDFQYRiDdYCMX6wrW8mRL+Cz\"
  ],
  \"auth_key\": \"d873f96dbd087d05a4c5490379a925b5\",
  \"region\": \"fra1\",
  \"interfaces\": {
    \"private\": [
      {
        \"ipv4\": {
          \"ip_address\": \"10.114.0.2\",
          \"netmask\": \"255.255.240.0\",
          \"gateway\": \"0.0.0.0\"
        },
        \"mac\": \"de:83:28:c9:fe:92\",
        \"type\": \"private\"
      }
    ],
    \"public\": [
      {
        \"ipv4\": {
          \"ip_address\": \"207.188.191.111\",
          \"netmask\": \"255.255.240.0\",
          \"gateway\": \"207.155.240.1\"
        },
        \"ipv6\": {
          \"ip_address\": \"2848:F0F:5171:2F45:58B5:3A88:6062:AB3F\",
          \"CIDR\": 64,
          \"gateway\": \"2A03:B0C0:0003:00D0:0000:0000:0000:0001\"
        },
        \"anchor_ipv4\": {
          \"ip_address\": \"10.19.0.5\",
          \"netmask\": \"255.255.0.0\",
          \"gateway\": \"10.19.0.1\"
        },
        \"mac\": \"12:BE:48:43:D9:0D\",
        \"type\": \"public\"
      }
    ]
  },
  \"floating_ip\": {
    \"ipv4\": {
      \"active\": false
    }
  },
  \"dns\": {
    \"nameservers\": [
        \"67.207.67.2\",
        \"67.207.67.3\"
    ]
  },
  \"tags\": [
    \"some\",
    \"test\",
    \"tags\"
  ],
  \"features\": {
    \"dhcp_enabled\": false
  },
  \"modify_index\": 88992338
}")

(define %metadata-scm
  (json-string->scm %metadata-json-string))

(define (digitalocean-metadata-mock-handler request body)
  (if (equal? (request-path-components request) '("metadata" "v1.json"))
      (values '((content-type . (text/plain)))
              %metadata-json-string)
      (not-found request)))

(define-syntax-rule (run-digitalocean-metadata-server args ...)
  (run-server digitalocean-metadata-mock-handler
              'http
              (list args ...)))

;;; Working with store
(define (eval-in-store gexp)
  (with-store store
    (run-with-store store
      (mlet* %store-monad ((drv (gexp->file "eval" gexp))
                           (_   (built-derivations (list drv))))
        (return (primitive-load
                 (derivation->output-path drv)))))))

(define (run-derivation->output-path ->drv)
  (with-store store
    (run-with-store store
      (mlet* %store-monad ((drv ->drv)
                           (_ (built-derivations (list drv))))
        (return (derivation->output-path drv))))))

;;; Utilities
(define* (wait-for-port address port
                        #:key (timeout 20))
  "Wait for up to TIMEOUT seconds for ADDRESS:PORT to accept
connections. ADDRESS is either printable network address, integer, or
\"loopback\" Raise an error on failure."
  (let ((sock (socket PF_INET SOCK_STREAM 0))
        (addr (match address
                ("loopback"          INADDR_LOOPBACK)
                ((and (? string?) x) (inet-pton AF_INET x))
                ((and (? number?) x) x))))
    (let loop ((i 0))
      (catch 'system-error
        (lambda ()
          (connect sock AF_INET addr port)
          (close-port sock)
          'success)
        (lambda args
          (if (< i timeout)
              (begin
                (sleep 1)
                (loop (+ 1 i)))
              'failure))))))

(define (nested-assoc-ref alist key . rest)
  (let rec ((al alist)
            (kl (cons key rest)))
    (match kl
      (() al)
      (else (rec (assoc-ref al (car kl)) (cdr kl))))))
