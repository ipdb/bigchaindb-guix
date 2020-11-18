;; -*- mode: scheme; coding: utf-8; -*-
;; Copyright © 2019, 2020 David Dashyan <mail@davie.li>
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

;; ??? tendermint records are generated from config.toml with elisp this
;; results in more than a hundred getter functions being exported. May be it's
;; bit too much.
(define-module (bigchaindb-guix services bigchaindb)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix build utils)
  #:use-module (bigchaindb-guix packages bigchaindb)
  #:use-module (bigchaindb-guix packages tendermint)
  #:use-module (bigchaindb-guix deployment tendermint-init)
  #:use-module (ice-9 match)
  #:use-module (json)
  ;; NOTE clean unused start
  #:use-module (gcrypt pk-crypto)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt base16)
  #:use-module (srfi srfi-19) ;date
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 binary-ports)
  ;; end
  #:use-module (srfi srfi-28)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-1)
  #:use-module (guix modules)
  #:export (<tendermint-configuration>
            tendermint-configuration
            make-tendermint-configuration
            tendermint-configuration?
            tendermint-configuration-tendermint
            tendermint-configuration-log-directory
            tendermint-configuration-run-directory
            tendermint-configuration-main
            tendermint-configuration-rpc
            tendermint-configuration-p2p
            tendermint-configuration-mempool
            tendermint-configuration-consensus
            tendermint-configuration-tx-index
            tendermint-configuration-instrumentation
            tendermint-configuration-file

            <tm-main-config>
            tm-main-config
            make-tm-main-config
            tm-main-config?
            tm-main-config-tendermint
            tm-main-config-log-directory
            tm-main-config-run-directory
            tm-main-config-proxy-app
            tm-main-config-moniker
            tm-main-config-fast-sync
            tm-main-config-db-backend
            tm-main-config-db-dir
            tm-main-config-log-level
            tm-main-config-log-format
            tm-main-config-genesis-file
            tm-main-config-priv-validator-key-file
            tm-main-config-priv-validator-state-file
            tm-main-config-priv-validator-laddr
            tm-main-config-node-key-file
            tm-main-config-abci
            tm-main-config-prof-laddr
            tm-main-config-filter-peers
            tm-main-config-file

            <tm-rpc-config>
            tm-rpc-config
            make-tm-rpc-config
            tm-rpc-config?
            tm-rpc-config-laddr
            tm-rpc-config-cors-allowed-origins
            tm-rpc-config-cors-allowed-methods
            tm-rpc-config-cors-allowed-headers
            tm-rpc-config-grpc-laddr
            tm-rpc-config-grpc-max-open-connections
            tm-rpc-config-unsafe
            tm-rpc-config-max-open-connections
            tm-rpc-config-max-subscription-clients
            tm-rpc-config-max-subscriptions-per-client
            tm-rpc-config-timeout-broadcast-tx-commit
            tm-rpc-config-tls-cert-file
            tm-rpc-config-tls-key-file

            <tm-p2p-config>
            tm-p2p-config
            make-tm-p2p-config
            tm-p2p-config?
            tm-p2p-config-laddr
            tm-p2p-config-external-address
            tm-p2p-config-seeds
            tm-p2p-config-persistent-peers
            tm-p2p-config-upnp
            tm-p2p-config-addr-book-file
            tm-p2p-config-addr-book-strict
            tm-p2p-config-max-num-inbound-peers
            tm-p2p-config-max-num-outbound-peers
            tm-p2p-config-flush-throttle-timeout
            tm-p2p-config-max-packet-msg-payload-size
            tm-p2p-config-send-rate
            tm-p2p-config-recv-rate
            tm-p2p-config-pex
            tm-p2p-config-seed-mode
            tm-p2p-config-private-peer-ids
            tm-p2p-config-allow-duplicate-ip
            tm-p2p-config-handshake-timeout
            tm-p2p-config-dial-timeout

            <tm-mempool-config>
            tm-mempool-config
            make-tm-mempool-config
            tm-mempool-config?
            tm-mempool-config-recheck
            tm-mempool-config-broadcast
            tm-mempool-config-wal-dir
            tm-mempool-config-size
            tm-mempool-config-max-txs-bytes
            tm-mempool-config-cache-size

            <tm-consensus-config>
            tm-consensus-config
            make-tm-consensus-config
            tm-consensus-config?
            tm-consensus-config-wal-file
            tm-consensus-config-timeout-propose
            tm-consensus-config-timeout-propose-delta
            tm-consensus-config-timeout-prevote
            tm-consensus-config-timeout-prevote-delta
            tm-consensus-config-timeout-precommit
            tm-consensus-config-timeout-precommit-delta
            tm-consensus-config-timeout-commit
            tm-consensus-config-skip-timeout-commit
            tm-consensus-config-create-empty-blocks
            tm-consensus-config-create-empty-blocks-interval
            tm-consensus-config-peer-gossip-sleep-duration
            tm-consensus-config-peer-query-maj23-sleep-duration

            <tm-tx-index-config>
            tm-tx-index-config
            make-tm-tx-index-config
            tm-tx-index-config?
            tm-tx-index-config-indexer
            tm-tx-index-config-index-tags
            tm-tx-index-config-index-all-tags
            tm-tx-index-config-prometheus
            tm-tx-index-config-prometheus-listen-addr
            tm-tx-index-config-max-open-connections
            tm-tx-index-config-namespace

            <tm-instrumentation-config>
            tm-instrumentation-config-prometheus
            tm-instrumentation-config
            make-tm-instrumentation-config
            tm-instrumentation-config?
            tm-instrumentation-config-prometheus-listen-addr
            tm-instrumentation-config-max-open-connections
            tm-instrumentation-config-namespace
            
            tendermint-service-type

            emit-tendermint-config-toml
            emit-tendermint-config-alist

            ;; Bigchaindb
            bigchaindb-service-type
            bigchaindb-configuration

            ;; FIXME these are exported for testing only
            generate-tm-keypair
            generate-tm-node-key-json
            tm-keypair?))

;;; Tendermint

;; convig.toml records
(define-record-type* <tendermint-configuration>
  tendermint-configuration make-tendermint-configuration
  tendermint-configuration?
  (tendermint                tendermint-configuration-tendermint ;; <package>
                             (default tendermint-bin))
  ;; (home-directory            tendermint-configuration-home-directory ;; string
  ;;                            (default "/var/lib/tendermint"))
  (log-directory             tendermint-configuration-log-directory ;; string
                             (default "/var/log/tendermint"))
  (run-directory             tendermint-configuration-run-directory ;; string
                             (default "/var/run/tendermint"))
  (tm-main-config            tendermint-configuration-main
                             (default (tm-main-config)))
  (tm-rpc-config             tendermint-configuration-rpc
                             (default (tm-rpc-config)))
  (tm-p2p-config             tendermint-configuration-p2p
                             (default (tm-p2p-config)))
  (tm-mempool-config         tendermint-configuration-mempool
                             (default (tm-mempool-config)))
  (tm-consensus-config       tendermint-configuration-consensus
                             (default (tm-consensus-config)))
  (tm-tx-index-config        tendermint-configuration-tx-index
                             (default (tm-tx-index-config)))
  (tm-instrumentation-config tendermint-configuration-instrumentation
                             (default (tm-instrumentation-config)))
  (file-provided             tendermint-configuration-file-provided
                             ;; #f | string
                             (default #f)))

(define-record-type* <tm-main-config>
  tm-main-config make-tm-main-config
  tm-main-config?
  (proxy-app                 tm-main-config-proxy-app ;; string
                             (default "tcp://127.0.0.1:26658"))
  (moniker                   tm-main-config-moniker
                             ;; NOTE Can we set it in runtime?
                             (default "tm-node"))
  (fast-sync                 tm-main-config-fast-sync ;; boolean
                             (default #t))
  (db-backend                tm-main-config-db-backend
                             (default 'leveldb)) ;; leveldb | memdb | cleveldb
  (db-dir                    tm-main-config-db-dir
                             (default "data"))
  (log-level                 tm-main-config-log-level
                             (default "main:info,state:info,*:error"))
  (log-format                tm-main-config-log-format ;; plain | json
                             (default 'plain))
  (genesis-file              tm-main-config-genesis-file
                             (default "config/genesis.json"))
  (priv-validator-key-file   tm-main-config-priv-validator-key-file
                             (default "config/priv_validator_key.json"))
  (priv-validator-state-file tm-main-config-priv-validator-state-file
                             (default "data/priv_validator_state.json"))
  (priv-validator-laddr      tm-main-config-priv-validator-laddr
                             (default ""))
  (node-key-file             tm-main-config-node-key-file
                             (default "config/node_key.json"))
  (abci                      tm-main-config-abci ;; socket | grpc
                             (default "socket"))
  (prof-laddr                tm-main-config-prof-laddr
                             (default ""))
  (filter-peers              tm-main-config-filter-peers ;; boolean
                             (default #f)))

(define-record-type* <tm-rpc-config>
  tm-rpc-config make-tm-rpc-config
  tm-rpc-config?
  (laddr                        tm-rpc-config-laddr
                                (default "tcp://0.0.0.0:26657"))
  (cors-allowed-origins         tm-rpc-config-cors-allowed-origins
                                (default '()))
  (cors-allowed-methods         tm-rpc-config-cors-allowed-methods
                                (default '("HEAD" "GET" "POST")))
  (cors-allowed-headers         tm-rpc-config-cors-allowed-headers
                                (default '("Origin"
                                           "Accept"
                                           "Content-Type"
                                           "X-Requested-With"
                                           "X-Server-Time")))
  (grpc-laddr                   tm-rpc-config-grpc-laddr
                                (default ""))
  (grpc-max-open-connections    tm-rpc-config-grpc-max-open-connections
                                (default 900))
  (unsafe                       tm-rpc-config-unsafe
                                (default #f))
  (max-open-connections         tm-rpc-config-max-open-connections
                                (default 900))
  (max-subscription-clients     tm-rpc-config-max-subscription-clients
                                (default 100))
  (max-subscriptions-per-client tm-rpc-config-max-subscriptions-per-client
                                (default 5))
  (timeout-broadcast-tx-commit  tm-rpc-config-timeout-broadcast-tx-commit
                                (default "10s"))
  (tls-cert-file                tm-rpc-config-tls-cert-file
                                (default ""))
  (tls-key-file                 tm-rpc-config-tls-key-file
                                (default "")))


(define-record-type* <tm-p2p-config>
  tm-p2p-config make-tm-p2p-config
  tm-p2p-config?
  (laddr                       tm-p2p-config-laddr
                               (default "tcp://0.0.0.0:26656"))
  (external-address            tm-p2p-config-external-address
                               (default ""))
  (seeds                       tm-p2p-config-seeds
                               (default ""))
  (persistent-peers            tm-p2p-config-persistent-peers
                               (default ""))
  (upnp                        tm-p2p-config-upnp
                               (default #f))
  (addr-book-file              tm-p2p-config-addr-book-file
                               (default "config/addrbook.json"))
  (addr-book-strict            tm-p2p-config-addr-book-strict
                               (default #t))
  (max-num-inbound-peers       tm-p2p-config-max-num-inbound-peers
                               (default 40))
  (max-num-outbound-peers      tm-p2p-config-max-num-outbound-peers
                               (default 10))
  (flush-throttle-timeout      tm-p2p-config-flush-throttle-timeout
                               (default "100ms"))
  (max-packet-msg-payload-size tm-p2p-config-max-packet-msg-payload-size
                               (default 1024))
  (send-rate                   tm-p2p-config-send-rate
                               (default 5120000))
  (recv-rate                   tm-p2p-config-recv-rate
                               (default 5120000))
  (pex                         tm-p2p-config-pex
                               (default #t))
  (seed-mode                   tm-p2p-config-seed-mode
                               (default #f))
  (private-peer-ids            tm-p2p-config-private-peer-ids
                               (default ""))
  (allow-duplicate-ip          tm-p2p-config-allow-duplicate-ip
                               (default #f))
  (handshake-timeout           tm-p2p-config-handshake-timeout
                               (default "20s"))
  (dial-timeout                tm-p2p-config-dial-timeout
                               (default "3s")))

(define-record-type* <tm-mempool-config>
  tm-mempool-config make-tm-mempool-config
  tm-mempool-config?
  (recheck       tm-mempool-config-recheck
                 (default #t))
  (broadcast     tm-mempool-config-broadcast
                 (default #t))
  (wal-dir       tm-mempool-config-wal-dir
                 (default ""))
  (size          tm-mempool-config-size
                 (default 5000))
  (max-txs-bytes tm-mempool-config-max-txs-bytes
                 (default 1073741824))
  (cache-size    tm-mempool-config-cache-size
                 (default 10000)))

(define-record-type* <tm-consensus-config>
  tm-consensus-config make-tm-consensus-config
  tm-consensus-config?
  (wal-file                     tm-consensus-config-wal-file
                                (default "data/cs.wal/wal"))
  (timeout-propose              tm-consensus-config-timeout-propose
                                (default "3s"))
  (timeout-propose-delta        tm-consensus-config-timeout-propose-delta
                                (default "500ms"))
  (timeout-prevote              tm-consensus-config-timeout-prevote
                                (default "1s"))
  (timeout-prevote-delta        tm-consensus-config-timeout-prevote-delta
                                (default "500ms"))
  (timeout-precommit            tm-consensus-config-timeout-precommit
                                (default "1s"))
  (timeout-precommit-delta      tm-consensus-config-timeout-precommit-delta
                                (default "500ms"))
  (timeout-commit               tm-consensus-config-timeout-commit
                                (default "1s"))
  (skip-timeout-commit          tm-consensus-config-skip-timeout-commit
                                (default #f))
  (create-empty-blocks          tm-consensus-config-create-empty-blocks
                                (default #t))
  (create-empty-blocks-interval tm-consensus-config-create-empty-blocks-interval
                                (default "0s"))
  (peer-gossip-sleep-duration   tm-consensus-config-peer-gossip-sleep-duration
                                (default "100ms"))
  (peer-query-maj23-sleep-duration
   tm-consensus-config-peer-query-maj23-sleep-duration
   (default "2s")))


(define-record-type* <tm-tx-index-config>
  tm-tx-index-config make-tm-tx-index-config
  tm-tx-index-config?
  (indexer        tm-tx-index-config-indexer
                  (default "kv"))
  (index-tags     tm-tx-index-config-index-tags
                  (default ""))
  (index-all-tags tm-tx-index-config-index-all-tags
                  (default #f)))

(define-record-type* <tm-instrumentation-config>
  tm-instrumentation-config make-tm-instrumentation-config
  tm-instrumentation-config?
  (prometheus             tm-instrumentation-config-prometheus
                          (default #f))
  (prometheus-listen-addr tm-instrumentation-config-prometheus-listen-addr
                          (default ":26660"))
  (max-open-connections   tm-instrumentation-config-max-open-connections
                          (default 3))
  (namespace              tm-instrumentation-config-namespace
                          (default "tendermint")))

(define emit-tendermint-config-alist
  (let ()
    (define (symbol->underscorized-string sym)
      (let ((str (string-copy (symbol->string sym))))
        (string-for-each-index
         (lambda (i)
           (string-set! str i (let ((c (string-ref str i)))
                                (if (char=? c  #\-) #\_ c))))
         str)
        str))
    (match-lambda
      (($ <tendermint-configuration>
          tendermint log-directory run-directory main rpc p2p
          mempool consensus tx-index instrumentation file-provided)
       (map-in-order
        (match-lambda
          ((section config-type config)
           (let* ((fields (record-type-fields config-type))
                  (accessors (map-in-order (cut record-accessor config-type <>)
                                           fields)))
             `(,section
               ,(map-in-order
                 cons
                 (map-in-order symbol->underscorized-string fields)
                 (map-in-order (cut apply <> `(,config)) accessors))))))

        `((""                  ,<tm-main-config>            ,main)
          ("[rpc]"             ,<tm-rpc-config>             ,rpc)
          ("[p2p]"             ,<tm-p2p-config>             ,p2p)
          ("[mempool]"         ,<tm-mempool-config>         ,mempool)
          ("[consensus]"       ,<tm-consensus-config>       ,consensus)
          ("[tx_index]"        ,<tm-tx-index-config>        ,tx-index)
          ("[instrumentation]" ,<tm-instrumentation-config> ,instrumentation)))))))

(define (emit-tendermint-config-toml-lines tendermint-config-alist)

  (define (list-of-strings->toml-array-of-strings lst)
    (format "[~a]"
            (match lst
              ('() "")
              (_ (fold (lambda (x y)
                         (string-append (format "~s" x) ", " y))
                       (format "~s" (car lst))
                       (cdr lst))))))

  (define (format-list key value)
    (format "~a = ~a" key (list-of-strings->toml-array-of-strings value)))

  (define (value-dispatch pair)
    ((match-lambda
       ((key . (? string?  value))
        (format "~a = ~s" key value))
       ((key . (? symbol? value))
        (format "~a = ~s" key (symbol->string value)))
       ((key . (? boolean? value))
        (format "~a = ~s" key (if value 'true 'false)))
       ((key . (? number? value))
        (format "~a = ~s" key value))
       ((key . ((? string? value) ...))
        (format-list key value)))
     pair))

  (match tendermint-config-alist
    ('() '())
    (((section keyvalpair) tail ...)
     (append
      '()
      `(,section
        ,@(map-in-order value-dispatch keyvalpair)
        ,@(emit-tendermint-config-toml-lines tail))))))

(define (emit-tendermint-config-toml tendermint-config)
  (with-output-to-string
    (lambda ()
      (for-each
       (lambda (line) (display line) (newline))
       (emit-tendermint-config-toml-lines
        (emit-tendermint-config-alist tendermint-config))))))


;; Generating genesis.json

(define-record-type <tm-keypair>
  (make-tm-keypair private public)
  tm-keypair?
  (private tm-keypair-private)
  (public tm-keypair-public))

;; FIXME Doesnt work see tendermint-init module workarround
(define (generate-tm-keypair-gcrypt)
  "Returns <tm-keypair>. Which is basically a ed25519 keypair with fields
encoded with base64, but private keys have public key as suffix \"to make
multiple signing operations with the same key more efficient\"."
  (define (extract-token-value sexp token)
    (cadr (canonical-sexp->sexp (find-sexp-token sexp token))))

  (define (concatenate-bytevectors bytevectors-list)
    (call-with-values open-bytevector-output-port
      (lambda (port get-bytevector)
        (for-each (lambda (x) (put-bytevector port x)) bytevectors-list)
        (get-bytevector))))

  (let* ((parameters (sexp->canonical-sexp
                      '(genkey (ecdsa (curve Ed25519) (flags rfc6979)))))
         (gcrypt-pair (generate-key parameters))
         (private (extract-token-value gcrypt-pair 'd)) ; 'd as in private
         (public (extract-token-value gcrypt-pair 'q))) ; 'q as in public

    (apply make-tm-keypair
           (map base64-encode
                (list (concatenate-bytevectors (list private public))
                      public)))))

(define (generate-tm-keypair)
  (define (concatenate-bytevectors bytevectors-list)
    (call-with-values open-bytevector-output-port
      (lambda (port get-bytevector)
        (for-each (lambda (x) (put-bytevector port x)) bytevectors-list)
        (get-bytevector))))

  (match-let ((('tendermint-ed25519-key ('pubkey private) ('privkey public))
               (tendermint-ed25519-genkey)))
    (apply make-tm-keypair
           (map base64-encode (list private public)))))

(define (generate-tm-node-key-json pair)
  "Make tendermint node_key.json configuration"
  (scm->json
   `((priv_key . ((type . "tendermint/PrivKeyEd25519")
                  (value . ,(tm-keypair-private pair)))))))

(define (generate-tm-genesis-json pair)
  "Generate tendermint genesis.json configuration"

  (define (tm-pubkey->validator-address pubkey)
    "Convert <tm-keypair> to validator address. Which is a upcased
    base16-encoded first 20 bytes of public key bytevecor"
    (string-upcase
     (bytevector->base16-string
      (get-bytevector-n (open-bytevector-input-port
                         (sha256 (base64-decode pubkey)))
                        20))))

  (scm->json
   `((genesis_time . ,(date->string (current-date) "~5.~NZ"))
     (chain_id . ,(string-append "test-chain-"
                                 (number->string (random (expt 2 24)) 16)))
     (consensus_params . ((block . ((max_bytes . "22020096")
                                    (max_gas . "-1")
                                    (time_iota_ms . "1000")))
                          (evidence . ((max_age . "100000")))
                          (validator . ((pub_key_types . #("ed25519"))))))
     (validators . #(((address . ,(tm-pubkey->validator-address
                                   (tm-keypair-public pair)))
                      (pub_key . ((value . ,(tm-keypair-public pair))
                                  (type . "tendermint/PubKeyEd25519")))
                      (power . "10")
                      (name . ""))))
     (app_hash . ""))
   #:pretty #t))



;; Tendermint service
(define %tendermint-accounts
  (list (user-group (name "tendermint") (system? #t))
        (user-account
         (name "tendermint")
         (group "tendermint")
         (system? #t)
         (comment "Tendermint user")
         (home-directory "/var/lib/tendermint")
         (shell (file-append shadow "/sbin/nologin")))))

(define (tendermint-activation tendermint-config)
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "tendermint"))
            (tendermint-exec (string-append #$tendermint-bin "/bin/tendermint"))
            (conf-toml "/var/lib/tendermint/config/config.toml")
            (file-provided #$(tendermint-configuration-file-provided
                              tendermint-config)))
        ;; Initializing tendermint
        (invoke tendermint-exec "init" "--home" "/var/lib/tendermint/")
        ;; Replace config.toml
        (with-output-to-file conf-toml
          (lambda ()
            (if (string? file-provided)
                (display file-provided)
                (display #$(emit-tendermint-config-toml tendermint-config)))))
        ;; Chown directories
        (for-each
         (lambda (directory)
           (mkdir-p directory)
           (chown directory (passwd:uid user) (passwd:gid user)))
         (find-files "/var/lib/tendermint" #:directories? #t)))))

(define (tendermint-shepherd-service tendermint-config)
  (list
   (shepherd-service
    (provision '(tendermint))
    (documentation "Run the Tendermint service.")
    (requirement '(user-processes loopback))
    (start #~(begin
               ;; ??? If we exec tendermint without daemonizing and pid file,
               ;; will shepherd be aware of its termination?
               (make-forkexec-constructor
                `(,(string-append
                    #$(tendermint-configuration-tendermint tendermint-config)
                    "/bin/tendermint")
                  "node"
                  "--home"
                  "/var/lib/tendermint"
                  "--proxy_app=kvstore")
                #:user "tendermint"
                #:group "tendermint"
                #:directory "/var/lib/tendermint"
                #:log-file "/var/log/tendermint.log"
                #:environment-variables '("HOME=/var/lib/tendermint"
                                          ;; TMPATH probably not
                                          ;; available in early versions
                                          "TMPATH=/var/lib/tendermint"))))
    (stop #~(make-kill-destructor)))))

(define tendermint-service-type
  (service-type
   (name 'tendermint)
   (description "Run the Tendermint server.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             tendermint-shepherd-service)
          (service-extension activation-service-type
                             tendermint-activation)
          (service-extension account-service-type
                             (const %tendermint-accounts))))
   (default-value (tendermint-configuration))))


;; BigchaiDB
(define %default-bigchaindb-configuration
  '((server . ((bind . "0.0.0.0:9984")
               (workers . #nil)
               (loglevel . "info")))
    (wsserver . ((scheme . "ws")
                 (advertised_port . 9985)
                 (advertised_host . "localhost")
                 (advertised_scheme . "ws")
                 (port . 9985)
                 (host . "localhost")))
    (tendermint . ((port . 26657)
                   (host . "localhost")))
    (database . ((backend . "localmongodb")
                 (password . #nil)
                 (login . #nil)
                 (replicaset . #nil)
                 (name . "bigchain")
                 (port . 27017)
                 (host . "localhost")
                 (crlfile . #nil)
                 (keyfile_passphrase . #nil)
                 (keyfile . #nil)
                 (certfile . #nil)
                 (ca_cert . #nil)
                 (ssl . #f)
                 (max_tries . 3)
                 (connection_timeout . 5000)))
    (log . ((file . "/var/log/bigchaindb/bigchaindb.log")
            (granular_levels . ())
            (fmt_logfile . "[%(asctime)s] [%(levelname)s] (%(name)s) \
%(message)s (%(processName)-10s - pid: %(process)d)")
            (fmt_console . "[%(asctime)s] [%(levelname)s] (%(name)s) \
%(message)s (%(processName)-10s - pid: %(process)d)")
            (datefmt_logfile . "%Y-%m-%d %H:%M:%S")
            (datefmt_console . "%Y-%m-%d %H:%M:%S")
            (level_logfile . "info")
            (level_console . "info")
            (error_file . "/var/log/bigchaindb/bigchaindb-errors.log")))
    (CONFIGURED . #t)))

(define (bigchaindb-configuration-alist->json bdb-conf-alist)
  (with-output-to-string
    (lambda ()
      (scm->json bdb-conf-alist #:pretty #t))))

(define-record-type* <bigchaindb-configuration>
  bigchaindb-configuration make-bigchaindb-configuration
  bigchaindb-configuration?
  (bigchaindb    bigchaindb-configuration-bigchaindb ;; <package>
                 (default bigchaindb))
  (alist         bigchaindb-configuration-alist
                 (default %default-bigchaindb-configuration))
  (log-directory bigchaindb-configuration-log-directory ;; string
                 (default "/var/log/bigchaindb")))

(define %bigchaindb-accounts
  (list (user-group (name "bigchaindb") (system? #t))
        (user-account
         (name "bigchaindb")
         (group "bigchaindb")
         (system? #t)
         (comment "BigchainDB user")
         (home-directory "/var/lib/bigchaindb")
         (shell (file-append shadow "/sbin/nologin")))))

(define (bigchaindb-activation bigchaindb-config)
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "bigchaindb"))
            (conf-json "/var/lib/bigchaindb/.bigchaindb"))
        ;; Place bigchaindb config
        (with-output-to-file conf-json
          (lambda ()
            (display #$(bigchaindb-configuration-alist->json
                        (bigchaindb-configuration-alist bigchaindb-config)))))
        ;; Chown directories
        (for-each
         (lambda (directory)
           (mkdir-p directory)
           (chown directory (passwd:uid user) (passwd:gid user)))
         `("/var/log/bigchaindb"
           ,@(find-files "/var/lib/bigchaindb" #:directories? #t))))))

(define (bigchaindb-shepherd-service bigchaindb-config)
  (list
   (shepherd-service
    (provision '(bigchaindb))
    (documentation "Run the BigchainDB service.")
    (requirement '(user-processes loopback mongodb tendermint))
    (start #~(begin
               ;; ??? If we exec bigchaindb without daemonizing and pid file,
               ;; will shepherd be aware of its termination?
               (make-forkexec-constructor
                `(,(string-append
                    #$(bigchaindb-configuration-bigchaindb bigchaindb-config)
                    "/bin/bigchaindb")
                  "start")
                #:user "bigchaindb"
                #:group "bigchaindb"
                #:directory "/var/lib/bigchaindb"
                #:log-file "/var/log/bigchaindb-sytem.log"
                #:environment-variables '("HOME=/var/lib/bigchaindb"))))
    (stop #~(make-kill-destructor)))))

(define bigchaindb-service-type
  (service-type
   (name 'bigchaindb)
   (description "Run the BigchainDB server.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             bigchaindb-shepherd-service)
          (service-extension activation-service-type
                             bigchaindb-activation)
          (service-extension account-service-type
                             (const %bigchaindb-accounts))))
   (default-value (bigchaindb-configuration))))

