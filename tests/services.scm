;;; guix.scm -- Guix package definition built from local source-tree
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

(define-module (tests services)
  #:use-module (bigchaindb-guix services bigchaindb)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt pk-crypto)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64))

;; Test services

(define %default-tendermint-config
  "
proxy_app = \"tcp://127.0.0.1:26658\"
moniker = \"tm-node\"
fast_sync = true
db_backend = \"leveldb\"
db_dir = \"data\"
log_level = \"main:info,state:info,*:error\"
log_format = \"plain\"
genesis_file = \"config/genesis.json\"
priv_validator_key_file = \"config/priv_validator_key.json\"
priv_validator_state_file = \"data/priv_validator_state.json\"
priv_validator_laddr = \"\"
node_key_file = \"config/node_key.json\"
abci = \"socket\"
prof_laddr = \"\"
filter_peers = false
[rpc]
laddr = \"tcp://0.0.0.0:26657\"
cors_allowed_origins = []
cors_allowed_methods = [\"POST\", \"GET\", \"HEAD\"]
cors_allowed_headers = [\"X-Server-Time\", \"X-Requested-With\", \"Content-Type\", \"Accept\", \"Origin\"]
grpc_laddr = \"\"
grpc_max_open_connections = 900
unsafe = false
max_open_connections = 900
max_subscription_clients = 100
max_subscriptions_per_client = 5
timeout_broadcast_tx_commit = \"10s\"
tls_cert_file = \"\"
tls_key_file = \"\"
[p2p]
laddr = \"tcp://0.0.0.0:26656\"
external_address = \"\"
seeds = \"\"
persistent_peers = \"\"
upnp = false
addr_book_file = \"config/addrbook.json\"
addr_book_strict = true
max_num_inbound_peers = 40
max_num_outbound_peers = 10
flush_throttle_timeout = \"100ms\"
max_packet_msg_payload_size = 1024
send_rate = 5120000
recv_rate = 5120000
pex = true
seed_mode = false
private_peer_ids = \"\"
allow_duplicate_ip = false
handshake_timeout = \"20s\"
dial_timeout = \"3s\"
[mempool]
recheck = true
broadcast = true
wal_dir = \"\"
size = 5000
max_txs_bytes = 1073741824
cache_size = 10000
[consensus]
wal_file = \"data/cs.wal/wal\"
timeout_propose = \"3s\"
timeout_propose_delta = \"500ms\"
timeout_prevote = \"1s\"
timeout_prevote_delta = \"500ms\"
timeout_precommit = \"1s\"
timeout_precommit_delta = \"500ms\"
timeout_commit = \"1s\"
skip_timeout_commit = false
create_empty_blocks = true
create_empty_blocks_interval = \"0s\"
peer_gossip_sleep_duration = \"100ms\"
peer_query_maj23_sleep_duration = \"2s\"
[tx_index]
indexer = \"kv\"
index_tags = \"\"
index_all_tags = false
[instrumentation]
prometheus = false
prometheus_listen_addr = \":26660\"
max_open_connections = 3
namespace = \"tendermint\"
")

(define (go-style-privkey-bytevector? privkey)
  ;; FIXME fix libgcrypt <-> go ed25519 capability
  (and
   (equal? (bytevector-length privkey) 64)
   (let* ((bvport (open-bytevector-input-port privkey))
          (priv-part (get-bytevector-n bvport 32))
          (pub-part  (get-bytevector-n bvport 33))
          (gcrypt-privkey-cannonical-sexp
           (sexp->canonical-sexp
            `(private-key
              (ecc (curve Ed25519)
                   (flags eddsa)
                   (q ,pub-part)
                   (d ,priv-part)))))
          (gcrypt-pubkey-canonical-sexp
           (sexp->canonical-sexp
            `(public-key
              (ecc (curve Ed25519)
                   (flags eddsa)
                   (q ,pub-part)))))
          (data (bytevector->hash-data
                 (sha256 (string->utf8 "some message"))))
          (signature (sign data gcrypt-privkey-cannonical-sexp)))

     (verify signature data gcrypt-pubkey-canonical-sexp))))

(test-begin "services")

(test-equal
    "Test emit-tendermint-config-toml - tendermint default config generation"
  %default-tendermint-config
  (emit-tendermint-config-toml (tendermint-configuration)))

(test-assert
    "Test generate-tm-keypir"
  (tm-keypair? (generate-tm-keypair)))

(test-assert
    "Test generate-tm-node-key"
  (match (json-string->scm
          (with-output-to-string
            (lambda () (generate-tm-node-key-json (generate-tm-keypair)))))
    ((("priv_key"
       ("value" . (? (lambda (x)
                       ;; (go-style-privkey-bytevector? (base64-decode x)) FIXME
                       (bytevector? (base64-decode x)))))
       ("type" . "tendermint/PrivKeyEd25519"))) #t)))

(test-end)
