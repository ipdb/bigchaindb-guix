;; -*- mode: scheme; coding: utf-8; -*-
;; Copyright © 2021 David Dashyan <mail@davie.li>
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

;; Comment: This module provides cloud-init replacement for Guix system.  It
;; contains services that can be added to custom cloud images.  They will read
;; data provided by cloud metadata services and configure system accordingly.
;; NOTE Experimental and only works with digitalocean at the moment.

(define-module (bigchaindb-guix services cloud-init)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (cloud-init-service-type
            cloud-init-config
            cloud-init-config?
            cloud-init-config-metadata-host
            cloud-init-config-metadata-port
            make-cloud-init-config))

(define %metadata-host
  (make-parameter "169.254.169.254"))

(define %metadata-port
  (make-parameter 80))

(define %metadata-path
  (make-parameter "/metadata/v1.json"))

(define-record-type* <cloud-init-config>
  cloud-init-config make-cloud-init-config
  cloud-init-config?
  (metadata-host cloud-init-config-metadata-host
                 (default (%metadata-host)))
  (metadata-path cloud-init-config-metadata-path
                 (default (%metadata-path)))
  (log-file      cloud-init-config-file
                 (default "/var/log/cloud-init.log"))
  (requirements  cloud-init-config-requirements
                 (default '()))) ;; + loopback

(define* (query-metadata)
  (let ((select? (match-lambda
                   (('json rest ...)  #t)
                   (_ #f))))
    (with-extensions (list guile-json-4)
      (with-imported-modules (source-module-closure '((json)) #:select? select?)
        #~(begin
            (use-modules (json)
                         (ice-9 match)
                         (web client)
                         (web request)
                         (web response)
                         (web uri))
            (let* ((xhost #$(%metadata-host))
                   (xpath #$(%metadata-path))
                   (xport #$(%metadata-port))
                   (uri (build-uri
                         'http #:host xhost #:path xpath #:port xport))
                   (addr (cond
                          ((equal? xhost "localhost") INADDR_LOOPBACK)
                          ((string? xhost) (inet-pton AF_INET xhost))
                          (else (throw 'invalid-host))))
                   (sock (socket PF_INET SOCK_STREAM 0))
                   (port-available? (lambda ()
                                      (let loop ((i 0))
                                        (catch 'system-error
                                          (lambda ()
                                            (connect sock AF_INET addr xport)
                                            (close-port sock)
                                            #t)
                                          (lambda _
                                            (if (< i 20)
                                                (begin
                                                  (sleep 1)
                                                  (loop (+ 1 i)))
                                                #f)))))))
              (call-with-values
                  (lambda ()
                    (if (port-available?)
                        (http-request uri #:method 'GET)
                        (throw 'port-unavailable)))
                (lambda (response body)
                  (if
                   (eq? (response-code response) 200)
                   (json-string->scm body)
                   (throw 'metadata-query-error response))))))))))

(define hwaddress-inerface-name-alist-gexp
  ;; XXX: Linux specific
  #~(begin
      (use-modules (ice-9 match))
      (map
       (lambda (name)
         ;; Make (mac-address . interface-name) pair
         (call-with-input-file
             (string-append "/sys/class/net/" name "/address")
           (lambda (file) (cons (string-trim-both (get-string-all file))
                                name))))
       ;; interface names list
       (let ((dir-stream (opendir "/sys/class/net")))
         (let rec ((iface-names '())
                   (iname (readdir dir-stream)))
           (match iname
             ((? eof-object?) (begin (closedir dir-stream) iface-names))
             ((or "." "..")   (rec iface-names (readdir dir-stream)))
             (else            (rec (cons iname iface-names)
                                   (readdir dir-stream)))))))))

(define (make-cloud-init-gexp)
  (with-imported-modules '((guix build syscalls))
    #~(begin
        (use-modules (guix build syscalls)
                     (ice-9 match)
                     (ice-9 textual-ports)
                     (guix build utils)
                     (srfi srfi-43))
        (let-syntax ((ref (syntax-rules ()
                            ;; nested assoc-ref for working with json alists
                            ;; (ref '((a . ((b . hello!)))) 'a 'b) => 'hello!
                            ((_ alist key key* ...)
                             (let rec ((al alist) (kl (list key key* ...)))
                               (match kl
                                 (() al)
                                 (_  (rec (assoc-ref al (car kl))
                                          (cdr kl)))))))))
          ;; Metadata vars
          (let* ((metadata           #$(query-metadata))
                 (dhcp-enabled       (ref metadata "features" "dhcp_enabled"))
                 (public-keys        (ref metadata "public_keys"))
                 (public-interfaces  (ref metadata "interfaces" "public"))
                 (private-interfaces (ref metadata "interfaces" "private"))
                 (hwaddr-name-alist  #$hwaddress-inerface-name-alist-gexp))
            ;; Configuring network
            (for-each
             (lambda (iface-info)
               (let* ((addr-str    (ref iface-info "ipv4" "ip_address"))
                      (addr        (inet-pton AF_INET addr-str))
                      (sockaddr    (make-socket-address AF_INET addr 0))
                      (mask-str    (ref iface-info "ipv4" "netmask"))
                      (mask        (inet-pton AF_INET mask-str))
                      (maskaddr    (make-socket-address AF_INET mask 0))
                      (gateway-str (ref iface-info "ipv4" "gateway"))
                      (gateway     (inet-pton AF_INET gateway-str))
                      (gatewayaddr (make-socket-address AF_INET gateway 0))
                      (iface-name  (assoc-ref hwaddr-name-alist
                                              (string-downcase
                                               (ref iface-info "mac")))))
                 (configure-network-interface iface-name sockaddr
                                              ;; ignoring loopback here
                                              (logior IFF_UP 0) ;; ???
                                              #:netmask maskaddr)
                 (when gateway
                   (let ((sock (socket AF_INET SOCK_DGRAM 0)))
                     (add-network-route/gateway sock gatewayaddr)
                     (close-port sock)))))
             (vector->list public-interfaces))
            ;; Inserting ssh keys
            (mkdir-p "/root/.ssh")
            (call-with-output-file "/root/.ssh/authorized_keys"
              (lambda (f)
                (vector-for-each (lambda (key) (display key f) (newline f))
                                 (pk 2 (ref metadata "public_keys"))))))))))

(define (with-gexp-logger file xgexp)
  #~(with-output-to-file #$file
      (lambda ()
        (with-exception-handler
            (lambda (err)
              (display ";;; Exiting with error:\n")
              (display err)
              (newline)
              (force-output))
          (lambda ()
            (display ";;; Log start\n")
            #$xgexp)))))

(define (cloud-init-shepherd-service config)
  (match-record config <cloud-init-config>
    (metadata-host metadata-path log-file requirements)
    (shepherd-service
     (provision '(cloud-init))
     (documentation "Configure system")
     (requirement `(user-processes loopback ,@requirements))
     (start #~(lambda _
                #$(with-gexp-logger
                   log-file
                   (parameterize ((%metadata-host metadata-host)
                                  (%metadata-path metadata-path))
                     (make-cloud-init-gexp)))))
     (stop #~(lambda _ #t))
     (respawn? #f)
     (modules `((json)
                (ice-9 match)
                (guix build syscalls)
                (web client)
                (web request)
                (web response)
                (web uri)
                ,@%default-modules)))))

(define cloud-init-service-type
  (service-type
   (name 'cloud-init)
   (description "")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list cloud-init-shepherd-service))))
   (default-value (cloud-init-config))))
