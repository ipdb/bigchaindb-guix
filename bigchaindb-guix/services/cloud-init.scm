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
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri))

(define %metadata-host "169.254.169.254")

(define %metadata-api-endpoint "/metadata/v1")

(define %metadata-top-level-index
  '("id"
    "hostname"
    "user-data"
    "vendor-data"
    "public-keys"
    "region"
    "interfaces/"
    "dns/"
    "floating_ip/"
    "tags/"))

(define %all-metadata-url-json
  (build-uri 'http
             #:host %metadata-host
             #:path "/metadata/v1.json"))

(define* (query-metadata #:key (uri %all-metadata-url-json))
  (let-values (((response body) (http-request uri
                                              #:method 'GET)))
    (if (eq? (response-code response) 200)
        (json-string->scm body)
        (throw 'metadata-query-error response))))

(define* (query-metadata #:key (uri %all-metadata-url-json))
  (let-values (((response body) (http-request uri
                                              #:method 'GET)))
    (if (eq? (response-code response) 200)
        (json-string->scm body)
        (throw 'metadata-query-error response))))

(define (resolve-metadata-quiery-method provider)
  (assoc-ref `((digitalocean . ,query-metadata)) provider))

(define-record-type* <cloud-init-configuration>
  cloud-inint-configuration make-cloud-init-configuration
  cloud-init-configuration?
  this-cloud-init-configuration
  (provider cloud-init-configuration-provider
            (default 'digitalocean))
  (metadata-quiery-method cloud-init-configuration-metadata-quiery-method
                          (thunked)
                          (default (resolve-metadata-quiery-method
                                    (cloud-init-configuration-provider
                                     this-cloud-init-configuration)))))

(define (metadata->static-networking-configuration provider metadata)
  (let* ((interfaces   (assoc-ref (assoc-ref metadata "interfaces") "public"))
         (name-servers (assoc-ref (assoc-ref metadata "dns") "nameservers"))
         (make-static-networking-conf
          (lambda (iface index)
            (let ((ipv4-field  (assoc-ref iface "ipv4")))
              (static-networking
               (interface    (string-concatenate
                              (list "eth" (number->string index))))
               (ip           (assoc-ref ipv4-field "ip_address"))
               (netmask      (assoc-ref ipv4-field "netmask"))
               (gateway      (assoc-ref ipv4-field "gateway"))
               (name-servers (vector->list name-servers)))))))
    (map (lambda (x) (apply make-static-networking-conf x))
         (zip (vector->list interfaces)
              (iota (vector-length interfaces))))))

