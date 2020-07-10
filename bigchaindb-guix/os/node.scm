;; -*- mode: scheme; coding: utf-8; -*-
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

(define-module (bigchaindb-guix os node)
  #:use-module (bigchaindb-guix os services)
  #:use-module (bigchaindb-guix packages bigchaindb)
  #:use-module (bigchaindb-guix packages tendermint)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu)
  #:use-module (guix build utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (bigchaindb-node))

;; BigchainDB node operating-system declaration.
(define bigchaindb-node
  (operating-system
    (host-name "BigchainDB Node")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    ;; Boot in "legacy" BIOS mode, assuming /dev/sdX is the
    ;; target hard disk, and "my-root" is the label of the target
    ;; root file system.
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vda")))
    (file-systems (cons (file-system
                          (mount-point "/")
                          (device "/dev/vda1")
                          (type "ext4"))
                        %base-file-systems))

    ;; No users other than root and those defined by services
    (users %base-user-accounts)

    ;; Globally-installed packages.
    (packages (append (list screen curl nss-certs) %base-packages))

    (services
     (append
      (list (service dhcp-client-service-type)
            (service openssh-service-type
                     (openssh-configuration
                      (port-number 2222)))
            (service mongodb-service-type)
            (service tendermint-service-type
                     (tendermint-configuration
                      (tm-main-config
                       (tm-main-config
                        (proxy-app "tcp://0.0.0.0:26658")))
                      (tm-p2p-config
                       (tm-p2p-config
                        (laddr "tcp://0.0.0.0:26656")
                        (pex #f)))
                      (tm-consensus-config
                       (tm-consensus-config
                        (create-empty-blocks #f)))))
            (service bigchaindb-service-type))
      %base-services))))
