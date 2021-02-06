;;; Copyright © 2021 Marius Bakke <marius.bakke@usit.uio.no>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (uio services monitoring)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu system shadow)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (uio packages monitoring)
  #:use-module (ice-9 match)
  #:export (zabbix-auto-config-configuration
            zabbix-auto-config-configuration?
            zabbix-auto-config-configuration-package
            zabbix-auto-config-configuration-user
            zabbix-auto-config-configuration-group
            zabbix-auto-config-configuration-requirement
            zabbix-auto-config-configuration-directory
            zabbix-auto-config-service-type))

(define-record-type* <zabbix-auto-config-configuration>
  zabbix-auto-config-configuration
  make-zabbix-auto-config-configuration
  zabbix-auto-config-configuration?
  (package zabbix-auto-config-configuration-package
           (default zabbix-auto-config))
  (user zabbix-auto-config-configuration-user
        (default "zabbix-auto-config"))
  (group zabbix-auto-config-configuration-group
        (default "zabbix-auto-config"))
  (requirement zabbix-auto-config-configuration-requirement
               ;; Note: Add postgres if using a local database.
               (default '(networking)))
  (directory zabbix-auto-config-configuration-directory))

(define (zabbix-auto-config-accounts config)
  (let ((user (zabbix-auto-config-configuration-user config))
        (group (zabbix-auto-config-configuration-group config)))
    (list (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "zabbix-auto-config user")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin")))
          (user-group
           (name group)
           (system? #t)))))

(define (zac-postgresql-role config)
  (let ((user (zabbix-auto-config-configuration-user config)))
    (list (postgresql-role
           (name user)
           (create-database? #t))
          ;; XXX: For convenience, add a zabbix-server role too.
          (postgresql-role
           (name "zabbix")
           (create-database? #t)))))

(define zabbix-auto-config-shepherd-service
  (match-lambda
    (($ <zabbix-auto-config-configuration> package user group requirement directory)
     (list
      (shepherd-service
       (documentation "Start the zabbix-auto-config service")
       (provision '(zabbix-auto-config))
       (requirement requirement)
       (start #~(make-forkexec-constructor
                 (list #$(file-append package "/bin/zac"))
                 #:directory #$directory
                 #:user "zabbix-auto-config"
                 #:group "zabbix-auto-config"
                 #:log-file "/var/log/zabbix-auto-config.log"))
       (stop #~(make-kill-destructor)))))))

(define zabbix-auto-config-service-type
  (service-type
   (name 'zabbix-auto-config)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-auto-config-shepherd-service)
          (service-extension account-service-type
                             zabbix-auto-config-accounts)
          (service-extension postgresql-role-service-type
                             zac-postgresql-role)))
   (description
    "Run the zabbix-auto-config service, which collects information from
different sources and updates the Zabbix API accordingly.")))
