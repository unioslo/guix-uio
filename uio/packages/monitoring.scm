;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius.bakke@usit.uio.no>
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

(define-module (uio packages monitoring)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public zabbix-auto-config
  (let ((commit "92f6458481ac3a17d8553c1076263d550fbb781a")
        (revision "2"))
    (package
      (name "zabbix-auto-config")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/unioslo/zabbix-auto-config")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fsa0licvg6ji6sq1qfca5splbr6j228hpmcrf7fc2zg0smw7v28"))))
      (build-system python-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda* (#:key tests? #:allow-other-keys)
                        (when tests?
                          (invoke "python" "-m" "unittest" "discover"
                                  "-v" "tests")))))))
      (inputs
       (list python-multiprocessing-logging python-pydantic python-psycopg2
             python-pyzabbix python-requests))
      (synopsis "Automate Zabbix configuration")
      (description
       "zabbix-auto-config (@command{zac}) is a tool to collect information
from different sources and update the Zabbix API accordingly.")
      (license license:expat))))
