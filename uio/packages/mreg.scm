;;; Copyright © 2020 Marius Bakke <marius.bakke@usit.uio.no>
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

(define-module (uio packages mreg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public mreg
  (let ((commit "a8306c740be05c419c7abdf84cf13fd67161b9c0")
        (revision "1"))
    (package
      (name "mreg")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/unioslo/mreg")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vpndgkqn3drgd5j6sda6szwrsd2x1s3w01y4mypl9d3yk8flhhh"))))
      (build-system python-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    ;; No setup.py, so install manually.
                    (delete 'build)
                    (replace 'install
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (python (assoc-ref inputs "python"))
                               (site-packages (string-append out "/lib/python"
                                                             (python-version python)
                                                             "/site-packages")))
                          (copy-recursively "."
                                            (string-append site-packages "/mreg"))
                          #t)))
                    (add-before 'check 'start-postgresql
                      (lambda _
                        (mkdir-p "/tmp/db")
                        (invoke "initdb" "-D" "/tmp/db")
                        (invoke "pg_ctl" "-D" "/tmp/db" "-l" "/tmp/db.log" "start")

                        (invoke "psql" "-c" "CREATE EXTENSION citext;" "template1")
                        (invoke "psql" "-d" "postgres" "-c"
                                "CREATE DATABASE travisci;")))
                    (replace 'check
                      (lambda _
                        ;; Pretend to be the Travis CI system to piggy back on
                        ;; the test project defined in settings.py ...
                        (setenv "TRAVIS" "1")
                        ;; ... but ignore the user and port setting.
                        (substitute* "mregsite/settings.py"
                          ((".*'(USER|PORT)':.*")
                           ""))

                        (invoke "python" "manage.py" "test"))))))
      (native-inputs
       `(("postgresql" ,postgresql-11)))
      (propagated-inputs
       `(("python-django" ,python-django)
         ("python-djangorestframework" ,python-djangorestframework)
         ("python-django-auth-ldap" ,python-django-auth-ldap)
         ("python-django-logging-json" ,python-django-logging-json)
         ("python-django-netfields" ,python-django-netfields)
         ("python-django-url-filter" ,python-django-url-filter)
         ("gunicorn" ,gunicorn)
         ("python-idna" ,python-idna)
         ("python-psycopg2" ,python-psycopg2)))
      (synopsis "Machine inventory system")
      (description
       "@command{mreg} is a RESTful API for managing DNS zones, networks,
and servers.  Information about networks and devices are added through the
API, and the server can export DNS zone files and DHCP information for use
with servers.  Authentication using LDAP is supported, and permissions can
be delegated so that groups can manage only the networks they own.")
      (license gpl3+))))

(define-public mreg-cli
  (package
    (name "mreg-cli")
    (version "0.9.10")
    (home-page "https://github.com/unioslo/mreg-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fb24qzh9j0a12kmwndv632314qqciz6ly8ws0awz3a2y7wvaixf"))))
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-prompt-toolkit@2" ,python-prompt-toolkit-2)
       ("python-requests" ,python-requests)))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (mreg-cli (string-append out "/bin/mreg-cli")))
                        (invoke mreg-cli "--playback" "testsuite-result.json"
                                "-d" "example.org")))))))
    (synopsis "Command-line interface for mreg")
    (description
     "@command{mreg-cli} is a command-line tool for working with the
@command{mreg} inventory system.")
    (license gpl3+)))
