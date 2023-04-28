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

(define-module (uio packages mreg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public mreg
  (let ((commit "f7ad8c5185bd84f08d3b56a2a1030a52367d3c2b")
        (revision "6"))
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
                  "06ih2x43iqws5cp6wcjsvpg1g12wkda2pfm3i2wq717pkpmjlyks"))))
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
                        (invoke "pg_ctl" "-D" "/tmp/db"
                                "-o" "-k /tmp" ;socket directory
                                "-l" "db.log"
                                "start")

                        (invoke "psql" "-h" "/tmp"
                                "-c" "CREATE EXTENSION citext;"
                                "template1")))
                    (replace 'check
                      (lambda* (#:key tests? #:allow-other-keys)
                        (if tests?
                            (begin
                              (setenv "MREG_DB_USER" "")
                              (setenv "MREG_DB_HOST" "/tmp")
                              (invoke "python" "manage.py" "test" "-v2"))

                            (format #t "test suite not run~%")))))))
      (native-inputs
       (list postgresql))
      (propagated-inputs
       (list python-django
             python-django-rest-framework
             python-django-auth-ldap
             python-django-logging-json
             python-django-netfields
             python-django-url-filter
             python-idna
             python-pika
             python-psycopg2

             ;; Ensure 'tzdata' is available for users of mreg such
             ;; that timezone functionality works in Django 4.0+.
             tzdata

             ;; For OpenAPI support.
             python-uritemplate
             python-pyyaml))
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
    (version "0.9.10-40-g42e77f9")
    (home-page "https://github.com/unioslo/mreg-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vq9r7wgxiwhgndlikczy4w4mqlf09506fcxyhc2brzjk5ymadcb"))))
    (inputs
     (list python-dateutil
           python-prompt-toolkit
           python-requests
           python-typing-extensions))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? outputs #:allow-other-keys)
                      (if tests?
                          (let* ((out (assoc-ref outputs "out"))
                                 (mreg-cli (string-append out "/bin/mreg-cli")))
                            (invoke mreg-cli "--playback" "testsuite-result.json"
                                    "-d" "example.org"))

                          (format #t "test suite not run~%")))))))
    (synopsis "Command-line interface for mreg")
    (description
     "@command{mreg-cli} is a command-line tool for working with the
@command{mreg} inventory system.")
    (license gpl3+)))
