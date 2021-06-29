;;; Copyright © 2020, 2021 Marius Bakke <marius.bakke@usit.uio.no>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public mreg
  (let ((commit "db020d38bb36ca9cc050d1a7fd2a5f67be92d379")
        (revision "3"))
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
                  "00r33yvx834a6xwl0j3cjl1wyfjqk84dprnvbbfa9gfnzr6ifrvc"))))
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
                      (lambda* (#:key tests? #:allow-other-keys)
                        (if tests?
                            (begin
                              ;; Pretend to be the upstream CI system to piggy back
                              ;; on the test project defined in settings.py ...
                              (setenv "CI" "1")
                              ;; ... but ignore the user and password setting.
                              (substitute* "mregsite/settings.py"
                                ((".*'(USER|PASSWORD)':.*")
                                 ""))
                              (invoke "python" "manage.py" "test" "-v2"))

                            (format #t "test suite not run~%")))))))
      (native-inputs
       `(("postgresql" ,postgresql)
         ("python-mock" ,python-mock)))
      (propagated-inputs
       `(("python-django" ,python-django)
         ("python-djangorestframework" ,python-djangorestframework)
         ("python-django-auth-ldap" ,python-django-auth-ldap)
         ("python-django-logging-json" ,python-django-logging-json)
         ("python-django-netfields" ,python-django-netfields)
         ("python-django-url-filter" ,python-django-url-filter)
         ("gunicorn" ,gunicorn)
         ("python-idna" ,python-idna)
         ("python-pika" ,python-pika)
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
    (version "0.9.10-14-gdc37efc")
    (home-page "https://github.com/unioslo/mreg-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0la8qj24m8rd4yfbjg8413998h07fv28r9hz06g8xhw692rcfzav"))))
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-prompt-toolkit@2" ,python-prompt-toolkit-2)
       ("python-requests" ,python-requests)))
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
