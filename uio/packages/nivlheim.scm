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

(define-module (uio packages nivlheim)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module ((guix licenses) #:select (gpl3+ expat))
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages web))

;; TODO: Upstream these dependencies.

(define-public go-github-com-azure-go-ntlmssp
  (let ((commit "66371956d46c8e2133a2b72b3d320e435465011f")
        (revision "0"))
    (package
      (name "go-github-com-azure-go-ntlmssp")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/Azure/go-ntlmssp")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0c2mj6xv0k93h9vys492vc50j260k7vwb5xnhj34x1d27nah2h69"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/Azure/go-ntlmssp"))
      (propagated-inputs
       `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
      (synopsis "NTLM/Negotiate authentication over HTTP")
      (description
       "go-github-com-azure-go-ntlmssp is a Go package.")
      (license expat))))

(define-public go-github-com-go-ldap-ldap
  (package
    (name "go-github-com-go-ldap-ldap")
    (version "3.2.4")
    (home-page "https://github.com/go-ldap/ldap")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "184ps1h3kh98a69s73v5ypxc0qy1dm7n1a78is0qg4i7kwfrfhhg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-ldap/ldap"
       ;; XXX: Test suite attempts to connect to a remote server.
       #:tests? #f))
    (inputs
     `(("go-github-com-azure-go-ntlmssp" ,go-github-com-azure-go-ntlmssp)
       ("go-github-com-go-asn1-ber-asn1-ber" ,go-github-com-go-asn1-ber-asn1-ber)))
    (synopsis "LDAP v3 implementation for Go")
    (description
     "This package provides basic LDAP v3 functionality for the Go programming
language.")
    (license expat)))

(define-public go-gopkg.in-ldap.v3
  (package
    (name "go-gopkg.in-ldap.v3")
    (version "3.2.4")
    (home-page "https://github.com/go-ldap/ldap")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "184ps1h3kh98a69s73v5ypxc0qy1dm7n1a78is0qg4i7kwfrfhhg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/ldap.v3"
       ;; XXX: Test suite attempts to connect to a remote server.
       #:tests? #f))
    (inputs
     `(("go-github-com-azure-go-ntlmssp" ,go-github-com-azure-go-ntlmssp)
       ("go-github-com-go-asn1-ber-asn1-ber" ,go-github-com-go-asn1-ber-asn1-ber)))
    (synopsis "LDAP v3 implementation for Go")
    (description
     "This package provides basic LDAP v3 functionality for the Go programming
language.")
    (license expat)))

(define-public go-github-com-go-asn1-ber-asn1-ber
  (package
    (name "go-github-com-go-asn1-ber-asn1-ber")
    (version "1.5.1")
    (home-page "https://github.com/go-asn1-ber/asn1-ber")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qaa1y99l6dh7bnvq7xyapwcrk8m24d3si3vpx8wg0rxv2np4v7l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-asn1-ber/asn1-ber"))
    (synopsis "ASN1 BER encoding/decoding library")
    (description
     "This package provides an ASN1 BER encoding / decoding Library for the
Go programming language.")
    (license expat)))

(define-public nivlheim
  (package
    (name "nivlheim")
    (version "2.7.10")
    (home-page "https://github.com/unioslo/nivlheim")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gi0iv4f0jkfsshay3h5yjw9fpyq8gapq95q38va3rxxw83gcd4j"))))
    (build-system go-build-system)
    (outputs '("out" "client"))
    (arguments
     (list #:install-source? #f
           #:unpack-path "github.com/unioslo/nivlheim"
           #:import-path "github.com/unioslo/nivlheim/server/service"
           #:build-flags #~(list (string-append "-ldflags=-X main.version="
                                                #$(package-version this-package)))
           #:phases
           #~(modify-phases %standard-phases
	     (add-after 'unpack 'patch-test
	       (lambda _
	         (substitute*
		     "src/github.com/unioslo/nivlheim/server/service/testingUtilities.go"
	           ;; Connect to Postgres using TCP instead of a Unix socket.
	           (("host=/var/run/postgresql")
		    "host=127.0.0.1 port=5432"))))
	     (add-before 'check 'prepare-tests
	       (lambda _
	         (mkdir-p "/tmp/db")
	         (invoke "initdb" "-D" "/tmp/db")
	         (invoke "pg_ctl" "-D" "/tmp/db"
                         "-o" "-k /tmp"
                         "-l" "db.log" "start")
	         (invoke "psql" "-h" "/tmp" "-d" "postgres"
                         "-c" "CREATE DATABASE nixbld;")

	         ;; Disable tests that require network access.
	         (setenv "NONETWORK" "indeed")))
	     (add-after 'install 'rename-server
	       (lambda* (#:key outputs #:allow-other-keys)
	         (let ((out (assoc-ref outputs "out")))
	           (with-directory-excursion (string-append out "/bin")
		     (rename-file "service" "nivlheim")))))
             (add-after 'install 'install-client
               (lambda* (#:key outputs unpack-path #:allow-other-keys)
                 (let ((client (assoc-ref outputs "client"))
                       (version #$(package-version this-package)))
	           (with-directory-excursion (string-append "src/" unpack-path)
                     (substitute* "client/nivlheim_client"
                       (("VERSION = '0\\.0\\.0'")
                        (string-append "VERSION = '" version "'")))
                     (install-file "client/nivlheim_client"
                                   (string-append client "/bin"))
                     (wrap-program (string-append client "/bin/nivlheim_client")
                       `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))))))))
    (native-search-paths
     ;; Automatically configure these variables when a CA bundle is available.
     (list $SSL_CERT_DIR $SSL_CERT_FILE))
    (native-inputs
     `(("postgresql" ,postgresql)))
    (inputs
     `(("github.com/Azure/go-ntlmssp" ,go-github-com-azure-go-ntlmssp)
       ("github.com/lib/pq" ,go-github-com-lib-pq)
       ("github.com/go-asn1-ber/asn1-ber" ,go-github-com-go-asn1-ber-asn1-ber)
       ("golang.org/x/net" ,go-golang-org-x-net)
       ("golang.org/x/oauth2" ,go-golang-org-x-oauth2)
       ("gopkg.in/ldap.v3" ,go-gopkg.in-ldap.v3)

       ;; For the client.
       ("perl" ,perl)
       ("perl-http-message" ,perl-http-message)
       ("perl-io-socket-inet6" ,perl-io-socket-inet6)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-net-dns" ,perl-net-dns)
       ("perl-yaml-libyaml" ,perl-yaml-libyaml)))
    (synopsis "Collect information from servers")
    (description
     "Nivlheim is a system for collecting key information from remote
machines and presenting it through an easy-to-use web GUI with search and
browse functions.")
    (license gpl3+)))
