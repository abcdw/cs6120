(define-module (cs6120 packages)
  #:use-module (cs6120 channels)
  #:use-module (gnu packages)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gcc)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix channels)
  #:use-module (guix build-system channel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  #:use-module (nonguix build-system binary)

  #:export (guix-from-core-channels
            core-channels-package))

(define (get-guix-channel channels)
  (car
   (filter (lambda (x) (equal? (channel-name x) 'guix)) channels)))

(define-public guix-from-core-channels
  (let ((commit (channel-commit (get-guix-channel core-channels))))
    (package
      (inherit guix)
      (version (string-append
                (package-version guix) "-" (string-take commit 7)))
      (source
       (git-checkout
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit commit)))
      (arguments
       (substitute-keyword-arguments (package-arguments guix)
         ((#:tests? _)
          #f)
         ((#:phases phases)
          #~(modify-phases #$phases (delete 'check)))
         ((#:configure-flags flags #~'())
          #~(append
             #$flags
             (list
              #$(string-append "--with-channel-commit=" commit))))))

      (inputs (modify-inputs (package-inputs guix)
                (replace "guile" guile-next))))))

(define-public (guix-for-channels channels)
  "Return a package corresponding to CHANNELS."
  (package
   (inherit guix-from-core-channels)
   (source (find guix-channel? channels))
   (build-system channel-build-system)
   (arguments
    `(#:channels ,(remove guix-channel? channels)))
   (inputs '())
   (native-inputs '())
   (propagated-inputs '())))

(define-public core-channels-package
  (guix-for-channels core-channels))

(define-public deno
  (package
   (name "deno")
   (version "2.1.9")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/denoland/deno/releases/download/v"
                  version
                  "/deno-x86_64-unknown-linux-gnu.zip"))
            (sha256
             (base32
              "11kdmk3jk2rfsljg6rid6d65blcl51r6qpq1997h3ipmafy469g4"))))
   (build-system binary-build-system)
   (arguments
    (list
     #:install-plan #~'(("deno" "bin/"))
     #:patchelf-plan #~'(("deno" ("gcc:lib" "glibc")))))
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (supported-systems '("x86_64-linux"))
   (synopsis "A modern runtime for JavaScript and TypeScript.")
   (description
    "Deno is a JavaScript, TypeScript, and WebAssembly runtime with secure
defaults and a great developer experience. It's built on V8, Rust, and
Tokio.")
   (home-page "https://deno.com")
   (license license:gpl3+)))
