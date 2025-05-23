(define-module (cs6120 packages)
  #:use-module (cs6120 channels)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix channels)
  #:use-module (guix build-system channel)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1)
  #:use-module (nonguix build-system binary)
  #:use-module (rde packages guix)

  #:export (guix-from-core-channels
            core-channels-package))

(define guix-from-core-channels
  (make-guix-package core-channels))

(define core-channels-package
  (make-channels-package core-channels))

;; ((@ (rde api store) build-with-store) core-channels-package)

(define-public bril
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/sampsyo/bril")
          (commit "ac9bbaa26cd342d7bae075d9d9b7b6591a4155a2")))
    (file-name (git-file-name "bril" "0.1.0"))
    (sha256
     (base32 "1gp2nyqiwbcgbx8z5ybprvmrsz7lfjpm4hbbzhsffhqv71rs6c7c"))))

(define-public turnt
  (package
   (name "turnt")
   (version "1.12.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/cucapra/turnt")
                  (commit (string-append "v" version))))
            (file-name (git-file-name "turnt" version))
            (sha256
             (base32 "18b8771w8x8pdyh6zsm8v0105gj3vvyl3yc3k9psaf63svdqcv3r"))))
   (arguments (list #:tests? #f))
   (build-system pyproject-build-system)
   (native-inputs (list python-flit-core))
   (inputs (list python-click))
   (synopsis "Simple snapshot-style integration testing for commands")
   (description
    "Turnt is a simple snapshot testing tool inspired by Cram and
 LLVM's lit. It's good for testing things that translate text files to
 other text files, like compilers. The idea is that each test is one
 input file, and you want to run a command and check that it still
 matches the saved output file.")
   (home-page "https://github.com/cucapra/turnt")
   (license license:expat)))

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
   (native-inputs (list unzip))
   (inputs `(("gcc:lib" ,gcc "lib")
             ("glibc" ,glibc)))
   (supported-systems '("x86_64-linux"))
   (synopsis "A modern runtime for JavaScript and TypeScript")
   (description
    "Deno is a JavaScript, TypeScript, and WebAssembly runtime with secure
defaults and a great developer experience. It's built on V8, Rust, and
Tokio.")
   (home-page "https://deno.com")
   (license license:gpl3+)))

(define-public brili
  (package
   (name "brili")
   (version "0.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/sampsyo/bril")
                  (commit "ac9bbaa26cd342d7bae075d9d9b7b6591a4155a2")))
            (file-name (git-file-name "bril" version))
            (sha256
             (base32 "1gp2nyqiwbcgbx8z5ybprvmrsz7lfjpm4hbbzhsffhqv71rs6c7c"))))
   (build-system trivial-build-system)
   (arguments
    (list
     #:modules '((guix build utils))
     #:builder
     #~(begin
         (use-modules (guix build utils))
         (define (wrap-ts-file source target)
           (let* ((bin-dir (string-append #$output "/bin"))
                  (bin-file (string-append bin-dir "/" target)))
             (mkdir-p bin-dir)
             (call-with-output-file bin-file
               (lambda (port)
                 (format
                  port "#!/bin/sh
exec ~a run --allow-read --allow-env --no-config \\
'file://~a' \"$@\""
                  (search-input-file %build-inputs "bin/deno")
                  (search-input-file %build-inputs source))))
             (chmod bin-file #o544)))
         (wrap-ts-file "brili.ts" "brili"))))
   (inputs (list deno))
   (synopsis "Bril interpreter")
   (description "Deno-based Bril intrepreter written in TypeScript.")
   (home-page "https://github.com/sampsyo/bril")
   (license license:expat)))

(define-public bril-txt
  (package
   (inherit brili)
   (name "bril-txt")
   (version "0.1.0")
   (build-system pyproject-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
        (add-after 'unpack 'go-to-bril-txt
         (lambda _
           (chdir "bril-txt"))))
     #:tests? #f))
   (inputs (list python-lark-parser))
   (native-inputs (list python-flit-core python-setuptools python-wheel))
   (synopsis "Bril to Json and json to bril tools")
   (description "Python ipmlementation of bril2json and json2bril.")
   (home-page "https://github.com/sampsyo/bril")
   (license license:expat)))


;; ((@ (rde api store) build-with-store) bril-txt)
