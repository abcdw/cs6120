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

(define (channel->git-checkout channel)
  (git-checkout
   (url (channel-url channel))
   (commit (channel-commit channel))))

(define* (channels-union name channels
                         #:key
                         (quiet? #f)
                         (resolve-collision 'resolve-collision/default))
  "Return a directory that is the union of CHANNELS sources."
  (define log-port
    (if quiet?
        (gexp (%make-void-port "w"))
        (gexp (current-error-port))))

  (computed-file
   name
   (with-imported-modules '((guix build union))
     (gexp
      (begin
        (use-modules (guix build union)
                     (srfi srfi-1)) ;for 'first' and 'last'

        (define (thing->srcs thing)
          (with-input-from-file (string-append thing "/.guix-channel")
            (lambda ()
              (let ((dirs (assoc-ref (cdr (read)) 'directory)))
                (if dirs
                    (map (lambda (x) (string-append thing "/" x)) dirs)
                    (list thing))))))

        (union-build (ungexp output)
                     (append-map thing->srcs '#$channels)

                     #:log-port (ungexp log-port)
                     #:symlink symlink
                     #:resolve-collision
                     (ungexp resolve-collision)))))))

(define (channels->combined-source-code channels)
  (channels-union
   "channels-sources"
   (map channel->git-checkout channels)))

(define-public (package-for-channels channels)
  (package
    (name "channels")
    (version "0.1.0")
    (source (channels->combined-source-code
             (remove guix-channel? channels)))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "."
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (mkdir "source")
              (chdir "source")
              (copy-recursively source "."
                                #:keep-mtime? #t
                                #:follow-symlinks? #t)
              (for-each (lambda (f)
                          (false-if-exception (make-file-writable f)))
                        (find-files ".")))))))
    (inputs `(("guile" ,guile-next)
              ("guix" ,guix-from-core-channels)))
    (home-page "https://git.sr.ht/~abcdw/rde")
    (synopsis "Combined package for channel source and bytecode files")
    (description "Combined package for channel source and bytecode files.")
    (license license:gpl3+)))

(define-public core-channels-package
  (package-for-channels core-channels))

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
