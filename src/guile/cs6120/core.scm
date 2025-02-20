(define-module (cs6120 core)
  #:use-module (json)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43))

(define (function-instructions fun)
    (assoc-ref fun "instrs"))

(define (print-instructions vec)
    (vector-for-each (lambda (i x) (format #t "~s\n" x)) vec))

(define (instruction-op instr)
  (assoc-ref instr "op"))

(define (print-blocks blocks)
  (for-each (lambda (x) (format #t "~s\n" x)) blocks))

(define terminators '("jmp" "br" "ret"))

(define (label? instruction)
  (assoc-ref instruction "label"))

(define (instructions->blocks instructions)
  (define fold-result
    (vector-fold
     (lambda (_ acc x)
       (match acc
         ((current-block blocks block-id label mapping)
          (let ((new-block (cons x current-block))
                (next-block-id (1+ block-id)))
            ;; (pk label mapping block-id)
            (if (or (member (instruction-op x) terminators)
                    (label? x))
                (list '() (cons (cons block-id (reverse new-block)) blocks)
                      next-block-id
                      (if (label? x) (assoc-ref x "label") #f)
                      (if (label? x)
                          (acons (assoc-ref x "label") next-block-id mapping)
                          mapping))
                (list new-block blocks block-id label mapping))))))
     '(() () 0 #f ())
     instructions))
  (list
   (reverse
    (match fold-result
      ((block blocks block-id _ _)
       (if (null? block)
           blocks
           (cons (cons block-id (reverse block)) blocks)))))
   (last fold-result)))


(define (blocks->control-flow-graph blocks)
  (car
   (match blocks
     ((blocks mapping)
      (fold
       (lambda (x acc)
         (match acc
           ((edges came-from)
            (match x
              ((id . instructions)
               (let* ((instr (last instructions))
                      (op (assoc-ref instr "op"))
                      (labels (and=> (assoc-ref instr "labels") vector->list))
                      (jump? (and (list? labels) (not (null? labels)))))
                 ;; TODO: [Andrew Tropin, 2025-02-04] Check if it is a jump
                 ;; instruction.  Right now we rely on the assumption that
                 ;; any instruction with labels field is a jump instruction.
                 (list
                  (append
                   edges
                   (if jump?
                       (map (lambda (x) (cons id (assoc-ref mapping x))) labels)
                       (if came-from (list (cons came-from id)) '())))
                  (if jump? #f id))))))))
       '(() #f)
       blocks)))))

(define bril-program
  (call-with-input-file
      "/data/abcdw/work/sampsyo/bril/tmp/jmp.json"
    (lambda (port)
      (json->scm port))))

(define (print-bril-blocks bril-program)
  (vector-map
   (lambda (i x)
     (format #t "=============\n")
     ((@ (ice-9 pretty-print) pretty-print)
      (instructions->blocks (function-instructions x)))
     (blocks->control-flow-graph
      (instructions->blocks (function-instructions x))))
   (assoc-ref bril-program "functions"))
  "hi")

(define (function->blocks bril-function)
  (instructions->blocks (function-instructions bril-function)))

(define (get-blocks bril-program)
  (vector-map
   (lambda (i x) (function->blocks x))
   (assoc-ref bril-program "functions")))

(define (blocks->instrs blocks)
  (list->vector
   (append-map cdr (car blocks))))

(define* (alist-update alist key f #:optional (dflt '()))
  (let* ((prev-pair (assoc key alist))
         (prev (if prev-pair (cdr prev-pair) dflt)))
    (acons
     key
     (f prev)
     (alist-delete key alist equal?))))

(define (transform-program-instrs bril-program instrs-transformation)
  (define (update-functions fns)
    (vector-map
     (lambda (i x)
       (alist-update x "instrs" instrs-transformation))
     fns))
  (alist-update bril-program "functions" update-functions))

(define (update-blocks-instrs blocks function)
  (cons
   (map
    (lambda (block)
      (cons (car block) (function (cdr block))))
    (car blocks))
   (cdr blocks)))

(define (remove-unused-variables block-instrs)
  "Just removes the variables that never used for sure."
  (define used-variables
    (fold
     (lambda (x acc)
       (lset-union
        equal?
        acc
        (vector->list (or (assoc-ref x "args") #()))))
     '()
     block-instrs))

  ;; (format (current-error-port) "used-variables: ~a\n" used-variables)
  (fold-right
   (lambda (x acc)
     (if (and (assoc "dest" x)
              (not (member (assoc-ref x "dest") used-variables)))
         acc
         (cons x acc)))
   '()
   block-instrs))

(define (tdce instrs)
  (define blocks (instructions->blocks instrs))
  ;; (map
  ;;  (lambda (x) ((@ (ice-9 pretty-print) pretty-print) x (current-error-port)))
  ;;  blocks)
  (define new-blocks
    (fold
     (lambda (opt acc)
       (update-blocks-instrs acc opt))
     blocks
     (list
      remove-unused-variables
      remove-unused-variables)))
  (blocks->instrs new-blocks))


(define (remove-unused-variables+ block-instrs)
  "Semi-smartly remove unused variables."

  ;; '(instrs currently-in-use )
  ;; (format (current-error-port) "used-variables: ~a\n" used-variables)
  (define result
    (fold-right
     (lambda (x acc)
       (match acc
         ((instrs variables-in-use)
          (let ((new-variables-in-use
                 (if (assoc "args" x) (vector->list (assoc-ref x "args")) '())))
            (cond
             ((and (assoc "dest" x)
                   (not (member (assoc-ref x "dest") variables-in-use)))
              acc)

             ((and (assoc "dest" x)
                   (member (assoc-ref x "dest") variables-in-use))
              (list (cons x instrs)
                    (lset-union
                     equal?
                     (lset-difference
                      equal?
                      variables-in-use
                      (list (assoc-ref x "dest")))
                     new-variables-in-use)))

             ;; TODO: [Andrew Tropin, 2025-02-20] Update variables-in-use
             (else (list
                    (cons x instrs)
                    (lset-union equal? variables-in-use new-variables-in-use)))))
          )))
     '(() ())
     block-instrs))

  (match result
    ((instrs _) instrs)))

(define (dkp instrs)
  (define blocks (instructions->blocks instrs))
  ;; (map
  ;;  (lambda (x) ((@ (ice-9 pretty-print) pretty-print) x))
  ;;  (car blocks))
  (define new-blocks
    (fold
     (lambda (opt acc)
       (update-blocks-instrs acc opt))
     blocks
     (list remove-unused-variables+)))
  (blocks->instrs new-blocks))

(define (comment)
  ;; (print-blocks tdce-combo-bril)
  (get-blocks bril-program)

  (define tdce-diamond-bril
    (call-with-input-file
        "./src/bril/tdce/diamond.json"
      (lambda (port)
        (json->scm port))))
  "he")

(define-public (identity-transformation)
  (define input (get-string-all (current-input-port)))
  (display input))

(define-public (tdce-transformation)
  (define program (json->scm (current-input-port)))
  (define tdce-function
    (match (command-line)
      (("guile") tdce)
      (("guile" "dkp") dkp)
      (else dkp)))
  (scm->json (transform-program-instrs program tdce-function) (current-output-port)))


;; TODO: [Andrew Tropin, 2025-02-06] Implement DCE and LVN

;; DCE is a Dead Code Elimination
;; DCE: don't forget to re-run it a few times until it stops updating
;; the blocks

;; LVN is Local Value Numbering
;; LVN: Use id semantics to make optimization from LVN deal with copy
;; propogation

;; LVN: Use add/mul commutativity. add a b produces the same value as add b a

;; LVN: Constant propagation, we can use constants instead of id
;; operation referencing a variable containing a constant value
