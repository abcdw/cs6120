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
          (let ((new-block (if (label? x) current-block (cons x current-block)))
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
  ;;  (lambda (x) ((@ (ice-9 pretty-print) pretty-print) x))
  ;;  (car blocks))
  (blocks->instrs (update-blocks-instrs blocks remove-unused-variables)))

;; ((@ (ice-9 pretty-print) pretty-print)
;;  (transform-program-instrs tdce-combo-bril tdce))

;; (define (edges->adjacency-list edges)
;;   (define (add-vertex v)
;;     (lambda (l)
;;       (cons v l)))
;;   (reverse
;;    (map
;;     (lambda (x)
;;       (cons
;;        (car x)
;;        (reverse (cdr x))))
;;     (fold
;;      (lambda (x acc)
;;        (update-alist acc (car x) (add-vertex (cdr x)) '()))
;;      '()
;;      edges))))

;; (define (topolgical-sort adjacency-list))


;; (update-alist '((key . (hi)) (key2 . (hello)))
;;               'key (lambda (x) '(hello there)) '())


;; (display (blocks->bril-program
;;           (vector-ref (get-blocks bril-program) 0)))
;; (newline)

(define (comment)
  (print-blocks tdce-combo-bril)
  (get-blocks bril-program)

  (define tdce-combo-bril
    (call-with-input-file
        "./src/bril/tdce/combo.json"
      (lambda (port)
        (json->scm port))))
  "he")

(define-public (identity-transformation)
  (define input (get-string-all (current-input-port)))
  (display input))

(define-public (tdce-transformation)
  (define program (json->scm (current-input-port)))
  (scm->json (transform-program-instrs program tdce) (current-output-port)))

;; TODO: [Andrew Tropin, 2025-02-13] Use guix for creating a script,
;; which contains pipeline and can be re-executed from repl

;; (scm->json bril-program )
;; (run-bril bril-program)


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
