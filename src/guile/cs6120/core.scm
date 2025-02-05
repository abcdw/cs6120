(define-module (cs6120 core)
  #:use-module (json)
  #:use-module (ice-9 match)
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

(define (comment)
  (define bril-program
    (call-with-input-file
        "/data/abcdw/work/sampsyo/bril/tmp/jmp.json"
      (lambda (port)
        (json->scm port))))
  "hi")

(vector-map
 (lambda (i x)
   (format #t "=============\n")
   ((@ (ice-9 pretty-print) pretty-print)
    (instructions->blocks (function-instructions x)))
   (blocks->control-flow-graph
    (instructions->blocks (function-instructions x))))
 (assoc-ref bril-program "functions"))
