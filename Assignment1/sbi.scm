#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.19 2020-01-14 16:58:47-08 - - $
;;
;; STUDENTS
;;    Timothy Wang tqwang
;;    Eric Mar emmar
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; Given - Defines consts for stdin, stdout, stderr, and arg-list
(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))
(define *arg-list* (vector->list (current-command-line-arguments)))


;; Hash-table with all functions needed for lookup
(define *function-table* (make-hash))

(for-each
    (lambda (pair)
        (hash-set! *function-table* (car pair) (cadr pair)))
    `( 
           (+ , +) 
           (- , -) 
           (* , *)
           (/ , /)
	   (^ , expt)
           (= , =)
           (< , <)
           (> , >)
           (!= , not_equal)
           (>= , >=)
           (<= , <=)
           (abs , abs)
           (acos , acos)
           (asin , asin)
           (atan , atan)
	   (ceiling , ceiling)
           (cos , cos)
           (exp , exp)
           (floor , floor)
	   (log , log)
	   (round , round)           
           (sin , sin)
           (sqrt , sqrt)
           (tan , tan)
           (truncate , truncate)
      )
)

(define *variable-table* (make-hash))
	 
;; Defines pi, e, non-numbers, and (?) end of file
(for-each
    (lambda (pair) 
        (hash-set! *variable-table* (car pair) (cadr pair)))
    `( 
       (e , (exp 1.0))
       (pi , (acos -1.0))
       (nan , (/ 0.0 0.0))
       (eof , 0.0)
     )
)

(define *array-table* (make-vector))

(define *label-table* (make-hash))



(define (interpret-program args))

(define (interpret-dim var expr))

(define (interpret-let var expr))

(define (interpret-goto label))

(define (interpret-if args label))

(define (interpret-print (lambda (printable)) ))

(define (interpret-input (lambda (mem) ))

;; evaluates the expression lmao
(define (evaluate-expression f))
    ;; if it is a number, return it
    (if (number? f) (+ f 0.0))
    ;; if it is a symbol in the table, look it up then return it
    (if (variable? f) (+ (hash-ref *variable-table* f 0) 0.0) 
    ;; TODO: if it's anything else


;; Given - defines run file
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; Given - for when given an error, exits
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; Given - exit out of program with error statement
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; Given - reads list from input file
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         ;; If it's not the right type, errors, otherwise reads it
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; Given - reads token from stdin?? not too sure
(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))


;; Given - prints out what program is doing
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~a~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (for-each (lambda (line) (printf "~a~n" line)) program)
    (printf ")~n"))

;; Given - Main Function
(define (main arglist)
    ;; Check to make sure exactly 1 input, assumes sbir file
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))

;; Given - runs main
(main *arg-list*)


