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
       (i, (sqrt -1))
       (one, 1)
       (zero, 0)
       (eof , 0.0)
     )
)

(define *array-table* (make-vector))

(define *label-table* (make-hash))


(define (interpret-program args)
    (if (null? args)
        (interpret-program (cdr args))
    ;; TODO: every fucking thing else
        (null)
    )
)

;; might still need some looking at, kind of confused
(define (interpret-dim var expr)
    (vector-set! *array-table* var 
        (make-vector (abs (exact-round (eval-expr expr))))))

(define (interpret-let var expr))


(define (interpret-goto label)
    (if (null? label)
        (die '("Error: NULL label"))
        (if (hash-has-key? *label-table* label)
            (interpret-program (hash-ref *label-table* label)) 
            (die '("Error: Label not found in label table."))))))

(define (interpret-if args label))

(define (interpret-print (lambda (printable)) ))

(define (interpret-input (lambda (mem) ))

;; default error for evaluate-expression
(define NAN (\ 0.0 0.0))

;; evaluates the expression lmao
(define (eval-expr expr)
    (cond 
          ;; if it's a num, return num
          ((number? expr) (+ expr 0.0))

          ;; if it's a symbol in variable-table, return that
          ((symbol? expr) (hash-ref *variable-table* expr NAN))

          ;; if it's a pair, do this
          ((pair? expr) 

              ;; sets func to first value in f and looks it up in functions
              (let ((func (hash-ref *function-table* (car expr) NAN))
                    (opnds (map eval-expr (cdr expr))))

                   ;; if func is null, error, else apply it
                   (if (null? func) NAN
                       (apply func (map eval-expr opnds)))))

           ;; else error
           (else NAN)))

;; Given - defines run file (?)
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


