#lang racket

(require racket/list)

;; Project 3: A church-compiler for Scheme, to Lambda-calculus

(provide church-compile
      ; provided conversions:
      church->nat
      church->bool
      church->listof)


;; Input language:
;
; e ::= (letrec ([x (lambda (x ...) e)]) e)    
;     | (let ([x e] ...) e)  
;     | (let* ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (e e ...)    
;     | x  
;     | (and e ...) | (or e ...)
;     | (if e e e)
;     | (prim e) | (prim e e)
;     | datum
; datum ::= nat | (quote ()) | #t | #f 
; nat ::= 0 | 1 | 2 | ... 
; x is a symbol
; prim is a primitive operation in list prims
; The following are *extra credit*: -, =, sub1  
(define prims '(+ * - = add1 sub1 cons car cdr null? not zero?))

; This input language has semantics identical to Scheme / Racket, except:
;   + You will not be provided code that yields any kind of error in Racket
;   + You do not need to treat non-boolean values as #t at if, and, or forms
;   + primitive operations are either strictly unary (add1 sub1 null? zero? not car cdr), 
;                                           or binary (+ - * = cons)
;   + There will be no variadic functions or applications---but any fixed arity is allowed

;; Output language:

; e ::= (lambda (x) e)
;     | (e e)
;     | x
;
; also as interpreted by Racket


; Using the following decoding functions:

; A church-encoded nat is a function taking an f, and x, returning (f^n x)
(define (church->nat c-nat)
  ((c-nat add1) 0))

; A church-encoded bool is a function taking a true-thunk and false-thunk,
;   returning (true-thunk) when true, and (false-thunk) when false
(define (church->bool c-bool)
  ((c-bool #t) #f))

; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning when-cons applied on the car and cdr elements
; A church-encoded cons-cell is a function taking a when-cons callback, and a when-null callback (thunk),
;   returning the when-null thunk, applied on a dummy value (arbitrary value that will be thrown away)
(define ((church->listof T) c-lst)
  ; when it's a pair, convert the element with T, and the tail with (church->listof T)
  ((c-lst (lambda (a) (lambda (b) (cons (T a) ((church->listof T) b)))))
   ; when it's null, return Racket's null
   (lambda (_) '())))


; Write your church-compiling code below:

; churchify recursively walks the AST and converts each expression in the input language (defined above)
;   to an equivalent (when converted back via each church->XYZ) expression in the output language (defined above)
(require racket/list)
(require racket/equal) ; Add import statement for eq?
(require racket/number) ; Add import statement for number?
(define (churchify e)
  (match e
    [(letrec ([x (lambda (x ...) body)]) rest)
     (quasiquote (lambda ((unquote x) ...)
                    (unquote (churchify body))
                    (unquote (churchify rest))))]
    
    [(let ([x val] ...) body)
     (quasiquote ((lambda ((unquote x) ...)
                    (unquote (churchify body)))
                  (unquote-splicing (map churchify (list val ...)))))]
    
    [(let* ([x val] ...) body)
     (let loop ([bindings (list (cons x val) ...)]
                [rest body])
       (match bindings
         [(list (cons x val) next ...)
          (quasiquote (let (((unquote x) (unquote (churchify val))))
                        (unquote (if (null? next)
                                      (churchify rest)
                                      (loop next rest)))))]))]

    [(lambda (args ...) body)
     (quasiquote (lambda ((unquote args) ...)
                    (unquote (churchify body))))]

    [(e1 e2 ...)
     (quasiquote (((unquote (churchify e1)))
                  (unquote (map churchify (list e2 ...))))]

    [(and exprs ...)
     (quasiquote ((lambda (x)
                    (unquote (apply (lambda args
                                      (quasiquote (if (unquote-splicing args) #t #f)))
                                    (map churchify (list exprs ...)))))
                  #t))]

    [(or exprs ...)
     (quasiquote ((lambda (x)
                    (unquote (apply (lambda args
                                      (quasiquote (if (unquote-splicing args) #t #f)))
                                    (map churchify (list exprs ...)))))
                  #f))]

    [(if cond then-expr else-expr)
     (quasiquote ((lambda (x)
                    (unquote (if-then-else (churchify cond)
                                           (churchify then-expr)
                                           (churchify else-expr))))
                  #t))]

    [(prim e1 e2)
     (quasiquote ((lambda (x)
                    (unquote (apply (lookup-prim prim)
                                    (map churchify (list e1 e2)))))
                  #t))]

    [(prim e)
     (quasiquote ((lambda (x)
                    (unquote (lookup-prim prim (churchify e))))
                  #t))]

    [(quote ())
     (quote ())]

    [datum
     datum]

    [x
     x]))


(define (lookup-prim prim)
  (case prim
    [(+) +]
    [(*) *]
    [(-) -]
    [(=) =]
    [(add1) add1]
    [(sub1) sub1]
    [(cons) cons]
    [(car) car]
    [(cdr) cdr]
    [(null?) null?]
    [(not) not]
    [(zero?) zero?]
    [else (error "Unknown primitive operation" prim)])) ; Handle unknown primitive operations


(define (if-then-else cond then-expr else-expr)
  ;; Helper function for if-then-else
  `(if ,(churchify cond) ,(churchify then-expr) ,(churchify else-expr)))

(define (church-compile program)
  (define todo `(lambda (x) x)) ; Placeholder for primitive operations
  (define prims-table
    ;; Define primitive operations and needed helpers
    (let ([add1 `(lambda (x) (+ x 1))]
          [sub1 `(lambda (x) (- x 1))]
          [cons `(lambda (x y) (lambda (f) (f x y)))]
          [car `(lambda (pair) (pair (lambda (x y) x)))]
          [cdr `(lambda (pair) (pair (lambda (x y) y)))]
          [null? `(lambda (lst) (lst (lambda (x y) #f)) #t)]
          [not `(lambda (b) (b #f #t))]
          [zero? `(lambda (n) (n (lambda (x) #f) #t))])
      (list (cons '+ add1)
            (cons '- sub1)
            (cons '* add1)
            (cons '= add1)
            (cons 'add1 add1)
            (cons 'sub1 sub1)
            (cons 'cons cons)
            (cons 'car car)
            (cons 'cdr cdr)
            (cons 'null? null?)
            (cons 'not not)
            (cons 'zero? zero?))))

  (let* ([program-churchified (churchify program)]
         [final-program (append `(lambda (,,@(map car prims-table))
                                   ,program-churchified)
                                (map cdr prims-table))])
    final-program))
