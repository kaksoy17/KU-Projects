#lang eopl

;; interpreter for the LET language. 

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;; implement const-exp here
      (const-exp (num) (num-val num))
      
      ;; implement var-exp here
      (var-exp (var) (apply-env env var))

      ;; implement comp-exp here
       (comp-exp (exp1 exp2 exp3)
              (let ((val1 (expval->num (value-of exp1 env)))
                    (comp (expval->string (value-of exp2 env)))
                    (val2 (expval->num (value-of exp3 env))))
                (cond ((equal? "'greater'" comp) (bool-val (> val1 val2)))
                      ((equal? "'equal'" comp) (bool-val (equal? val1 val2)))
                      ((equal? "'less'" comp) (bool-val (< val1 val2)))
                      (else "Invalid comp operation"))))
      
      ;; implement op-exp here

      (op-exp (exp1 exp2 exp3)
              (let ((num1 (expval->num (value-of exp1 env)))
                    (op (expval->string (value-of exp2 env)))
                    (num2 (expval->num (value-of exp3 env))))
                (cond ((equal? "'add'" op) (num-val (+ num1 num2)))
                      ((equal? "'sub'" op) (num-val (- num1 num2)))
                      ((equal? "'div'" op) (num-val (/ num1 num2)))
                      ((equal? "'mult'" op) (num-val (* num1 num2)))
                      (else op))))

      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here
      ;(my-cond-exp (exp1 exp2 (arbno expression) exp3)
       ;            (exp1))

      
      ;; implement str-exp here

      (str-exp (str) (str-val str))
      
      ;; implement bool-exp here
      
      (bool-exp (strB)
                  (cond ((equal? strB "#true") (bool-val #t))
                        ((equal? strB "#false") (bool-val #f))
                        (else "errorInsideCaseBoolExp")))
      
      ;; implement zero-exp here
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (bool-val (zero? num1)))
                   )
                 )


      ;; implement let-exp here
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; Implementing mod-exp
      (mod-exp (exp1 exp2)
               (let ((a (expval->num (value-of exp1 env)))
                     (n (expval->num (value-of exp2 env))))
                 (num-val (mod-helper a n))))
                 
)))

(define (mod-helper x n)
  (cond ((= x n) 0)
        ((< x 0) (mod-helper (+ x n) n))
        ((< x n) x)
        ((> x n) (mod-helper (- x n) n))))
;(trace value-of)

