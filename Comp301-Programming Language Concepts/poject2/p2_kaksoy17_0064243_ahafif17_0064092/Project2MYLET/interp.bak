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
     

      ;; implement var-exp here
      

      ;; implement comp-exp here


      
      ;; implement op-exp here



      
      ;; if-exp
      (if-exp (cond1 exp1 else-exp)
              (let ((val1 (value-of cond1 env)))      
                (if (expval->bool val1)
                    (value-of exp1 env)
                    (value-of else-exp env))))

      ;; implement my-cond-exp here


      
      ;; implement str-exp here


      ;; implement bool-exp here

      
      ;; implement zero-exp here


      ;; implement let-exp here




;(trace value-of)