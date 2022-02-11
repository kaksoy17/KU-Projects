(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")

  (provide translation-of-program)
  ;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

  ;; translation-of-program : Program -> Nameless-program
  ;; Page: 96
  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-senv)))))))

  ;; translation-of : Exp * Senv -> Nameless-exp
  ;; Page 97
  (define translation-of
    (lambda (exp senv)
      (cases expression exp
        
        (const-exp (num) (const-exp num))
        
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)))
        
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 senv)))
        
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 senv)
            (translation-of exp2 senv)
            (translation-of exp3 senv)))
        
        (var-exp (var)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of var-exp here
                   (let ((num (apply-senv-number senv var)))
                          (let ((var1 (symbol->string var))
                                (num1 (number->string num)))
                             (var-exp (string->symbol (string-append var1 num1))))    
                          ))
                   
               
        
        (let-exp (var exp1 body)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of let-exp here
                 (let-exp
                 (let ((num (apply-senv-number senv var)))
                       (let ((var1 (symbol->string var))
                             (num1 (number->string (+ 1 num))))
                             (string->symbol (string-append var1 num1)))  
                          ) ;The variable
                 (translation-of exp1 senv) ;expression            
                 (translation-of body (extend-senv var senv));Body
                 ))



        
        (proc-exp (var body)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;implement translation of proc-exp here   
                    (proc-exp
                     (let ((num (apply-senv-number senv var)))
                       (let ((var1 (symbol->string var))
                             (num1 (number->string (+ 1 num))))
                             (string->symbol (string-append var1 num1)))  
                          )
                     (translation-of body
                                     (extend-senv var senv))))
        
        
        (call-exp (rator rand)
          (call-exp
            (translation-of rator senv)
            (translation-of rand senv)))
        (else (report-invalid-source-expression exp))
        )))

  (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
  
   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;
  
  ;;; Senv = Listof(Sym)
  ;;; Lexaddr = N

  ;; empty-senv : () -> Senv
  ;; Page: 95
  (define empty-senv
    (lambda ()
      '()))

  ;; extend-senv : Var * Senv -> Senv
  ;; Page: 95
  (define extend-senv
    (lambda (var senv)
      (cons var senv)))

  ;;;;define apply-senv-number, a procedure that applies the environment
  ;;;;and finds the occurences of variable var in the environment senv


  (define apply-senv-number
    (lambda (senv var)
      (cond
        ((null? senv) 0);End of the environment
        ((eqv? var (car senv)) (+ 1 (apply-senv-number (cdr senv) var)))
        (else (+ 0 (apply-senv-number (cdr senv) var)))
         )))


  
  ;; apply-senv : Senv * Var -> Lexaddr
  ;; Page: 9
  (define apply-senv
    (lambda (senv var)
      (cond
        ((null? senv) (report-unbound-var var))
        ((eqv? var (car senv))
         0)
        (else
          (+ 1 (apply-senv (cdr senv) var))))))


  

  (define report-unbound-var
    (lambda (var)
      (eopl:error 'translation-of "unbound variable in code: ~s" var)))

  ;; init-senv : () -> Senv
  ;; Page: 96
  (define init-senv
    (lambda ()
      (extend-senv 'i
        (extend-senv 'v
          (extend-senv 'a
            (empty-senv))))))
  
  )
