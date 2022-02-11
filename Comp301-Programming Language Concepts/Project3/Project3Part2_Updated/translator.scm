(module translator (lib "eopl.ss" "eopl")
  
  (require "lang.scm")
  (require "environments.scm")

  (provide translation-of-program)

  (define translation-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (a-program                    
            (translation-of exp1 (init-env)))))))

  (define translation-of
    (lambda (exp env)
      (cases expression exp
        
        (const-exp (num) (const-exp num))
        
        (var-exp (var) (var-exp var))
        
        (diff-exp (exp1 exp2)
          (diff-exp
            (translation-of exp1 env)
            (translation-of exp2 env)))
        
        (zero?-exp (exp1)
          (zero?-exp
            (translation-of exp1 env)))
        
        (if-exp (exp1 exp2 exp3)
          (if-exp
            (translation-of exp1 env)
            (translation-of exp2 env)
            (translation-of exp3 env)))
       
        (let-exp (var exp1 body)
          (let-exp
             var
            (translation-of exp1 env)            
            (translation-of body env)))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### THE LINES BELOW ARE PUT ONLY TO PROVIDE YOU
        ; ###### A WORKING CODE BASELINE. THEY ARE REQUIRED
        ; ###### TO CHANGE.
        ; ######
        ; ###### Here, you need to translate the original
        ; ###### proc-exp, call-exp, and letrec-exp to the 
        ; ###### nested versions. To understand how translation
        ; ###### works, you can check the lexaddr code in the
        ; ###### EOPL GUI. 
        ; ###### In this part, you will also do the count 
        ; ###### incrementation!!!
        ; #####################################################

        (proc-exp (var body) ;Done
          (proc-nested-exp
            var
            'n
            'anonym
            (translation-of body env)
          )
        )
        
        (call-exp (rator rand) ;Done
                  (let ((operator (translation-of rator env))
                        (operand (translation-of rand env)))
                    
                        (call-nested-exp 
                         operator
                         operand
                         (var-exp 'n)
                         )
        ))

        (letrec-exp (p-name b-var p-body letrec-body) ;Seems done
          (letrec-nested-exp
            p-name
            b-var
            'n
            (translation-of p-body env)
            (translation-of letrec-body env) 
          )
        )

        ; #####################################################
        
        (else (report-invalid-source-expression exp))
        )))
  
 (define report-invalid-source-expression
    (lambda (exp)
      (eopl:error 'value-of 
        "Illegal expression in source code: ~s" exp)))
)
  