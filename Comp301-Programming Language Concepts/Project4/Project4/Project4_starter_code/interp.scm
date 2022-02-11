(module interp (lib "eopl.ss" "eopl")
  
  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")
  (require "store.scm")
  
  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program 
    (lambda (pgm)
      (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))
      
        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        (proc-exp (var body)
          (proc-val (procedure var body env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator env)))
                (arg (value-of rand env)))
            (apply-procedure proc arg)))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env)))

        (begin-exp (exp1 exps)
          (letrec 
            ((value-of-begins
               (lambda (e1 es)
                 (let ((v1 (value-of e1 env)))
                   (if (null? es)
                     v1
                     (value-of-begins (car es) (cdr es)))))))
            (value-of-begins exp1 exps)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (ref-val (newref v1))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env)))
            (let ((ref1 (expval->ref v1)))
              (deref ref1))))

        (setref-exp (exp1 exp2)
          (let ((ref (expval->ref (value-of exp1 env))))
            (let ((v2 (value-of exp2 env)))
              (begin
                (setref! ref v2)
                (num-val 23)))))

        ; #####################################################
        ; ###### ENTER YOUR CODE HERE
        ; ###### value-of cases for new expressions, remember
        ; ###### that you need to use memory functionalities. 
        ; #####################################################
        (newarray-exp (exp1 exp2)
                       (let ((len (expval->num (value-of exp1 env)))
                             (val (value-of exp2 env)))
                         (newarray len val) ; A procedure which creates an array with  length and its first value
                      ))

        (update-array-exp (exp1 exp2 exp3)
                       (let ((arr (expval->arr (value-of exp1 env))) ;The array
                             (idx (expval->num (value-of exp2 env))) ;Index of the entry which will be updated
                             (val (value-of exp3 env))) ;New value
                         (update-array arr idx val) ; A procedure which updates an array with given array index and new value
                  ))


        (read-array-exp (exp1 exp2)
                       (let ((arr (expval->arr (value-of exp1 env))) ;The array
                             (idx (expval->num (value-of exp2 env)))) ;Index of the entry which will be updated
                           (read-array arr idx) ; A procedure which updates an array with given array index and new value
                  ))
        

        (print-array-exp (exp1)
                       (let ((arr (expval->arr (value-of exp1 env)))) ;The array
                         (print-array arr)))
                              
        ; #####################################################
        ;Cases for stack expressions.
        ; A note: Stacks are simply arrays with length 1000. We are filling the array from end to begining. (ForExample: First push is done to index 999 of the array)
        (newstack-exp ()
         (newarray 1000 -1)) ;-1 means null in the stack implementation since values can be 1-10000 

        (stack-push-exp (exp1 exp2)
          (let ((stack (expval->arr (value-of exp1 env))) ;Stack is simply an array
                (val (expval->num (value-of exp2 env)))) ;Taking the value which will be pushed
            (cases expval val
              (num-val (num)  (push stack val))
              (else
               "dummy"))))
        
        (stack-pop-exp (exp1)
                      (let ((stack (expval->arr (value-of exp1 env))))
                        (num-val (pop stack))))
        (stack-top-exp (exp1)
                      (let ((stack (expval->arr (value-of exp1 env))))
                        (num-val (top stack))))

        (stack-size-exp (exp1)
                         (let ((stack (expval->arr (value-of exp1 env))))
                           (num-val (stack-size stack))))

        (empty-stack?-exp (exp1)
                           (let ((stack (expval->arr (value-of exp1 env))))
                             (bool-val (is-stack-empty stack))))

        (print-stack-exp (exp1)
                         (let ((stack (expval->arr (value-of exp1 env))))
                             (print-stack stack)))
        

        ; Array comprehension expression

        (array-comprehension-exp (exp1 var exp2)
                                 (let ((arr (expval->arr (value-of exp2 env)))) ;This returns an array
                                   (let ((len (arr-len arr))) ;taking the length of the array
                                     (begin
                                       (arraycomp-helper exp1 arr var len 0 env) ;This returns nothing
                                       (arr-val arr)) ;So we need to return that arr as exp val
                                     )))

        
        
        )))


  ; ###### YOU CAN WRITE HELPER FUNCTIONS HERE
;;Operators will be implemented here !

;; Helper for array-comp-exp
   (define arraycomp-helper
     (lambda (body arr var len idx saved-env)
       (if (< idx len) ;Checking we finish or not
          (let ((entryVal (read-array arr idx))) ;Taking the value of that entry (arr[i]) 
           (let ((val (value-of body (extend-env var entryVal saved-env))) ;Evaluating the body with that entry(arr[i])
                 (nextIdx (+ 1 idx))) ;Calculating new index
             (begin
               (update-array arr idx val) ;;Updating the index with this value
               (arraycomp-helper body arr var len nextIdx saved-env) ;Recursively calling for the next i
               )))
           '() ;For the else condition, we do not need to nothing, just returning a empty list(null)
           )))
                 


    
;; #########################  Helpers for array implementation  #########################
  (define newarray ;Returns an arr-val
    (lambda (len val)
      (if (> len 0)
          (let ((arrRef (newref val))) ;arrRef --> is the reference to the first element
            (begin
              (newarray-Helper (- len 1) val)
              (arr-val (arr len arrRef)))) ;Creating the array
          (display "Given length is not valid !") ; indicates an error occured when creating the array
          )))
              
              
  (define read-array
    (lambda (arr idx)
      (let ((first (arr-first arr)) ;Taking the reference to the first element of the array
            (len (arr-len arr))) ;Taking the length
        (if (>= idx len) ;Checking index is valid or not
             (display "Given index is not valid !")
             (deref (+ first idx)))))) ; Since we hold the first reference, we need to add the index


  (define update-array
    (lambda (arr idx val)
       (let ((first (arr-first arr)) ;Reference to the first element
            (len (arr-len arr))) ;Length of the array
        (if (>= idx len)
             (display "Given index is not valid !")
             (setref! (+ first idx) val))))) ;Using set ref by giving first + idx since we hold reference of the first element

  
  (define print-array
    (lambda (arr)
      (let ((first (arr-first arr)) ;Reference to the first element
            (len (arr-len arr))) ;Length of the array
        (print-helper (len first)))))


(define newarray-Helper
  (lambda (n val)
    (if(= n 0)
    0
    (begin (newref val) ;Initially all the elements other than 0th have value 'null as default
           (newarray-Helper (- n 1) val)
          ))))

; A helper which prints the elemnts of the array (recursively)
  (define print-helper
    (lambda (n ref) ;n is the length , ref would be the next element which will be printed
      (if (= n 0)
          (display "End of the array")
          (begin
            (display (deref ref))
            (print-helper (- n 1) (+ ref 1))))))
  
;A helper which returns length of the array
  (define arr-len
    (lambda (givenArray)
      (cases array givenArray
        (arr (size first)
             size))))

; A helper which returns the reference of the first element
   (define arr-first
    (lambda (givenArray)
      (cases array givenArray
        (arr (size first)
             first))))

;######################### Helpers for stack implementation #########################

  ; Since we are using arrays while implementing stacks we need indexes,this function finds the index of the array where to push. In our stack implementation, we fill the stack
  ;from end of the array to the begining of it.
  (define find-next-idx ;If stack is full, returns -1 
    (lambda (stack n)
      (let ((temp (read-array stack n))) ;Using read-array function
        (if (< n 0) -1 
            (if (= temp -1)
                n
                (find-next-idx stack (- n 1))))
        )))

  ;This function returns true if the stack is empty
  (define is-stack-empty
    (lambda (stack)
      (let ((temp (find-next-idx stack 999))) ;If the next index is 999, it means there is no element in the stack since the first push is done to 999 of the array
        (if (= 999 temp) #t ;Means the stack is empty
            #f
            ))))

  ;Push operation
  (define push
    (lambda (stack val)
      (let ((nextIdx (find-next-idx stack 999))) ;;Since stacks are length 1000 arrays, last index is always 999
        (update-array stack nextIdx val)))) ;Using update-array will handle push operation easily 

  ; Pop operation
  (define pop
    (lambda (stack)
      (if (is-stack-empty stack)
          -1
          (let ((idx (+ (find-next-idx stack 999) 1))) ;;If we find where to push next, n-1 will be the top element
            (let ((element (read-array stack idx)))
              (begin
                (update-array stack idx -1) ; Since we pop it, we need to turn its value to -1
                element
                ))))))

  ;Top operation
  (define top
    (lambda (stack)
      (if (is-stack-empty stack)
          -1
          (let ((idx (+ (find-next-idx stack 999) 1))) ;;If we find where to push next, n+1 will be the top element
            (let ((element (read-array stack idx)))
                element
                )))))

  (define stack-size
    (lambda (stack)
      (let ((nextIdx (find-next-idx stack 999))) ;Since we fill the array from end to begining and find-next-idx returns
        (- 999 nextIdx)                          ;the next idx(if array is full -1),we can simply return 999 - nextidx
      )))


  ;Stack printer
  (define print-stack
    (lambda (stack)
      (let ((topIdx (+ (find-next-idx stack 999) 1)))
        (print-stack-helper stack topIdx))))

  (define print-stack-helper
    (lambda (stack topIdx)
      (if (> topIdx 999) ;Means printing is done
          (display "End of the Stack")
          (begin
            (display (read-array stack topIdx)) ;Printing form can be changed
            (print-stack-helper stack (+ 1 topIdx)) ; Recursively calling
            ))))
        
  

;;; ########################################################################### 
  
  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; 
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (var body saved-env)
	  (let ((r arg))
	    (let ((new-env (extend-env var r saved-env)))
	      (when (instrument-let)
		(begin
		  (eopl:printf
		    "entering body of proc ~s with env =~%"
		    var)
		  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list)))
                  (eopl:printf "~%")))
              (value-of body new-env)))))))


  ;; store->readable : Listof(List(Ref,Expval)) 
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))
 
  )
  


  
