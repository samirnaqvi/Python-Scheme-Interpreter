(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests))
;Takes in an element first and a list of lists rests, 
; and adds first to the beginning of each list in rests

(define (zip pairs)
  (list (map (lambda (x) (car x)) pairs)
   (map (lambda (x) (car(cdr x))) pairs))
  )

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17  #Uses a helper function to have a counter variable and 
  ; creates a list of list associating the counter variable and the 
  ; corresponding element of the original variable
  (define (helper s num)
    (cond 
    ((null? s) nil)
    (else (cons (cons num (cons (car s) nil))
      (helper (cdr s)(+ num 1)))
      )
    ; Recursively creates the list with the helper functions until the list 
    ; passed in is empty
    )
  )
; Calls the function with the list and original number 0
  (helper s 0)

  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  ; Checks if there are no denominations then there are no combinations
  ; checks if total is less than 0 returns no combinations
  ; checks if total is empty returns empty nested list
  ; Finally, recursively creates a list of all possible combinations
  ; by using the cons-all function with the first denomination 
  ; and then calls the function on that value subtracted from total
  ; It then uses the cdr of denominations to find all of the possible c
  ; combinations, without using the first denomination
  (cond
    ((null? denoms) nil)
    ((< total 0) nil)
    ((= total 0) '(()))
    (else (append (cons-all (car denoms)  
    (list-change (- total (car denoms) ) denoms))
    (list-change total (cdr denoms)))

      )
    )
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
        expr         ; END PROBLEM 19 
          ; just returns expression
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; just returns expression if a quote
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19

           (cons form 
            (cons params (map let-to-lambda body)
            ))    
           ; creates the list used to evaluate
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           ; defines how to handle the let function in terms of lambda
           (cons (list 'lambda (car (zip  values))
            ( let-to-lambda (car body))) 
            ( let-to-lambda (car ( cdr (zip values)))))
           ))
           ; END PROBLEM 19
           
        (else
         ; BEGIN PROBLEM 19
         ( map let-to-lambda expr )
         ; else case map the lambda expr
         ; END PROBLEM 19
         )))
