;; Functions by Kerim Celik
;; for Programming Languages, 04/05/2017

;; This function creates a list of consecutive integers
;; from the first passed number to the second passed number.
;; Returns an empty list if starting integer is
;; greater than the stop integer.
;; Arguments:    start   the first integer to go in the list.
;;               stop    the last integer to go into the list.
;; Returns               the constructed list of consecutive integers.
(define gen-list
  (lambda (start stop)
    (if (> start stop)
        '()
        (cons start (gen-list (+ 1 start) stop)))))

;; This function checks if consecutive entries in a passed list
;; sum up to a passed value.
;; Arguments:    lst    the list to check.
;;               v      the value to check for.
;; Returns              #t if the value can be found by summing
;;                      consecutive entries.
;;                      #f if the value cannot be found.
(define pair-sum?
  (lambda (lst v)
    (cond ((null? (cdr lst)) #f)
          ((= (+ (car lst) (car (cdr lst))) v) #t)
          (else (pair-sum? (cdr lst) v)))))

;; Function given by the instructor. Defines a pair of values whose
;; cdr is a function that will return another pair of this kind.
;; Arguments:    start   the first integer to go in the list.
;;               stop    the last integer to go into the list.
;; Returns               the constructed list of consecutive integers.
(define gen-lazy-list
  (lambda (start stop)
    (if (> start stop)
        #f
        (cons start
            (lambda () (gen-lazy-list (+ start 1) stop))))))

;; This function checks if consecutive entries in a passed lazy-list
;; sum up to a passed value. Depends on the structure of a
;; lazy-list to execute properly.
;; Arguments:    lst    the list to check.
;;               v      the value to check for.
;; Returns              #t if the value can be found by summing
;;                      consecutive entries.
;;                      #f if the value cannot be found
(define pair-sum-lazy?
  (lambda (lst v)
    (cond ((equal? ((cdr lst)) #f) #f)
          ((= (+ (car lst) (car ((cdr lst)))) v) #t)
          (else (pair-sum-lazy? ((cdr lst)) v)))))

;; My assortment of test cases. Uncomment to use.
;(gen-list '6 '9)
;(gen-list '100 '2)
;(pair-sum? (gen-list '1 '4) '7)
;(pair-sum? (gen-list '1 '3) '7)
;(pair-sum? (gen-list '1 '20) '200)
;(pair-sum-lazy? (gen-lazy-list '1 '3) '5)
;(pair-sum-lazy? (gen-lazy-list '2 '4) '7)
;(pair-sum-lazy? (gen-lazy-list '1 '2) '5)
;(pair-sum-lazy? (gen-lazy-list '3 '4) '5)