;; Functions by Kerim Celik
;; for Programming Languages, 04/10/2017

;; Function given by the instructor, modified slightly.
;; Defines a cons cell whose car is an integer cdr is a function
;; that will return another pair of this kind, whose value will be
;; the next consecutive integer. The cdr of the final cons cell
;; stores a function that evaluates to #f.
;; Arguments:    first   The first integer to go in the list.
;;               last    The last integer to go into the list.
;; Returns               The constructed lazy-list of
;;                       consecutive integers.
(define seq
  (lambda (first last)
    (if (> first last)
        #f
        (cons first
              (lambda () (seq (+ first 1) last))))))

;; Defines a cons cell whose car is an integer cdr is a function
;; that will return another pair of this kind, whose value will be
;; the next consecutive integer. Infinite length, no final cons cell.
;; Arguments:    first   The first integer to go in the list.
;; Returns               The constructed lazy-list of
;;                       consecutive integers.
(define inf-seq
  (lambda (first)
    (cons first
          (lambda () (inf-seq (+ first 1))))))

;; Given a lazy-list, returns its first n elements as a list,
;; where n is an integer passed as an argument.
;; Arguments:    lazy-list    The lazy-list to work on.
;;               n            The number of elements to place
;;                            in the return list.
;; Returns                    The constructed list of the
;;                            first n elements from lazy-list.
;;                            An empty list, if lazy-list is empty.
;;                            The entire list, is n is larger than
;;                            the length of the lazy-list.
(define first-n
  (lambda (lazy-list n)
    (cond ((equal? lazy-list #f) '())
          ((or (= n 1) (equal? ((cdr lazy-list)) #f))
           (list (car lazy-list)))
          (else (cons (car lazy-list)
                      (first-n ((cdr lazy-list)) (- n 1)))))))

;; Given a lazy-list, returns its nth element, where n is
;; an integer passed as an argument.
;; Arguments:    lazy-list    The lazy-list to work on.
;;               n            The list index of the element to return.
;; Returns                    The element at the list's nth index.
;;                            If fewer than n elements, #f.
(define nth
  (lambda (lazy-list n)
    (cond ((= n 1) (car lazy-list))
          ((equal? #f ((cdr lazy-list))) #f)
          (else (nth ((cdr lazy-list)) (- n 1))))))

;; Given a lazy-list, returns a lazy-lisy with all multiples of n
;; removed, where n is a number specified by the user.
;; Arguments:    lazy-list    The lazy-list to work on.
;;               n            The number whose multiples are to
;;                            be removed from the lazy-list.
;; Returns                    A lazy-list without all multiples of n.
(define filter-multiples
  (lambda (lazy-list n)
    (cond ((= n 1) #f)
          ((equal? ((cdr lazy-list)) #f)
           (if (= (modulo (car lazy-list) n) 0)
               (cdr lazy-list)
               (cons (car lazy-list) (cdr lazy-list))))
          ((= (modulo (car lazy-list) n) 0)
           (filter-multiples ((cdr lazy-list)) n))
          (else (cons (car lazy-list)
                      (lambda () (filter-multiples ((cdr lazy-list))
                                                   n)))))))

;; Helper function for primes function below.
;; Takes a lazy-list, then returns a lazy-list
;; without the multiples of any of elements in the
;; list, working from the front of the list to the back.
;; Arguments:    lst       the lazy-list to work on.
;; Returns                 the modified lazy-list.
(define builder
  (lambda (lst)
    (cons (car lst)
          (lambda () (builder (filter-multiples ((cdr lst))
                                             (car lst)))))))

;; Returns the lazy-list containing all prime numbers.
;; Implemented using "builder" helper function above.
(define primes
  (lambda ()
    (builder (inf-seq 2))))


;; My assortment of test cases. Uncomment to use.
;(equal? ((cdr ((cdr ((cdr (seq 1 3))))))) #f)
;(= 7 (car ((cdr ((cdr ((cdr (inf-seq 4)))))))))
;(first-n (inf-seq -5) 6)
;(first-n (seq 1 2) 21)
;(nth (inf-seq 1) 1)
;(nth (inf-seq 1) 74)
;(nth (seq 1 5) 3)
;(nth (seq 1 2) 21)
;((cdr ((cdr ((cdr (filter-multiples (seq 1 5) 2)))))))
;(first-n (filter-multiples (seq 1 5) 2) 10)
;(first-n (filter-multiples (seq 1 10) 3) 10)
;(first-n (filter-multiples (seq 1 4) 1) 5)
;(first-n (seq 2 1) 5)

;; Primes test takes a bit longer.
;(= (nth (primes) 20) 71)