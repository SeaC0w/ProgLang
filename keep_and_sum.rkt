;; Functions by Kerim Celik for Programming Languages, 03/31/2017

;; This function keeps the first n elements of a list,
;; where n is a user-specified integer.
;; The function does this by recursively appending the
;; car of the passed list to the front of a proper list.
;; Assumes argument types are correct:
;; first argument should be an integer,
;; and the second argument should be a proper list.
;; Arguments:     n     number of elements to keep
;;                L     the list to work on
;; Returns:             the list of the first n elements of L
;;                      an empty list if n <= 0
;;                      the whole list if n > length of L
(define keep-first-n
  (lambda (n L)
    (cond ((>= 0 n) '()) ; first edge case check
          ((> n (length L)) L) ; other edge case check
          (else (append (list (car L))
                        (keep-first-n (- n 1) (cdr L)))))))

;; This function sums all elements of a proper list of numbers.
;; The function does this by recursively appending the
;; sum of the first and second elements of the list and then
;; replacing the second element of the list with that sum.
;; Assumes the passed list is a proper list of numbers.
;; Arguments:     L     the list to work on
;; Returns:             the sum of all elements of L
;;                      "NULL" if (length of L) = 0
(define sum
  (lambda (L)
    (cond ((= (length L) 0) 'NULL)
          ((= (length L) 1) (car L))
          (else (sum (append (list(+ (car L) (car (cdr L))))
                             (cdr (cdr L))))))))