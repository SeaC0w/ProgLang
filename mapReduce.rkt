(define my-map
  (lambda (f lst)
    (if (null? lst)
        lst
        (cons (f (car lst)) (map f (cdr lst))))))

(define mult5
  (lambda (x)
    (* x 5)))

;; check against dave's
(define my-foldl
  (lambda (f v lst)
    (if (null? lst)
        v
        (my-foldl f
                  (f (car lst) v)
                  (cdr lst)))))

(my-map mult5 '(1 3 6))
