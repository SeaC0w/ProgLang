(define null-bst
  (lambda ()
    '()))

(define null-bst?
  (lambda (bst)
    (equal? bst '())))

(define valid-bst?
  (lambda (bst)
    ;(if (not ((and (list? bst)) (or (null-bst? bst) (not (= (length bst) 3)))))
    (if (and (and (list? bst) (not (null-bst? bst))) (= (length bst) 3))         
        (and (and (number? (car bst)) (list? (cdr bst)))
             (list? (cdr (cdr bst))))
        #f)))

        
(define entry
  (lambda (bst)
    (if (valid-bst? bst)
        (car bst)
        #f)))

(define left
  (lambda (bst)
    (if (valid-bst? bst)
        (car (cdr bst))
        #f)))

(define right
  (lambda (bst)
    (if (valid-bst? bst)
        (car (cdr (cdr bst)))
        #f)))

(define make-bst
  (lambda (elt left right)
    (if (and (and (number? elt) (or (valid-bst? left) (null-bst? left)))
             (or (valid-bst? right) (null-bst? right)))
        (list elt left right)
        #f)))

(define preorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (list (entry bst)) (preorder (left bst)) (preorder (right bst))))))

(define inorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (inorder (left bst)) (list (entry bst)) (inorder (right bst))))))

(define postorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (postorder (left bst)) (postorder (right bst)) (list (entry bst))))))

(define insert
  (lambda (v bst)
    
    (cond ((null-bst? bst) (list v '() '()))
          ((< v (entry bst)) (append (entry bst) (insert v (left bst)) (right bst)))
          ((> v (entry bst)) (append (entry bst) (left bst) (insert v (right bst)))))))

;;tests
(insert 1 '())

;(make-bst 5 '() '())
;(not (make-bst 'a '() '()))
;(not (make-bst 1 '() 6))

;(preorder '(5 (3 () (4 () ()) ) ()))
;(equal? (preorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(5 3 2 4 6))
;(inorder '(5 (3 () (4 () ()) ) ()))
;(equal? (inorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(2 3 4 5 6))
;(postorder '(5 (3 () (4 () ()) ) ()))
;(equal? (postorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(2 4 3 6 5))

;(null-bst? (null-bst))
;(null-bst? '(1))
;(valid-bst? '(4 (5 () ()) (6 () ())))
;(valid-bst? '(4 () ()))
;(valid-bst? '())
;(valid-bst? '(a b))
;(entry '())
;(entry '(1))
;(entry '(1 () ()))
;(entry '(1 () () ()))
;(left '(1 (2) ()))
;(left '(1 (2 () ()) ()))
;(right '(1 () (2)))
;(right '(1 () (2 () ())))