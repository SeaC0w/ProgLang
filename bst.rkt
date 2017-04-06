;; Functions by Kerim Celik and Adam Klein
;; for Programming Languages, 04/03/2017

;; This function creates and returns an empty binary search tree,
;; represented as an empty list.
(define null-bst
  (lambda ()
    '()))

;; This function checks if the passed argument is an empty BST.
;; Arguments:    bst     the object to check.
;; Returns               #t if bst is an empty BST.
;;                       #f if bst is not an empty BST.
(define null-bst?
  (lambda (bst)
    (equal? bst '())))

;; This helper function checks if the passed argument is
;; a valid node in a bst, meaning that it conforms to the format:
;; '(number list list).
;; Arguments:    bst     the object to check.
;; Returns               #t if bst conforms to node format.
;;                       #f if bst fails to conform.
(define valid-bst?
  (lambda (bst)
    ;; First check that bst is a list, then make sure it isn't empty,
    ;; then make sure it contains only three elements, then make sure
    ;; those elements have the correct type.
    (if (and (and (list? bst) (not (null-bst? bst))) (= (length bst) 3))     
        (and (and (number? (car bst)) (list? (cdr bst)))
             (list? (cdr (cdr bst))))
        #f)))

;; Get the value of a node in the passed BST, but first
;; check if the passed object is actually a BST.
;; Arguments:    bst     the object to extract a value from
;; Returns               the entry if bst is a valid node.
;;                       #f if bst is not a valid node.
(define entry
  (lambda (bst)
    (if (valid-bst? bst)
        (car bst)
        #f)))

;; Get the left subtree of a node in the passed BST, but first
;; check if the passed object is actually a BST.
;; Arguments:    bst     the object to extract a subtree from.
;; Returns               the subtree if bst is a valid node.
;;                       #f if bst is not a valid node.
(define left
  (lambda (bst)
    (if (valid-bst? bst)
        (car (cdr bst))
        #f)))

;; Get the right subtree of a node in the passed BST, but first
;; check if the passed object is actually a BST.
;; Arguments:    bst     the object to extract a subtree from.
;; Returns               the subtree if bst is a valid node.
;;                       #f if bst is not a valid node.
(define right
  (lambda (bst)
    (if (valid-bst? bst)
        (car (cdr (cdr bst)))
        #f)))

;; Make a new BST using elt as the root node value, left as the left
;; subtree, and right as the right subtree. First check that each of
;; the passed arguments are valid, subtrees can either be empty or
;; be valid nodes.
;; Arguments:    elt     the root node value for the new tree.
;;               left    the new tree's left subtree.
;;               right   the new tree's right subtree.
;; Returns               the new tree if it can be made.
;;                       #f if an input value is invalid.
(define make-bst
  (lambda (elt left right)
    (if (and (and (number? elt) (or (valid-bst? left) (null-bst? left)))
             (or (valid-bst? right) (null-bst? right)))
        (list elt left right)
        #f)))

;; Return a list containing all values in BST in the order
;; obtained from a pre-order traversal. Recursive implementation.
;; Assumes that it receives a BST as its only argument.
;; Arguments:    bst     the BST to pre-order.
;; Returns               bst's values in pre-ordered arrangement.
(define preorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (list (entry bst))
                (preorder (left bst))
                (preorder (right bst))))))

;; Return a list containing all values in BST in the order
;; obtained from a in-order traversal. Recursive implementation.
;; Assumes that it receives a BST as its only argument.
;; Arguments:    bst     the BST to put in order.
;; Returns               bst's values in in-ordered arrangement.
(define inorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (inorder (left bst))
                (list (entry bst))
                (inorder (right bst))))))

;; Return a list containing all values in BST in the order
;; obtained from a post-order traversal. Recursive implementation.
;; Assumes that it receives a BST as its only argument.
;; Arguments:    bst     the BST to post-order.
;; Returns               bst's values in post-ordered arrangement.
(define postorder
  (lambda (bst)
    (if (null-bst? bst)
        '()
        (append (postorder (left bst))
                (postorder (right bst))
                (list (entry bst))))))

;; Inserts a value in the proper place in the passed BST, the returns
;; the modified version of the BST with the new value added on.
;; If the passed argument is a valid BST, the function will
;; output a valid BST.
;; Arguments:    v       the value to add to the passed BST.
;;               bst     the BST to add a value to.
;; Returns               the modified BST with v added on.
(define insert
  (lambda (v bst)
    
    (cond ((null-bst? bst) (list v '() '()))
          ((< v (entry bst)) (append (list (entry bst))
                                     (list (insert v (left bst)))
                                     (list (right bst))))
          ((> v (entry bst)) (append (list (entry bst))
                                     (list (left bst))
                                     (list (insert v (right bst))))))))


;; Our assortment of test cases. Uncomment to use.
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

;(equal? (make-bst 5 '() '()) '(5 () ()))
;(not (make-bst 'a '() '()))
;(not (make-bst 1 '() 6))

;(preorder '(5 (3 () (4 () ()) ) ()))
;(equal? (preorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(5 3 2 4 6))
;(inorder '(5 (3 () (4 () ()) ) ()))
;(equal? (inorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(2 3 4 5 6))
;(postorder '(5 (3 () (4 () ()) ) ()))
;(equal? (postorder '(5 (3 (2 () ()) (4 () ()) ) (6 () ()))) '(2 4 3 6 5))

;(equal? (insert 1 '()) '(1 () ()))
;(equal? (insert 1 '(4 (2 () ()) (6 () ()))) '(4 (2 (1 () ()) ()) (6 () ())))
;(equal? (insert 3 '(4 (2 () ()) (6 () ()))) '(4 (2 () (3 () ())) (6 () ())))
;(equal? (insert 7 '(4 (2 () ()) (6 () ()))) '(4 (2 () ()) (6 () (7 () ()))))