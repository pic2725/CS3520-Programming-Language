#lang plait


(define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))


;part1 - Sum

(define (sum [t : Tree]) : Number
  (type-case Tree t
    [(leaf l) l]
    [(node v l r) (+ v(+ (sum l) (sum r)))])
 )

(test (sum (node 5 (leaf 6) (leaf 7))) 18 )
(test (sum (node 1 (leaf 2) (leaf 3))) 6 )
(test (sum (node 0 (leaf 0) (leaf 0))) 0 )

;part2 - Negate

(define (checkNegate (s : Number))
  (if (< 0 s)
      (* -1 s)
       s))

(define (negate [t : Tree]) : Tree
  (type-case Tree t
    [(leaf l) (leaf(checkNegate l))]
    [(node v l r) (node(checkNegate v) (negate l) (negate r ))]))

(test (negate (node 5 (leaf 6) (leaf 7)))  (node -5 (leaf -6) (leaf -7)))
(test (negate (node -5 (leaf 6) (leaf 7)))  (node -5 (leaf -6) (leaf -7)))
(test (negate (node -5 (leaf -6) (leaf -7)))  (node -5 (leaf -6) (leaf -7)))



;part3 - contains?

(define (contains? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
  [(leaf l) (= n l)]
    [(node v l r) (or (or (contains? (leaf v) n) (contains? l n)) (contains? r n))]
    )
  )

(test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 7) #t)
(test (contains? (node 5 (leaf 6) (leaf 7)) 1) #f)
(test (contains? (node 1 (leaf 2) (leaf 3)) 1) #t)

;part4 - Big Leaves?

(define (bigger-leaves? [t : Tree] [n : Number]) : Boolean
  (type-case Tree t
    [(leaf l) (> l n)]
    [(node v l r) (and (bigger-leaves? l (+ v n))(bigger-leaves? r (+ v n)))]
    )
  )

(define (big-leaves? [t : Tree]) : Boolean
  (type-case Tree t
    [(leaf l) (bigger-leaves? (leaf l) 0) ]
    [(node v l r) (and (bigger-leaves? l v) (bigger-leaves? r v))]
    )
  )

(test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
(test (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7))) #f)
(test (big-leaves? (leaf 0)) #f)


;;part5 - Positive Tree?

#;
(define-type (Listof Tree)
  empty
  [cons (first : Tree)
        (rest : Listof Tree)]
  )

(define (sumall [t : (Listof Tree)]) : Number
  (type-case (Listof Tree) t
    [empty 0]
    [(cons first rst) (+ (sum first) (sumall rst))]))



(define (positive-trees? [temp : (Listof Tree)]) : Boolean
  (type-case (Listof Tree) temp
    [empty #t]
    [(cons first rst) (> (+(sum first) (sumall rst)) 0)]))


(test (positive-trees? empty) #t)


(test (positive-trees? (cons (leaf 6)
                               empty))
        #t)

(test (positive-trees? (cons (leaf -6)
                               empty))
        #f)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               empty))
        #t)

(test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #t)

(test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #f)

      