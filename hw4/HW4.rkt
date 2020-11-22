#lang plait

(define-type-alias Location Number)

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boxV [l : Location])
  (recV [s : (Listof Symbol)]
        [v : (Listof Value)]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE [n : Symbol] 
        [rhs : Exp]
        [body : Exp])
  (lamE [n : Symbol]
        [body : Exp])
  (appE [fun : Exp]
        [arg : Exp])
  (boxE [arg : Exp])
  (unboxE [arg : Exp])
  (setboxE [bx : Exp]
           [val : Exp])
  (getE [r : Exp]
        [n : Symbol])
  (setE [r : Exp]
        [n : Symbol]
        [v : Exp])
  (recordE [s : (Listof Symbol)]
           [args : (Listof Exp)])
  (beginE [exps : (Listof Exp)]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  (cell [location : Location] 
        [val : Value]))

(define-type-alias Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(define-type Result
  (v*s [v : Value] [s : Store]))

(module+ test
  (print-only-errors #t))

;;change----------------------------------------------------------------------------------------
(define-type Results
  (vs*s [v : (Listof Value)]
        [s : Store]))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{box ANY} s)
     (boxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{unbox ANY} s)
     (unboxE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{set-box! ANY ANY} s)
     (setboxE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? `{begin ANY ANY} s)
     (beginE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? `{record {SYMBOL ANY} ...} s)
     (recordE (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? `{get ANY SYMBOL} s)
     (getE (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test (parse `{box 0})
        (boxE (numE 0)))
  (test (parse `{unbox b})
        (unboxE (idE 'b)))
  (test (parse `{set-box! b 0})
        (setboxE (idE 'b) (numE 0)))
  (test (parse `{begin 1 2})
        (beginE (numE 1) (numE 2)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [(v*s v-id sto-id) body]))
                                
;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env] [sto : Store]) : Result
  (type-case Exp a
    [(numE n) (v*s (numV n) sto)]
    [(idE s) (v*s (lookup s env) sto)]
    [(plusE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num+ v-l v-r) sto-r)))]
    [(multE l r)
     (with [(v-l sto-l) (interp l env sto)]
       (with [(v-r sto-r) (interp r env sto-l)]
         (v*s (num* v-l v-r) sto-r)))]
    [(letE n rhs body)
     (with [(v-rhs sto-rhs) (interp rhs env sto)]
       (interp body
               (extend-env
                (bind n v-rhs)
                env)
               sto-rhs))]
    [(lamE n body)
     (v*s (closV n body env) sto)]
    [(appE fun arg)
     (with [(v-f sto-f) (interp fun env sto)]
       (with [(v-a sto-a) (interp arg env sto-f)]
         (type-case Value v-f
           [(closV n body c-env)
            (interp body
                    (extend-env
                     (bind n v-a)
                     c-env)
                    sto-a)]
           [else (error 'interp "not a function")])))]
    [(boxE a)
     (with [(v sto-v) (interp a env sto)]
       (let ([l (new-loc sto-v)])
         (v*s (boxV l) 
              (override-store (cell l v) 
                              sto-v))))]
    [(unboxE a)
     (with [(v sto-v) (interp a env sto)]
       (type-case Value v
         [(boxV l) (v*s (fetch l sto-v) 
                        sto-v)]
         [else (error 'interp "not a box")]))]
    [(setboxE bx val)
     (with [(v-b sto-b) (interp bx env sto)]
       (with [(v-v sto-v) (interp val env sto-b)]
         (type-case Value v-b
           [(boxV l)
            (v*s v-v
                 (override-store (cell l v-v)
                                 sto-v))]
           [else (error 'interp "not a box")])))]
    
    [(beginE lists)
     (type-case (Listof Exp) lists
       [empty (error 'interp "empry check beginE")]
       [(cons f r)(if (empty? r)
                      (interp f env sto)
                      (with [(v-l sto-l) (interp f env sto)]
                            (interp (beginE r) env sto-l)))])]
    
    [(getE l r)
     (with [(v-l sto-l) (interp l env sto)]
           (type-case Value v-l
             [(recV ns vs)
              (type-case Value (find r ns vs) [(boxV loc)(v*s (fetch loc sto-l) sto-l)]
                                              [else (error 'interp "input should be a box")])]
             [else(error 'interp "X record")]))]
    
    [(setE r n v)
     (with [(v-l sto-l) (interp r env sto)](with [(v-n sto-n) (interp v env sto-l)]
                                                 (type-case Value v-l[(recV ns vs)
                                                                      (type-case Value (find n ns vs)
                                                                        [(boxV loc) (v*s v-n (replaceS sto-n (cell loc v-n)))]
                                                                        [else (error 'interp "input should be a box")])]
                                                   [else (error 'interp "X record")])))]

    [(recordE ns args)
     (type-case (Listof Exp) args
       [empty (v*s(recV ns empty)sto)]
       [(cons f r)
        (with [(v-l sto-l) (interp f env sto)]
              (type-case Results (listsInterp r env sto-l)
                [(vs*s vlist sto-ll)
                 (let ([nl (new-loc sto-ll)])
                   (let ([nl+1 (new-loc (override-store (cell nl v-l) sto-ll))])
                     (v*s
                      (recV ns (cons (boxV nl) vlist))
                      (override-store (cell nl+1 (boxV nl))
                                      (override-store (cell nl v-l) sto-ll)))))]))])]
    

    ))

(define (find [n : Symbol] [ns : (Listof Symbol)] [vs : (Listof Value)])
  : Value
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (first vs)
              (find n (rest ns) (rest vs)))]))

(define (update [n : Symbol]
                [v : Value]
                [ns : (Listof Symbol)]
                [vs : (Listof Value)]) : (Listof Value)
  (cond
    [(empty? ns) (error 'interp "no such field")]
    [else (if (symbol=? n (first ns))
              (cons v (rest vs))
              (cons (first vs) 
                    (update n v (rest ns) (rest vs))))]))


(module+ test
  (test (interp (parse `2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse `{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse `{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusE (idE 'x) (idE 'x)) mt-env)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse `{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse `{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse `{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse `{let {[b {box 5}]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))

  (test/exn (interp (parse `{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse `{unbox 1}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{set-box! 1 2}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable"))


(define (interp-expr [e : Exp]) : S-Exp
  (with [(v sto) (interp e mt-env mt-store)]
        (type-case Value v
          [(numV n) (number->s-exp n)]
          [(recV ns vs) `record]
          [(closV arg body env) `function]
          [(boxV l) `box])))


(define (listsInterp [exps : (Listof Exp)]
                     [env : Env]
                     [store : Store]) : Results
  (type-case (Listof Exp) exps
    [empty (vs*s empty store)]
    [(cons f r)
     (with [(v-l sto-l) (interp f env store)]
           (type-case Results (listsInterp r env sto-l)
             [(vs*s vlist sto-ll)
              (let ([nl (new-loc sto-ll)])
                (let ([nl+1 (new-loc (override-store (cell nl v-l) sto-ll))])
                  (vs*s (cons (boxV nl) vlist) (override-store (cell nl+1 (boxV nl)) (override-store (cell nl v-l) sto-ll)))))]))]))

(define (replaceS [store : Store] [storage : Storage]) : Store
  (type-case (Listof Storage) store
    [empty empty]
    [(cons f r)
     (type-case Storage storage
       [(cell l v)
        (type-case Storage f
          [(cell lo va) (if (= l lo)
                              (cons (cell l v) r)
                              (cons f (replaceS r storage)))])])]))




;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (type-case (Listof Storage) sto
   [empty 0]
   [(cons c rst-sto) (max (cell-location c)
                          (max-address rst-sto))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (type-case (Listof Storage) sto
   [empty (error 'interp "unallocated location")]
   [(cons c rst-sto) (if (equal? l (cell-location c))
                         (cell-val c)
                         (fetch l rst-sto))]))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))
