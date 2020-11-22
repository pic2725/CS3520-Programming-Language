#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : Symbol]
         [body : Exp]
         [env : Env])
  (boolV [b : Boolean])
  (thunkV [p : Exp]
          [v : Env])
  )

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
  (boolE [b : Boolean])
  (equalE [l : Exp]
          [r : Exp])
  (condIfE [cond : Exp]
           [t : Exp]
           [f : Exp])
  (unletE [n : Symbol]
          [b : Exp])
  (delayE [e : Exp])
  (forceE [e : Exp])
  )

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (local [(define tempSymbol (s-exp->symbol s))]
       (if (or (eq? tempSymbol 'true)(eq? tempSymbol 'false))
           (boolE (if (eq? tempSymbol 'true) #t #f))(idE tempSymbol)))]
    
    [(s-exp-match? `{delay ANY} s)
     (delayE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{force ANY} s)
     (forceE (parse (second (s-exp->list s))))]
    
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
    
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    
    [(s-exp-match? `{= ANY ANY} s)
     (equalE (parse (second (s-exp->list s)))(parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{if ANY ANY ANY} s)
     (condIfE (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s)))(parse (fourth (s-exp->list s))))]

    
    [(s-exp-match? `{unlet SYMBOL ANY} s)
     (let ([temp (s-exp->list s)])(unletE (s-exp->symbol (second temp))(parse (third temp))))]
  
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
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  
  )

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind n (interp rhs env))
                                env))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]
  
     [(boolE b) (boolV b)]
 
     [(equalE l r) (checkNum (interp l env) (interp r env))]
  
     [(condIfE c t f) (if (checkBool (interp c env))
                        (if (equal? (boolV #t) (interp c env))(interp t env)(interp f env))
                        (error 'interp "not a boolean"))]
     [(unletE n b) (interp b (helperRemove n env))]

     [(forceE f)
      (type-case Value (interp f env)
                  [(thunkV exp t-env) (interp exp t-env)]
                  [else (error 'interp "not a thunk")])]


     [(delayE d) (thunkV d env)]))



(define (checkBool [v : Value]) : Boolean
  (type-case Value v
    [(numV n) #f]
    [(closV a b c) #f]
    [(boolV f) #t]
    [(thunkV exp env) #f]))

;; checkBool TESTS ----
(test (checkBool (numV 999))#f)
(test (checkBool (boolV #f))#t)
(test (checkBool (closV 'a (multE (idE 'a) (idE 'a)) mt-env))#f)
(test (checkBool (thunkV (multE (numE 999)(idE 'x)) (list (bind 'x (numV 999))))) #f)



(define (checkNum [lv : Value] [rv : Value]) : Value
  (type-case Value lv
    [(numV n)
     (type-case Value rv
       [(numV temp) (boolV (= n temp))]
       [(closV a b c) (error 'interp "not a number")]
       [(boolV b) (error 'interp "not a number")]
       [(thunkV p v) (error 'interp "not a number")])]
    
    [(boolV b) (error 'interp "not a number")]
    [(closV a b c) (error 'interp "not a number")]
    [(thunkV p v) (error 'interp "not a number")]))

;; checkNum TESTS ----
(test (checkNum (numV 123) (numV 123))(boolV #t))
(test (checkNum (numV 123) (numV 321))(boolV #f))

(test/exn (checkNum (numV 123) (boolV #f))"not a number")

(test/exn (checkNum (numV 1) (closV 'a (multE (idE 'a) (idE 'a)) mt-env))"not a number")
          
(test/exn (checkNum (numV 1) (thunkV (multE (numE 999)(idE 'x)) (list (bind 'x (numV 999)))))"not a number")

(test/exn (checkNum (boolV #t) (boolV #f))"not a number")

(test/exn (checkNum (closV 'a (multE (idE 'a) (idE 'a)) mt-env)(numV 123)) "not a number")

(test/exn (checkNum (thunkV (multE (numE 999)(idE 'x)) (list (bind 'x (numV 999)))) (boolV #t)) "not a number")




(define (helperRemove [s : Symbol] [env : Env]) : (Listof Binding)
  (type-case (Listof Binding) env
    [empty empty]
    [(cons b r) (cond
                   [(symbol=? s (bind-name b))r]
                   [else (cons b (helperRemove s r))])]))

;; helperRemove TESTS ----
(test (helperRemove 'x mt-env)empty)


(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse `{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

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


;; TESTS ----------------------------------------
(test (interp (parse `{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse `7)
                mt-env))
(test (interp (parse `{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse `9)
                mt-env))
(test (interp (parse `{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse `10)
                mt-env))
(test/exn (interp (parse `{if 1 2 3})mt-env)"not a boolean")


(test/exn (interp (parse `{let {[x 1]}{unlet x x}})mt-env)"free variable")

(test (interp (parse `{let {[x 1]}{+ x {unlet x 1}}}) mt-env)(interp (parse `2) mt-env))

(test (interp (parse `{let {[x 1]}{let {[x 2]}{+ x {unlet x x}}}})mt-env)
        (interp (parse `3) mt-env))

(test (interp (parse `{let {[x 1]}{let {[x 2]}{let {[z 3]}
                              {+ x {unlet x {+ x z}}}}}})mt-env)
      (interp (parse `6) mt-env))

(test (interp (parse `{let {[f {lambda {z}{let {[z 8]}{unlet z z}}}]}{f 2}}) mt-env)(interp (parse `2) mt-env))

(test/exn (interp (parse `{force 1}) mt-env) "not a thunk")

(test (interp (parse `{force {if {= 8 8} {delay 7} {delay 9}}}) mt-env)
        (interp (parse `7) mt-env))

(test (interp (parse `{let {[d {let {[y 8]}{delay {+ y 7}}}]}{let {[y 9]}{force d}}})mt-env)
        (interp (parse `15)mt-env))