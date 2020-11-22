#lang plait

(define-type Value
  (numV [n : Number])
  (closV [args : (Listof Symbol)]
         [body : Exp]
         [env : Env])
  (contV [k : Cont]))

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (lamE [ns : (Listof Symbol)]
        [body : Exp])
  (appE [fun : Exp]
        [args : (Listof Exp)])
  (let/ccE [n : Symbol]
           [body : Exp])

  (if0E [e : Exp]
        [thn : Exp]
        [els : Exp])
           
  (negE [e : Exp])
  
  (avgE [e1 : Exp]
        [e2 : Exp]
        [e3 : Exp])

  )

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  (doneK)
  (plusSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doPlusK [v : Value]
           [k : Cont])
  (multSecondK [r : Exp]
               [e : Env]
               [k : Cont])
  (doMultK [v : Value]
           [k : Cont])
  
  (appArgK [vs : (Optionof Value)] ;;[vs : (Listof Value)]
           [listValues : (Listof Value)]
           [args : (Listof Exp)]
           [env : Env]
           [k : Cont])
  
  (doAppK [v : Value]
          [listValues : (Listof Value)]
          [k : Cont])
  
  (doNegK [k : Cont])

  (doAvgK [firstAvg : Value]
          [secondAvg : Value]
          [k : Cont])
  
  (secondAvg  [second : Exp]
              [third : Exp]
              [e : Env]
              [k : Cont])
  
  (thirdAvg  [v : Value]
             [third : Exp]
             [e : Env]
             [k : Cont])

  (doIf0K [k : Cont])

  (subIf0 [thn : Exp]
          [els : Exp]
          [e : Env]
          [k : Cont])

  )

(module+ test
  (print-only-errors #t))


(define (interp-expr [e : Exp]) : S-Exp
  (type-case Value (interp e mt-env (doneK))
    [(numV n) (number->s-exp n)]
    [(contV l) `function]
    [(closV arg body env) `function]
    ))

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
       (appE (lamE (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]
    
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s) ;;placeholde another exp
     (lamE (map s-exp->symbol (s-exp->list 
                               (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{let/cc SYMBOL ANY} s)
     (let/ccE (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    
    
    [(s-exp-match? `{neg ANY} s)
     (negE (parse (second (s-exp->list s))))]
    
    [(s-exp-match? `{avg ANY ANY ANY} s)
     (avgE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    

    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]

  
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
   (test (parse `{neg 1})
        (negE (numE 1)))
  (test (parse `x) ; note: backquote instead of normal quote
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
        (appE (lamE (list 'x) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{let/cc k 0})
        (let/ccE 'k (numE 0)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input"))


;; interp & continue ----------------------------------------
(define (interp [a : Exp] [env : Env] [k : Cont]) : Value
  (type-case Exp a
    [(numE n) (continue k (numV n))]
    
    [(idE s) (lookup s env k)]
    
    [(plusE l r) (interp l env
                         (plusSecondK r env k))]
    [(multE l r) (interp l env
                         (multSecondK r env k))]
    [(lamE ns body)
     (continue k (closV ns body env))]
    
    [(appE fun args) (interp fun env
                             (appArgK (none) empty args env k))] ;;(appArgK empty empty args env k))
    ;;
    [(let/ccE n body)
     [type-case Exp body
       ;;working
       [else (interp body (extend-env
                   (bind n (contV k))
                   env) k)]]]

    
    ;;
    ;;(interp body (extend-env
    ;;               (bind n (contV k))
    ;;               env) k)]
    
    
    [(negE e) (interp e env(doNegK k))]
    
    [(avgE e1 e2 e3) (interp e1 env (secondAvg e2 e3 env k))]
    
    [(if0E e thn els) (interp e env (subIf0 thn els env k))]

    ))

(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [(doneK) v]

    [(plusSecondK r env next-k)
     (interp r env
             (doPlusK v next-k))]
    
    [(doPlusK v-l next-k)
     (num+ v-l v next-k)]
    
    [(multSecondK r env next-k)
     (interp r env
             (doMultK v next-k))]
    
    [(doMultK v-l next-k)
     (num* v-l v next-k)]

    [(doNegK next-k)
     (type-case Value v
       [(numV num) (continue next-k (numV (* -1 num)))]
       [(closV ns body env) (error 'interp "not a number")]
       [(contV v) (error 'interp "not a number")])]
    
    [(secondAvg second third env next-k)
     (interp second env(thirdAvg v third env next-k))]
    
    [(thirdAvg v1 thd env next-k)
     (interp thd env (doAvgK v1 v next-k))]
    
    [(doAvgK v1 v2 next-k)
     (num-op /(num+ (num+ v1 v2 next-k) v next-k)(numV 3)next-k)]
    
    [(subIf0 thn els env next-k)
     (type-case Value v
       [(numV n) (if0Helper n thn els env next-k)]
       [(closV ns body env) (error 'interp "not a number")]
       [(contV l) (error 'interp "not a number")])]
           
    [(doIf0K next-k)
     (continue next-k v)]
    
    
    [(appArgK vs listValues args env next-k)
     (type-case (Listof Exp) args
       [empty (if (none? vs) ;;args empty and vs is none then continue
                  (continue (doAppK v listValues next-k) v)
                  (continue (doAppK (some-v vs) (cons v listValues) next-k) v))]
       
       [(cons first rest) (if (none? vs)
                              (interp first env (appArgK (some v) empty rest
                                                         env next-k))
                              
                              (interp first env(appArgK vs (cons v listValues) rest
                                                        env next-k)))])]
    ;; some : ('a -> (Optionof 'a))
    ;; some-v : ((Optionof 'a) -> 'a)

    
    [(doAppK v1 listValues next-k)
     (type-case Value v1
       [(numV n) (error 'interp "not a function")]
       [(contV cv) (continue cv v)]
       [(closV ns body c-env) (if (eq? (length listValues) 0)
                                  (interp body c-env next-k )
                                  
                                  (interp body (extend-env* ;;reverse ns to bind with listValues
                                                (map2 bind (reverse ns) listValues) c-env)
                                          next-k))]

       )]
       

    ))

(define (if0Helper [n : Number] [thn : Exp] [els : Exp] [e : Env] [k : Cont])
  (if (eq? n 0)
      (interp thn e(doIf0K  k))
      
      (interp els e(doIf0K  k))))

(module+ test
  (test (interp (parse `2) mt-env (doneK))
        (numV 2))
  
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  
  (test (interp (parse `{+ 2 1}) mt-env (doneK))
        (numV 3))
  
  (test (interp (parse `{* 2 1}) mt-env (doneK))
        (numV 2))
  
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))

  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test (interp (parse `{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK))
        (numV 0))
  (test (interp (parse `{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK))
        (numV 10))

  (test/exn (interp (parse `{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  ;; Eager:
  (test/exn (interp (parse `{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (plusSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doPlusK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numE 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35))
  (test (continue (appArgK (none) empty (list (numE 5)) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env))
        (numV 5))
  (test (continue (doAppK (closV (list 'x) (idE 'x) mt-env) (list (numV 8)) (doneK)) (numV 8))
        (numV 8))

  (test/exn (continue (doNegK (doneK)) (contV (doneK))) "not a number")
  (test/exn (continue (doNegK (doneK)) (closV (list 'x) (idE 'x) mt-env))"not a number")
  (test/exn (continue (subIf0 (numE 1) (numE 2) mt-env (doneK)) (contV (doneK)))"not a number")
  (test/exn (continue (subIf0 (numE 1) (numE 2) mt-env (doneK)) (closV (list 'x) (idE 'x) mt-env))"not a number")
  (test (interp-expr (parse (quasiquote (neg ((lambda (x y) (+ x y)) 1 2))))) `-3)
  

  ;;given test
  (test (interp-expr (parse `{neg 2}))
        `-2)
  (test (interp-expr (parse `{avg 0 6 6}))
        `4)
  (test (interp-expr (parse `{let/cc k {neg {k 3}}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg 0 {k 3} 0}}))
        `3)
  (test (interp-expr (parse `{let/cc k {avg {k 2} {k 3} 0}}))
        `2)
  (test (interp-expr (parse `{if0 1 2 3}))
        `3)
  (test (interp-expr (parse `{if0 0 2 3}))
        `2)
  (test (interp-expr (parse `{let/cc k {if0 {k 9} 2 3}}))
        `9)
  (test (interp-expr (parse `{{lambda {x y} {+ y {neg x}}} 10 12}))
        `2)
  (test (interp-expr (parse `{lambda {} 12}))
        `function)
  (test (interp-expr (parse `{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse `{{{lambda {x} {lambda {} x}} 13}}))
        `13)

  (test (interp-expr (parse `{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        `3)
  (test (interp-expr (parse `{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        `20)

  (test (interp-expr (parse `{let/cc esc esc})) `function)
  
  ;;(test/exn (interp-expr (parse `{let/cc esc {esc}}))
            ;; error because continuation is given 0 arguments,
            ;; but the specific error message is not specified
  ;;          "")
  ;;(test/exn (interp-expr (parse `{let/cc esc {esc 1 2}}))
            ;; error because continuation is given 2 arguments
  ;;          "")
  
  )

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value] [k : Cont]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))

(define (num+ [l : Value] [r : Value] [k : Cont]) : Value
  (num-op + l r k))

(define (num* [l : Value] [r : Value] [k : Cont]) : Value
  (num-op * l r k))


(module+ test
  (test (num+ (numV 1) (numV 2) (doneK))
        (numV 3))
  (test (num* (numV 2) (numV 3) (doneK))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env] [k : Cont]) : Value
  (type-case (Listof Binding) env
    [empty (error 'interp "free variable")]
    [(cons b rst-env) (cond
                        [(symbol=? n (bind-name b)) (continue k (bind-val b))] 
                        [else (lookup n rst-env k)])]))

(module+ test
  (test/exn (lookup 'x mt-env (doneK))
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env) (doneK))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)) (doneK))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)) (doneK))
        (numV 8)))