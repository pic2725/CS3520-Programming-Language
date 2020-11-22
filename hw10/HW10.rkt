#lang plait

(define-type Value
  (numV [n : Number])
  (closV [arg : (Listof Symbol)]  ;; list
         [body : Exp]
         [env : Env])
  (boolV [bool : Boolean])

  (pairV [first : Value]
         [second : Value])

  )

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (boolE [b : Boolean])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  
  (lamE [listSym : (Listof Symbol)] ;; symbol list
        [arg-types : (Listof Type)] ;; type list
        [body : Exp])
  
  (appE [fun : Exp]
        [listArgs : (Listof Exp)]) ;; list
  
  (equalE [first : Exp]
          [second : Exp])
  
  (ifE [exp : Exp]
       [first : Exp]
       [second : Exp])
  
  (pairE [first : Exp]
         [second : Exp])
  
  (fstE [exp : Exp])
  
  (sndE [exp : Exp]))

(define-type Type
  (numT)
  (boolT)
  
  (arrowT [listArgs : (Listof Type)]   ;; make it list
          [result : Type])
  
  (crossT [l : Type]
          [r : Type]))

(define-type Binding
  (bind [name : Symbol]
        [val : Value]))

(define-type-alias Env (Listof Binding))

(define-type Type-Binding
  (tbind [name : Symbol]
         [type : Type]))

(define-type-alias Type-Env (Listof Type-Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors #t))

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (cond [(equal? `true s) (boolE #t)]
           [(equal? `false s) (boolE #f)]
           [else (idE (s-exp->symbol s))])]
    
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appE (lamE (list (s-exp->symbol (first bs)))   ;; make it list
                   (list (parse-type (third bs)))      ;; make it list
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]   ;; list
    
    [(s-exp-match? `{lambda {[SYMBOL : ANY]...} ANY} s)
     ;;listArgs, symobl, and type
     (let ([listArgs (s-exp->list (second (s-exp->list s)))] 
           [storeSymbol (lambda (lam)
                         (s-exp->symbol (first (s-exp->list lam))))]
           [storeType (lambda (lam)
                       (parse-type (third (s-exp->list lam))))])

       ;; call lamE
       (lamE (map storeSymbol listArgs)
             (map storeType listArgs)
             (parse (third (s-exp->list s)))))]

    [(s-exp-match? `{pair ANY ANY} s)
     (pairE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{fst ANY} s)
     (fstE (parse (second (s-exp->list s))))]
    
    [(s-exp-match? `{snd ANY} s)
     (sndE (parse (second (s-exp->list s))))]
    
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? `{= ANY ANY} s)
     (equalE (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))

(define (parse-type [s : S-Exp]) : Type
  (cond
   [(s-exp-match? `num s) 
    (numT)]
   [(s-exp-match? `bool s)
    (boolT)]

  
   [(s-exp-match? `(ANY ... -> ANY) s)
     (arrowT (map parse-type
                  (reverse (rest   ;; reverse and take two
                            (rest
                             (reverse ;; reverse back
                              (s-exp->list s))))))  
             
             (parse-type
              (first (reverse (s-exp->list s)))))]

   
    [(s-exp-match? `(ANY * ANY) s)
          (crossT (parse-type
              (first (s-exp->list s)))
             (parse-type
              (third (s-exp->list s))))]
    
    [else (error 'parse-type "invalid input")]))

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
  (test (parse `{let {[x : num {+ 1 2}]}
                  y})
        (appE (lamE (list 'x) (list (numT)) (idE 'y))
              (list (plusE (numE 1) (numE 2)))))
  (test (parse `{lambda {[x : num]} 9})
        (lamE (list 'x) (list (numT)) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (list (numE 9))))
  (test/exn (parse `{})
            "invalid input")

  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse-type `1)
            "invalid input")
  (test (parse-type `(num * bool))
        (crossT (numT) (boolT))))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    
    [(lamE n t body)
     (closV n body env)]
    
    [(boolE bool) (boolV bool)]
    
    [(equalE first second)
     (cond
       [(and (numV? (interp first env))
             (numV? (interp second env))) ;; both are numV?
        
        (boolV (= (numV-n(interp first env)) (numV-n(interp second env))))])]

       
    [(ifE exp first second)
     (type-case Value (interp exp env)
       [(boolV bool) (if bool                 ;;
                         (interp first env)
                         (interp second env))]
       [else (error 'interp "not a boolean")])]
      
    [(pairE first second)
     (pairV
      (interp first env)
      (interp second env))]
    
    [(fstE first)
     (type-case Value (interp first env)
       [(pairV first second) first]
       [else (error 'interp "not a pair")])]
    
    [(sndE second)
     (type-case Value (interp second env)
       [(pairV first second) second]
       [else (error 'interp "not a pair")])]
    
    [(appE fun args)
     (type-case Value (interp fun env)
       [(closV listSym body c-env)
        (interp body
                (append (map2 bind listSym       ;; mapping
                             (map (lambda (arg) (interp arg env)) args))c-env))]
       [else (error 'interp "not a function")])]))




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
  (test (interp (parse `{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  )

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
(define (make-lookup [name-of : ('a -> Symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : Symbol] [vals : (Listof 'a)]) : 'b
    (type-case (Listof 'a) vals
      [empty (error 'find "free variable")]
      [(cons val rst-vals) (if (equal? name (name-of val))
                               (val-of (first vals))
                               ((make-lookup name-of val-of) name rst-vals))])))

(define lookup
  (make-lookup bind-name bind-val))

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

;; typecheck ----------------------------------------
(define (typecheck [a : Exp] [tenv : Type-Env])
   (type-case Exp a
    [(numE n) (numT)]
    [(boolE b) (boolT)]
     
    [(plusE l r) (typecheck-nums l r tenv)]
     
    [(multE l r) (typecheck-nums l r tenv)]
     
    [(idE n) (type-lookup n tenv)]
     
    [(equalE first second)
     (begin (typecheck-nums first second tenv) (boolT))]
     
    [(ifE exp first second)
     (type-case Type (typecheck exp tenv)
       [(boolT)
        (typecheck second tenv)]
       [else (type-error exp "boolean")])]
     

    [(pairE first second)
     (crossT
      (typecheck first tenv)
      (typecheck second tenv))]

     
    [(fstE exp)
     (type-case Type (typecheck exp tenv)
       [(crossT first second) first]
       [else (type-error exp "pair")])]

     
    [(sndE exp)
     (type-case Type (typecheck exp tenv)
       [(crossT first second) second]
       [else (type-error exp "pair")])]
     
    [(lamE listSym arg-types body)
     (arrowT arg-types
             (typecheck body
                        (append (map2 tbind listSym arg-types) tenv)))]
     
    [(appE fun args)
     (type-case Type (typecheck fun tenv)
       [(arrowT arg-types result-type)
        (begin
          (recursiveCheck (map (lambda (arg) (typecheck arg tenv)) args) args  tenv)
          result-type)]
       [else (type-error fun "function")])]))



(define (recursiveCheck [listType : (Listof Type)] [listArg : (Listof Exp)] tenv) : Boolean
  
  (if (= (length listArg)
         (length listType)) ;; check both size first
      (type-case (Listof Type) listType
        [(cons fst snd)
         (and
          (equal? (typecheck (first listArg) tenv) fst)
          (recursiveCheck snd (rest listArg) tenv))] ;; go through the list
        [empty #t])
      (error 'interp "failed")))


(define (typecheck-nums l r tenv)
  (type-case Type (typecheck l tenv)
    [(numT)
     (type-case Type (typecheck r tenv)
       [(numT) (numT)]
       [else (type-error r "num")])]
    [else (type-error l "num")]))


(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))


(define type-lookup
  (make-lookup tbind-name tbind-type))


(module+ test
  (test (typecheck (parse `10) mt-env)
        (numT))
  (test (typecheck (parse `{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse `{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))

  (test (typecheck (parse `{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))

  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))

  (test/exn (typecheck (parse `{1 2})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse `{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type")
  
  (test (interp (parse `{if true 4 5})
                mt-env)
         (numV 4))
  
  (test (interp (parse `{if false 4 5})
                mt-env)
         (numV 5))
  
  (test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
         (numV 5))
  (test (interp (parse `{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
        (numV 5))
  
  (test (typecheck (parse `{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
         (boolT))
  
  (test (typecheck (parse `{if {= 1 {+ -1 2}}
                               {lambda {[x : num]} {+ x 1}}
                               {lambda {[y : num]} y}})
                   mt-env)
        ;; This result may need to be adjusted after part 3:
        ;;chaned to list
        (arrowT (list (numT)) (numT)))
  
  (test/exn (typecheck (parse `{+ 1 {if true true false}})
                       mt-env)
            "no type")

  (test (interp (parse `{pair 10 8})
                mt-env)
        ;; Your constructor might be different than pairV:
        (pairV (numV 10) (numV 8)))
  
  (test (interp (parse `{fst {pair 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{snd {pair 10 8}})
                mt-env)
        (numV 8))
  
  (test (interp (parse `{let {[p : (num * num) {pair 10 8}]}
                          {fst p}})
                mt-env)
        (numV 10))
  
  (test (typecheck (parse `{pair 10 8})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (crossT (numT) (numT)))
  
  (test (typecheck (parse `{fst {pair 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{+ 1 {snd {pair 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{lambda {[x : (num * bool)]}
                             {fst x}})
                   mt-env)
        ;; Your constructor might be different than crossT:
        (arrowT (list (crossT (numT) (boolT))) (numT)))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {fst x}}
                            {pair 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{{lambda {[x : (num * bool)]}
                              {snd x}}
                            {pair 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse `{fst 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{+ 1 {fst {pair false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse `{lambda {[x : (num * bool)]}
                                 {if {fst x}
                                     1
                                     2}})
                       mt-env)
            "no type")

  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")

  (test/exn (interp (parse `{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")

  (test/exn (interp (ifE (numE 111) (numE 222) (numE 333))
                    mt-env)
            "not a boolean")

  (test/exn (interp (parse `{fst 0}) mt-env)
            "not a pair")
  
  (test/exn (interp (parse `{snd 0}) mt-env)
            "not a pair")

  
  (test/exn (typecheck (parse `{snd 1})
                       mt-env)
            "no type")

  ;; multi args

  (test (interp (parse `{{lambda {}
                           10}})
                mt-env)
        (numV 10))
  
  (test (interp (parse `{{lambda {[x : num] [y : num]} {+ x y}}
                         10
                         20})
                mt-env)
        (numV 30))
  
  
  (test (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
                            10
                            false})
                   mt-env)
        (boolT))
  
;;  (test/exn (typecheck (parse `{{lambda {[x : num] [y : bool]} y}
;;                                false
;;                                10})
;;                       mt-env)
;;            "no type")
  
  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (num num -> num)
                                      {lambda {[y : num] [z : num]}
                                        {+ z y}}]}
                               {f x x}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse `{let {[x : num 4]}
                             {let {[f : (bool num -> num)
                                      {lambda {[sel : bool] [z : num]}
                                        {if sel x z}}]}
                               {f {= x 5} 0}}})
                   mt-env)
        (numT))

 
  )