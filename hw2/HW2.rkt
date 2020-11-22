#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [arg : (Listof Exp)])
  (maxE [l : Exp]
        [r : Exp]))


(define-type Func-Defn
  (fd [name : Symbol] 
      [listArgs : (Listof Symbol)] 
      [body : Exp]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP)

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

;; parse ----------------------------------------
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? `{max ANY ANY} s) (maxE
                                      (parse (second (s-exp->list s)))
                                      (parse (third (s-exp->list s))))]

    
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]

    
    [(s-exp-match? `{SYMBOL ANY ...} s)(appE
                                        (s-exp->symbol (first (s-exp->list s)))
                                        (map parse (rest (s-exp->list s))))]

    
    [else (error 'parse "invalid input")]))



(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
       
     (fd (s-exp->symbol
          (first (s-exp->list (second (s-exp->list s)))))
          (tempArg s)
     (parse (third (s-exp->list s))))]
    
    [else (error 'parse-fundef "invalid input")]))


(define (tempArg [s : S-Exp])
  (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))))


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
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
 
  (test (parse `{max 1 5})
        (maxE (numE 1) (numE 5)))
  (test (parse `{max 5 1})
        (maxE (numE 5) (numE 1)))
        
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")
  
  


  (define area-def
    (parse-fundef `{define {area w h} {* w h}}))
  
  (define five-def
    (parse-fundef `{define {five} 5}))
  
  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(maxE l r) (max (interp l defs) (interp r defs))]
    [(appE s args) (local [(define fd (get-fundef s defs))]
                     (if (= (length (fd-listArgs fd)) (length args))
                         (interp
                          (tempLists (map numE (map (lambda (arg) (interp arg defs)) args))
                                             (fd-listArgs fd)
                                             (fd-body fd))
                                 defs)
                         (error 'interp "wrong arity")
                         ))]))


(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)


  (test (interp (parse `{s 1 2})
                (list (parse-fundef `{define {s x y } {max x y} })))
        2)
  (test (interp (parse `{s 1 2 3})
                (list (parse-fundef `{define {s x y z} {* {max x y} z}})))
        6))


;; get-fundef ----------------------------------------

(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : Exp] [for : Symbol] [in : Exp]) : Exp
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    
    [(appE s args) (appE s (map (lambda (arg) (subst what for arg)) args))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8}))

  (test (subst (parse `2) 'y (parse `{max 4 y}))
        (parse `{max 4 2}))
  (test (subst (parse `3) 'y (parse `{max x y}))
        (parse `{max x 3}))
  
  (test (subst (parse `2) 'h (parse `{area w h}))
        (parse `{area w 2}))
  (test (subst (parse `1) 'h (parse `{area w h}))
        (parse `{area w 1}))
  (test (subst (parse `4) 'h (parse `{area 3 h}))
        (parse `{area 3 4})))

(define (tempLists [what : (Listof Exp)] [for : (Listof Symbol)] [in : Exp]) : Exp
  (type-case (Listof Exp) what
    [empty in]
    [(cons left right)
     (type-case Exp in
       [(numE n) in]
       [(idE s) (if (eq? (first for) s)
                    left
                    (tempLists right (rest for) (subst left (first for) in)))]
       [(plusE l r)
        (plusE (tempLists right (rest for) (subst left (first for) l)) (tempLists right (rest for) (subst left (first for) r)))]
       
       [(multE l r)
        (multE (tempLists right (rest for) (subst left (first for) l)) (tempLists right (rest for) (subst left (first for) r)))]
       
       [(maxE l r)
        (maxE (tempLists right (rest for) (subst left (first for) l)) (tempLists right (rest for) (subst left (first for) r)))]
       
       [(appE s args)
        (appE s (map (lambda (arg) (tempLists right (rest for) (subst left (first for) arg))) args))])]))



  (test (tempLists (list (parse `1) (parse `2)) (list 'a 'b) (parse `{area a b}))
        (parse `{area 1 2}))

  (test (tempLists (list) (list) (parse `{three}))
        (parse `{three}))

  (test (tempLists (list (parse `3)) (list 'x) (parse `{double x}))
        (parse `{double 3}))





  (test (interp (parse `{f 1 2})
                (list (parse-fundef `{define {f x y} {+ x y}})))
        3)
  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test (interp (parse `{f 2 3})
                (list (parse-fundef `{define {f x y} {+ y {+ x x}}})))
        7)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity")
