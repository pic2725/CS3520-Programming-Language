#lang plait

#|part-1|#
(define (3rd-power q) : Number (* (* q q) q))

(test (3rd-power 17) 4913)
(test (3rd-power 2) 8)

;part-2
(define (6nd-power q) : Number
  (* (3rd-power q) (3rd-power q)))

(define (12nd-power q) : Number
  (* (6nd-power q) (6nd-power q)))

(define (24nd-power q) : Number
  (* (12nd-power q) (12nd-power q)))

(define (36nd-power q) : Number
  (* (24nd-power q) (12nd-power q)))


(define (42nd-power q) : Number
  (* (36nd-power q) (6nd-power q)))

(test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)
(test (42nd-power 2) 4398046511104)


  
;part-3
(define (plural s) : String
  (cond
    [(equal? (substring s (- (string-length s) 1) (string-length s)) "y")
     (string-append (substring s 0 (- (string-length s) 1)) "ies")]
    [else (string-append s "s")]
    
    )
  )

(test (plural "baby") "babies")
(test (plural "fish") "fishs")
(test (plural " ") " s")
(test (plural "bybybyby") "bybybybies")
(test (plural "bybybyb") "bybybybs")

;part-4
(define-type Light
    (bulb [watts : Number]
          [technology : Symbol])
    (candle [inches : Number]))

(define (energy-usage [e : Light])
  (type-case Light e
    [(bulb w t) (* w 0.024)]
    [(candle i) (* i 0)]
  )
 )

(test (energy-usage (bulb 100.0 'halogen)) 2.4)
(test (energy-usage (candle 10.0)) 0)



;part-5

(define (use-for-one-hour [u : Light])
  (type-case Light u
    [(bulb w t) (bulb w t)]
    [(candle i) (candle (- i 1))]
  ))

  
    
(test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
(test (use-for-one-hour (candle 10.0)) (candle 9.0))


  
