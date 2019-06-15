#lang racket
(require plot)
(plot-new-window? true)
(require posn)
(require racket/include)
(require racket/shared)
(require 2htdp/universe)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS and CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;A Pworld is a (make-pworld p goal)

(define-struct pworld [p goal])

;a Food is a (make-posn Number Number)


;a traits is a (make-traits Number Number Number Number)

;speed is a Number 1 - 5

;sight is a Number 1 - 7
; - pixel sight = sight-trait * 10

;size is a Number 1 - 10

;health is a Number 1 - 100

(define-struct traits [speed sight size health fertility gene])


;A gene is a Genomic code (String) used for tracking offspring of the form
; - (letter [A-Z]) (Number [1-1000])
; - Examples: F20, A45, B67
; - Non-examples: 20Z, 500A, A2000

;A fertility is a Real Number 1 - 1000

;K = Super Gene


(define A (make-traits 5 1 2 100 1 "A1"))
(define B (make-traits 4 2 4 100 1 "B1"))
(define C (make-traits 3 3 6 100 1 "C1"))
(define D (make-traits 2 4 8 100 1 "D1"))
(define E (make-traits 1 5 10 100 1 "E1"))
(define F (make-traits 5 3 2 100 1 "F1"))
(define G (make-traits 4 7 4 100 1 "G1"))
(define H (make-traits 6 4 6 100 1 "H1"))
(define I (make-traits 3 8 8 100 1 "I1"))
(define J (make-traits 5 6 10 100 1 "J1"))
(define K (make-traits 4 7 10 100 1 "K1"))

; a sim is a (make-sim pworld traits)

(define-struct sim [pworld traits])


(define all-genes (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"))

;a sims is a ListOfSim

;a field is one of
; - Food
; - Food ListOfFood

;a world is a (make-world field sims)

;sim sight = size * sight = radius of sight perimeter

(define-struct world [field sims])




(define-struct posn [x y])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PLOTTING FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Sims -> Traits

(define (retrieve-traits w)
  (map (λ (s) (sim-traits s)) (world-sims w)))

;count-genes: ListOfTraits 1String -> Number
;counts number of given gene codes

(define (count-genes lot gene)
  (foldr (λ (t y) (cond [(string=? (substring (traits-gene t) 0 1) gene)
                         (+ 1 y)]
                        [else (+ 0 y)])) 0 lot))

;gene-plot: ListOfTraits -> List of List
;makes trait data plot readable

(define (gene-plot lot genes)
  (map (λ (g) (list g (count-genes lot g))) genes ))

;surviving-genes: ListOfTraits -> Plot
(define (surviving-genes lot)
  (plot (discrete-histogram (gene-plot lot all-genes)
                            #:label "Surviving Genes")))


;plot-genes: World -> Plot
;plots number of surviving genes

(define (plot-genes w)
  (surviving-genes(retrieve-traits w)))


;a Health is a (list 1String Number)

;retrieve-health: LisOfTraits -> ListOfHealth
;returns list of health values
(define (retrieve-health lot)
  (map (λ (t) (list (substring (traits-gene t) 0 1 ) (traits-health t))) lot))



;average-health: World 1String
(define (average-health w g)
  (/(total-health (retrieve-health(retrieve-traits w)) g) (count-genes (retrieve-traits w) g)))

(define (all-average w)
  (map (λ (g) (list g (average-health w g))) (filter (λ (g) (>= (count-genes (retrieve-traits w) g) 1))
                                            all-genes)))
          

(define (total-health loh g)
  (foldr (λ (h y) (cond [(string=? g (first h))
                       (+ (first(rest h)) y)]
                        [else (+ 0 y)])) 0 loh))

                              

(define (health-plot w)
  (plot (discrete-histogram (all-average w)
                            #:label "Average Health")))


                    