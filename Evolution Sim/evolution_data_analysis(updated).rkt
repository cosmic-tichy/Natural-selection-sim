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

(define-struct traits [speed sight size health gene])


;A gene is a Genomic code (String) used for tracking offspring of the form
; - (letter [A-Z]) (Number [1-1000])
; - Examples: F20, A45, B67
; - Non-examples: 20Z, 500A, A2000

;A fertility is a Real Number 1 - 1000

;K = Super Gene



; a sim is a (make-sim pworld traits)

(define-struct sim [pworld traits])


(define all-genes (list "A" "B" "C" "D" "E" "F"))

;a sims is a ListOfSim

;a field is one of
; - Food
; - Food ListOfFood

;a world is a (make-world field sims)

;sim sight = size * sight = radius of sight perimeter

(define-struct world [phase field sims dead])

(define-struct phase [timer])



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


;surviving-plot: World -> Plot
;plots number of surviving genes

(define (surviving-plot w)
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


;A Generation is an integer in String form

;get-gen: Integer World -> List Of Traits
;gets all N gen gene traits from living and dead

(define (get-gen gen w)
  (append (filter (λ (s) (cond [(string-contains? (traits-gene(sim-traits s)) "M")
                                (= (+ (string->number (substring(first(string-split (traits-gene(sim-traits s)) "M")) 1))
                                      (string->number (first(rest(string-split (traits-gene(sim-traits s)) "M")))))
                                   gen)]
                               [else (= (string->number (substring (traits-gene(sim-traits s)) 1)) gen)])) (world-sims w))
          (filter (λ (s) (cond [(string-contains? (traits-gene(sim-traits s)) "M")
                                (= (+ (string->number (substring(first(string-split (traits-gene(sim-traits s)) "M")) 1))
                                      (string->number (first(rest(string-split (traits-gene(sim-traits s)) "M")))))
                                   gen)]
                               [else (= (string->number (substring (traits-gene(sim-traits s)) 1)) gen)])) (world-dead w))))
          
      

;get-trait(ABSTRACTION) : Generation [Traits Selector Function] World -> Pair
;pairs given trait of given Generation [traits selector] with gene letter

(define (get-trait gen trait w)
  (map (λ (s) (list (substring (traits-gene(sim-traits s)) 0 1)
                    (trait (sim-traits s)))) (get-gen gen w)))

(define (get-sight gen w)
  (get-trait gen (λ (t) (traits-sight t)) w))

(define (get-speed gen w)
  (get-trait gen (λ (t) (traits-speed t)) w))

(define (get-size gen w)
  (get-trait gen (λ (t) (traits-size t)) w))

(define (get-health gen w)
  (get-trait gen (λ (t) (traits-health t)) w))



;count-gen: Number World -> Number
;counts how many sims are in given generation

(define (count-gen gen w)
  (length (get-gen gen w)))

;num-generations: Number World -> ListofPairs
;takes the number of generations which have lived and the world and gives
;list of pairs (generation numberOFoffspring) 

(define (num-generations n w)
  (map (λ (g) (list g (count-gen g w))) (build-list n (λ (x) (+ x 1)))))



;highest-pop: World -> Number
;returns highest population reached

(define (highest-pop lop)
  (cond [(empty? lop) 0]
        [else (max (first (cdr (first lop)))
                   (highest-pop (rest lop)))]))

;highest-gen; World -> Number
;returns highest generation reached
;
;(define (highest-gen w)
;  (cond [(empty? lop) 0]
;        [else (max (first (car (first lop)))
;                   (highest-pop (rest lop)))]))






;a TraitPair is a list of Pairs of the form ("A" 10) ("E" 7).... etc

;sum-trait: ListOf TraitPairs -> Number
;sums given trait

(define (sum-trait lop)
  (cond [(empty? lop) 0]
        [else (+ (first(cdr(first lop)))
                 (sum-trait (rest lop)))]))



;TRAIT-average: Number World -> Number
;averages the given traits for a given generation

(define (sight-average gen w)
  (/(sum-trait(get-sight gen w))
    (length (get-sight gen w))))

(define (speed-average gen w)
  (/(sum-trait(get-speed gen w))
    (length (get-speed gen w))))

(define (size-average gen w)
  (/(sum-trait(get-size gen w))
    (length (get-size gen w))))

(define (health-average gen w)
  (/(sum-trait(get-health gen w))
    (length (get-health gen w))))

;averages: Number TRAIT-average World -> ListOfPairs
;gives list of all averages of given trait each generation up to N

(define (averages n av w)
  (map (λ (g) (list g (av g w))) (build-list n (λ (x) (+ x 1)))))

;sight-averages

(define (sight-averages n w)
  (map (λ (g) (list g (sight-average g w))) (build-list n (λ (x) (+ x 1)))))

;speed-averages

(define (speed-averages n w)
  (map (λ (g) (list g (speed-average g w))) (build-list n (λ (x) (+ x 1)))))
;size-averages

(define (size-averages n w)
  (map (λ (g) (list g (size-average g w))) (build-list n (λ (x) (+ x 1)))))

;health-averages

(define (health-averages n w)
  (map (λ (g) (list g (health-average g w))) (build-list n (λ (x) (+ x 1)))))

;an Average is a Pair of the form (1 10.4)

;plot-averages: Number World -> Plot
;plots given max generation and world and returns plot of averages
;;;;;;;;;;; n = Number of generations lived

;;;;;;;;;;;;;;;;;; PLOTTING INSTRUCTIONS: First run (num-generations n w) with arbitrarily large number as n, until some pairs have 0

;plot-generations: Number World -> Plot
;plots the number of sims in each generation

(define (plot-generations n w)
  (plot (discrete-histogram (num-generations n w)
                            #:label "Number of NPC's"
                             #:y-max (+(highest-pop(num-generations n w))5)
                             #:color "Green"
                             )))


(define (plot-sight n w)
  (plot (discrete-histogram (sight-averages n w)
                            #:label "Sight Average"
                            #:color "Yellow"
                            #:y-max 17)))

(define (plot-speed n w)
  (plot (discrete-histogram (speed-averages n w)
                            #:label "Speed Average"
                            #:color "DarkOrange"
                            #:y-max 14
                            )))


(define (plot-size n w)
  (plot (discrete-histogram (size-averages n w)
                            #:label "Size Average"
                            #:color "SteelBlue"
                            #:y-max 14
                           )))

(define (plot-health n w)
  (plot (discrete-histogram (health-averages n w)
                            #:label "Health Average"
                            #:color "Crimson"
                            #:y-max 85
                            )))