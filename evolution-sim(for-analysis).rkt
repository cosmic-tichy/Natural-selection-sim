#lang racket
(require plot)
(plot-new-window? true)

(require racket/string)
(require (prefix-in htdp: 2htdp/image))
(require 2htdp/universe)
(require 2htdp/batch-io)
(require plot/no-gui)
(require racket/serialize)
(require racket/include)

(require (file "replicate-sim.rkt"))

(provide (all-defined-out))



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

;surviving-genes: World -> Plot
(define (surviving-genes w)
  (plot (discrete-histogram (gene-plot (retrieve-traits w) all-genes)
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

;get-gen: Integer World -> List Of Sims
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

;alive-gen Integer World -> List Of Sims
;gets all surviving sims in given generation

(define (alive-gen gen w)
   (filter (λ (s) (cond [(string-contains? (traits-gene(sim-traits s)) "M")
                                (= (+ (string->number (substring(first(string-split (traits-gene(sim-traits s)) "M")) 1))
                                      (string->number (first(rest(string-split (traits-gene(sim-traits s)) "M")))))
                                   gen)]
                               [else (= (string->number (substring (traits-gene(sim-traits s)) 1)) gen)])) (world-sims w)))
          
      

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PER GENE ANALYTICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;l



;single-gene: Generation Gene World -> List of Pairs
;gets pairs of (Gene Trait) for given Generatoin, Gene and World

(define (gene-sight gen g w)
  (filter (λ (p) (string=? g (car p))) (get-sight gen w)))

(define (gene-speed gen g w)
  (filter (λ (p) (string=? g (car p))) (get-speed gen w)))

(define (gene-size gen g w)
  (filter (λ (p) (string=? g (car p))) (get-size gen w)))

(define (gene-health gen g w)
  (filter (λ (p) (string=? g (car p))) (get-health gen w)))



;gene-av-TRAIT: Generation Gene World -> Number
;returns trait average for given GENERATION and GENE

(define (gene-av-sight gen g w)
  (cond [(empty? (gene-sight gen g w)) 0]
        [else
         (/(sum-trait(gene-sight gen g w))
           (length (gene-sight gen g w)))]))

(define (gene-av-speed gen g w)
  (cond [(empty? (gene-speed gen g w)) 0]
        [else
         (/(sum-trait(gene-speed gen g w))
           (length (gene-speed gen g w)))]))

(define (gene-av-size gen g w)
  (cond [(empty? (gene-size gen g w)) 0]
        [else
         (/(sum-trait(gene-size gen g w))
           (length (gene-size gen g w)))]))

(define (gene-av-health gen g w)
  (cond [(empty? (gene-health gen g w)) 0]
        [else
         (/(sum-trait(gene-health gen g w))
           (length (gene-health gen g w)))]))

;gene-gen: Gene Generation World -> Sims
;returns all sims of given gene type in given generation

(define (gene-gen g gen w)
  (filter (λ (s) (string=? g (substring (traits-gene(sim-traits s)) 0 1))) (get-gen gen w)))

;gene-pop: Gene Generations World -> List Of Pairs
;returns population change over generations of given gene type

(define (gene-pop g gens w)
  (map (λ (gen) (list gen (length (gene-gen g gen w)))) (build-list gens (λ (x) (+ 1 x)))))



;g-TRAIT-average: Generations Gene World -> ListOf Pairs
;returns average for each generation of given gene

(define (g-sight-average gen g w)
  (map (λ (e) (list e (gene-av-sight e g w))) (build-list gen (λ (x) (+ 1 x)))))

(define (g-speed-average gen g w)
  (map (λ (e) (list e (gene-av-speed e g w))) (build-list gen (λ (x) (+ 1 x)))))

(define (g-size-average gen g w)
  (map (λ (e) (list e (gene-av-size e g w))) (build-list gen (λ (x) (+ 1 x)))))

(define (g-health-average gen g w)
  (map (λ (e) (list e (gene-av-health e g w))) (build-list gen (λ (x) (+ 1 x)))))

(define Ac "DarkOrange")
(define Bc "Orchid")
(define Cc "Red")
(define Dc "MediumTurquoise")
(define Ec "RoyalBlue")
(define Fc "Green")

(define (get-color g)
  (cond [(string=? g "A") Ac]
        [(string=? g "B") Bc]
        [(string=? g "C") Cc]
        [(string=? g "D") Dc]
        [(string=? g "E") Ec]
        [(string=? g "F") Fc]))



;genes-per-gen: Generation Gene World -> List Of Traits
;returns all alive genes of given type in given generation

(define (genes-per-gen gen g w)
  (filter (λ (t) (string=? g (substring (traits-gene(sim-traits t)) 0 1))) (alive-gen gen w)))

;gene-pop: Generations Gene World -> List Of Pairs
;returns list of pairs of alive sims in each generation of given gene

(define (alive-gene-pop gens g w)
  (map (λ (gen) (list gen (length (genes-per-gen gen g w)))) (build-list gens (λ (x) (+ 1 x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PER GENERATION ANALYTICS ;;;;;;;;;;;;;;;;;;;;;;  

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
;averages the given traits for a given GENERATION

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




;count-gene: World-sims Gene -> Number
;counts number of given gene

(define (count-gene s g)
  (count (λ (sim) (string=? (substring(traits-gene(sim-traits sim)) 0 1) g)) s))

;get-stat-pop: Gene World -> List Of Pairs
;gets population for every gene-type at phase (mod phase 10 must = 0)



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
                          ;  #:y-max 
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

(define (plot-gene-speed gens g w)
  (plot (lines (g-speed-average gens g w)
                            #:label (string-append g " " "Average Speed")
                            #:color "Green"
                            #:y-max 25
                            )))

(define (plot-gene-sight gens g w)
  (plot (lines (g-sight-average gens g w)
                            #:label (string-append g " " "Average Sight")
                            #:color "Brown"
                           ; #:y-max 10
                            )))

(define (plot-gene-size gens g w)
  (plot (lines (g-size-average gens g w)
                            #:label (string-append g " " "Average Size")
                            #:color "Blue"
                            ;#:y-max 10
                            )))

(define (plot-gene-health gens g w)
  (plot (lines (g-health-average gens g w)
                            #:label (string-append g " " "Average Health")
                            #:color "Red"
                           ; #:y-max 10
                            )))

    


(define (help-pop gens w)
  (map (λ (gene) (list gene(gene-pop gene gens w))) all-genes))

(define (get-pop gens w)
  (map (λ (gene) (list (first gene) (sum-trait (first(rest gene))))) (help-pop gens w)))

(define (get-dg gens w)
  (argmax (λ (g) (first (cdr g))) (get-pop gens w)))



;plot-gene-pop: Gene Generations World -> Plot

(define (plot-gene-pop gens w)
   (map (λ (gene) (lines (gene-pop gene gens w)
                        
                        
                               #:label (string-append gene " " "Population Change")
                              #:color (get-color gene)
                              ;#:y-max 200
                              
                              )) all-genes))

(define (plot-all-speed gens w)
  (map (λ (gene) (lines (g-speed-average gens gene w)
                        
                        
                               #:label (string-append gene " " "Speed Trait Average")
                              #:color (get-color gene)
                              ;15min
                              ;:y-max 15
                              ;25min
                              #:y-max 20
                              )) all-genes))

(define (plot-all-sight gens w)
  (map (λ (gene) (lines (g-sight-average gens gene w)
                        
                        
                               #:label (string-append gene " " "Sight Trait Average")
                              #:color (get-color gene)
                              ;15min
                              ;:y-max 15
                              ;25min
                              #:y-max 20
                              )) all-genes))

(define (plot-all-size gens w)
  (map (λ (gene) (lines (g-size-average gens gene w)
                        
                        
                               #:label (string-append gene " " "Size Trait Average")
                              #:color (get-color gene)
                              ;15min
                              ;:y-max 15
                              ;25min
                              #:y-max 20
                              )) all-genes))



(define (current-alive w)
  (list (phase-timer(world-phase w))
  (length (world-sims w))))
;accumulator: pop is the list of current modular populations

(define (run-retreive-pop n pop w)
  (cond [(= n 0) pop]
        [(=(modulo n 5) 0) (run-retreive-pop (sub1 n) (cons (current-alive w) pop) (update-world w))]
        [else (run-retreive-pop (sub1 n) pop (update-world w))]))

(define (plot-all-pop w)
  (lines (run-retreive-pop stop-phase empty w)
         #:label "Overall Population Change"
         #:color "red"
         ;15min
         ;:y-max 15
         ;25min
         ;#:y-max 20
         ))

(plot-width 800)
(plot-height 500)
(plot-legend-anchor 'top-left)

;plot-all-15: String Number World -> Plots
;takes a String (filename prefix) Number (max gens for given stop-phase) and a world and returns file-plots

(define (plot-all-gens pref gens)
  (begin (let ([w (run-n-times stop-phase init-world1)])
           (plot-file (plot-gene-pop gens w)
                      (string-append pref "gene-pop.png")
                      'png)
           (plot-file (plot-all-speed gens w)
                      (string-append pref "av-speed.png")
                      'png)
           (plot-file (plot-all-sight gens w)
                      (string-append pref "av-sight.png")
                      'png)
           (plot-file (plot-all-size gens w)
                      (string-append pref "av-size.png")
                      'png))))
                     

(define (file-all-pop pref w)
           (plot-file (plot-all-pop w)
                      (string-append pref "gene-pop.png")
                      'png))
                     


        
