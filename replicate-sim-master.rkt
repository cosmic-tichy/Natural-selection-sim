#lang racket
(require posn)
(require racket/string)
(require (prefix-in htdp: 2htdp/image))
(require 2htdp/universe)
(require 2htdp/batch-io)
(require plot/no-gui)
(require racket/serialize)


(provide (all-defined-out))

(define-struct posn [x y] #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EVNIRONMENT CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Randomness
;set-random: World -> PRNG

(define reset-prng (pseudo-random-generator->vector(current-pseudo-random-generator)))
(define static-prng '#(2787456147 4234022470 3307900229 924538768 2673908145 3644834039))
(define gen1 (vector->pseudo-random-generator reset-prng))
;Food
(define field-count 20)

;Sims
(define sight-factor 5)
(define death-rate .12)
(define food/health-rate 8)
(define fertility-rate 9)
(define mutation-rate 9)
(define improvement-rate 3)
(define decay-rate 3)
(define all-genes (list "A" "B" "C" "D" "E" "F"))

;Big-Bang
(define tick-speed 10)
(define stop-phase  (* 5 8410))
(define height 700)
(define width 700)
(define background (htdp:place-image(htdp:rectangle (- width 80) 4 "solid" "white") (/ width 2) (- height 20) 
                                    (htdp:rectangle height width "solid" "black")))
(define prog-size 7)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define posn1 (make-posn 10 10))
(define posn2 (make-posn 30 30))
(define posn3 (make-posn 40 40))
(define posn4 (make-posn 3 3))



;(check-expect (posn-sum posn1 posn1) (make-posn 20 20))
;(check-expect (posn-sum posn1 posn2) (make-posn 40 40))


;posn-sum: Posn Posn -> Posn
;takes in two Posns and computes sum


(define (posn-sum p1 p2)
  (make-posn(+(posn-x p1)(posn-x p2)) (+(posn-y p1)(posn-y p2))))


;posn-dif Posn Posn -> Posn
;takes a Posn and a Posn and computes difference between second and first

;(check-expect (posn-diff posn2 posn1) (make-posn 20 20))
;(check-expect (posn-diff posn3 posn2)
;              (make-posn 10 10))

(define (posn-diff p2 p1)
  (make-posn(-(posn-x p2)(posn-x p1)) (-(posn-y p2)(posn-y p1))))

;posn-scale: Number Posn -> Posn
;takes in a Number and a Posn and scales Posn by Number
;
;(check-expect (posn-scale 10 posn1)
;              (make-posn 100 100))
;
;(check-expect (posn-scale 10 posn2)
;              (make-posn 300 300))

(define (posn-scale num p)
  (make-posn (* num (posn-x p)) (* num (posn-y p))))

; dist : Posn Posn -> Number
; computes the distance between two Posns
;(check-within (dist posn1 posn2 ) (sqrt 800) #i28.284271247461902)
;(check-within (dist posn3 posn4 ) (sqrt 2738) #i52.32590180780452)


(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))


; direction : Posn Posn -> Posn
; computes the direction from p to goal,
; represented as a Posn whose distance from the origin is 1
;
;(check-expect (direction (make-posn 230 240) (make-posn 200 200))
;              (make-posn -3/5 -4/5))
;(check-expect (direction (make-posn 170 240) (make-posn 200 200))
;              (make-posn 3/5 -4/5))
;(check-expect (direction (make-posn 230 160) (make-posn 200 200))
;              (make-posn -3/5 4/5))
;(check-expect (direction (make-posn 170 160) (make-posn 200 200))
;              (make-posn 3/5 4/5))
; 

(define (direction p goal)
  (posn-scale (/ 1 (dist p goal)) (posn-diff goal p)))

; approach-helper : Posn Posn Number -> Posn
; takes a Posn a Posn and a Number and returns a posn at the next coordinate at designated speed (s)
;
;(check-expect (approach-helper (make-posn 230 240) (make-posn 200 200) 3)
;              (make-posn 228.2 237.6))
;
;(check-expect (approach-helper (make-posn 230 240) (make-posn 200 200) 40)
;              (make-posn 206 208))

(define (approach-helper p goal s)
  (posn-sum p (posn-scale s (direction p goal))))




;A Pworld is a (make-pworld p goal)

(define-struct pworld [p goal] #:transparent)

(define init-state (make-pworld (make-posn 256 256) (make-posn 257 257)))

(define pworld1 (make-pworld (make-posn 230 240) (make-posn 200 200)))
(define pworld2 (make-pworld (make-posn 170 240) (make-posn 200 200)))




;within-screen: Pworld -> Pworld
;if goal is outside of frame, make goal within frame



(define (within-screen pw)
  (cond [(or (>= (posn-x(pworld-goal pw)) width)
             (< (posn-x(pworld-goal pw)) 0)
             (>= (posn-y(pworld-goal pw)) height)
             (< (posn-x(pworld-goal pw)) 0)) 
         (make-pworld (pworld-p pw) (make-posn (random width gen1 ) (random height gen1 )))]
        [else pw]))
             


;move-player: Pworld -> Pworld
;takes in a Pworld and outputs updated with player approaching goal at s speed



(define (move-player pw s)
  (make-pworld
   (approach (pworld-p pw) (pworld-goal pw) s)
   (pworld-goal pw)))

;approach: Posn Posn Number -> Posn
;takes Posn (player) a Posn (goal) and a Number (speed) and returns a Posn

;(check-expect (approach (make-posn 1 1) (make-posn 2 2) 2)
;              (make-posn 2 2))
;
;(check-expect (approach (make-posn 1 1) (make-posn 5 5) 5)
;              (make-posn 5 5))

(define (approach p1 p2 s)
  (cond
    [(and ( < (posn-x p1) (+ (posn-x p2) s))
          ( < (posn-y p1) (+ (posn-y p2) s))
          ( > (posn-x p1) (- (posn-x p2) s))
          ( > (posn-y p1) (- (posn-y p2) s)) ) p2 ]
    [else (approach-helper p1 p2 s)]))








;SIM-WORLD BEGIN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEFINITIONS and CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define slice (htdp:triangle 10 "solid" "Dark Green"))

(define pworld4 (make-pworld (make-posn 325 400) (make-posn 220 113)))
(define pworld5 (make-pworld (make-posn 100 34) (make-posn 200 124)))

(define pworld-start (make-pworld (make-posn (/ width 2) (/ height 2))
                                  (make-posn (random width gen1) (random height gen1 ))))

;a Food is a (make-posn Number Number)

(define food1 (make-posn 226 238))
(define food2 (make-posn 103 60))
(define food3 (make-posn 20 400))



;a traits is a (make-traits Number Number Number Number)

;; speed is a Number 1 - 7
;; sight is a Number 1 - 15
; - sight pixel distance = sight-trait * sight-factor
;; size is a Number 1 - 15
;; health is a Number 1 - 100
;; gene is a Genomic code (String) used for tracking offspring of the form
; - (letter [A-Z]) (Number [1-1000])
; - Examples: F20, A45, B67
; - Non-examples: 20Z, 500A, A2000


(define-struct traits [speed sight size health gene] #:transparent)

;;;;;;;;; FIRST EPOCH
;(define A (make-traits 7 9 8 100 "A1"))
;(define B (make-traits 5 15 5 100 "B1"))
;(define C (make-traits 4 9 15 100 "C1"))
;(define D (make-traits 4 15 15 100 "D1"))
;(define E (make-traits 5 11 12 100 "E1"))
;(define F (make-traits 4 12 12 100 "F1"))

(define A (make-traits 5 7 6 100 "A1"))
(define B (make-traits 4 8 5 100 "B1"))
(define C (make-traits 5 7 7 100 "C1"))
(define D (make-traits 6 5 6 100 "D1"))
(define E (make-traits 5 8 3 100 "E1"))
(define F (make-traits 4 9 8 100 "F1"))


; a sim is a (make-sim pworld traits)

(define-struct sim [pworld traits] #:transparent)

(define simA (make-sim pworld-start A))
(define simB (make-sim pworld-start B))
(define simC (make-sim pworld-start C))
(define simD (make-sim pworld-start D))
(define simE (make-sim pworld-start E))
(define simF (make-sim pworld-start F))

(define first-food (make-posn (random height gen1) (random width gen1)))

;a sims is a ListOfSim
;a field is one of
; - Food
; - Food ListOfFood
;a world is a (make-world field sims)
;a phase is a (make-phase Number Number)
;a timer is a Number (number of phases in big-bang)

(define-struct phase [prog timer hist stat])

(define-struct world [phase field sims dead prng])

(define-struct stat [sims]#:transparent)

;counter: Phase -> Image
;keeps phase count

(define (counter p)
  (htdp:text (number->string (quotient (phase-timer p)28)) 25 "blue"))


(define init-world1 (make-world (make-phase 40 0 empty empty) (list first-food) (list simA simB simC simD simE simF) empty
  reset-prng))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD DRAWING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;current-gens: World-sims -> List Of Number

(define (all-gens s)
  (cond [(empty? s) empty]
        [else 
         (cond [(string-contains? (traits-gene(sim-traits (first s))) "M")
                (cons
                (+ (string->number (substring(first(string-split (traits-gene(sim-traits (first s))) "M")) 1))
                      (string->number (first(rest(string-split (traits-gene(sim-traits (first s))) "M")))))
                (all-gens (rest s)))]
               [else (cons (string->number (substring (traits-gene(sim-traits (first s))) 1))
                           (all-gens (rest s)))])]))

;current-gens: world-sims -> List OF Number

(define (current-gens s)
  (map (λ (gen) (count (λ (g) (= g gen)) (sort (all-gens s)<))) (sort (remove-duplicates (all-gens s))<)))

;gen-count-help: list of Number List of Number -> List of Pairs
;ASSUMES BOTH LISTS ARE SAME LENGTH pairs population of gen with gen number of current living sims


(define (gen-count-help pop g)
  (cond [(empty? pop) empty]
        [else (cons (list (first g) (first pop))
                    (gen-count-help (rest pop) (rest g)))]))


(define (gen-count w)
  (gen-count-help (current-gens(world-sims w))
                  (sort (remove-duplicates (all-gens (world-sims w)))<)))

;furthest-current: World -> Pair
;returns the furthest generation and the highest pop generation (current)
;(first is the furthest and second is the highest pop gen)

(define (furthest-current w)
  (cond [(empty? (gen-count w)) (list (list 0 0) (list 0 0))]
        [else 
         (list (argmax (λ (g) (first g)) (gen-count w))
               (argmax (λ (g) (first (rest g))) (gen-count w)))]))
          


; convert : Number Number Number Number Number -> Number
; return what a should become according to linear interpolation
; if a1 becomes b1 and a2 becomes b2
; *Assumption*: a1 and a2 are different

(define (convert a1 b1 a2 b2 a)
  (/ (+ (* b1 (- a2 a)) (* b2 (- a a1))) (- a2 a1)))

;progress-bar: World -> x-posn
;gives progress bar indicator x-pos according to linear interpolation
(define (progress-bar w)
  (convert 0 40 stop-phase (- height 40) (phase-prog(world-phase w))))


;health-color: traits -> RGB value
;determines sim color for health value

(define (health-color t)
  (cond [(>(traits-health t) 100) (htdp:make-color 0 250 0)]
        [else (htdp:make-color (round (convert 0 250 100 1 (traits-health t)))
                               (round (convert 0 1 100 250 (traits-health t)))
                               0)]))
;draw-sims: sims -> Image
;draws just sims on background height * width

(define (draw-sims s)
  (foldr (λ (s y) (htdp:place-image (htdp:overlay (htdp:text (substring (traits-gene (sim-traits s)) 0 1) 10 "black")
                                        (htdp:circle (traits-size(sim-traits s)) "solid"
                                                "green"))
                               (posn-x (pworld-p(sim-pworld s))) (posn-y (pworld-p(sim-pworld s))) y)) background s ))

(define (draw-stats w)
  (htdp:place-image (htdp:above (htdp:text(string-append "Youngest " (number->string (first (first (furthest-current w))))
                                                         "("
                                                         (number->string (first (rest (first(furthest-current w)))))
                                                         ")")12 "white")
                                                                  
                                (htdp:text(string-append "Most Populous "(number->string (first(first (rest(furthest-current w)))))
                                                         "("
                                                         (number->string (first(rest (first (rest(furthest-current w))))))
                                          ")")12 "white")
                                (htdp:text(string-append "Total Population: "(number->string(length(world-sims w))))
                                          12 "white")
                                (htdp:text(string-append "Phase: "(number->string(phase-timer(world-phase w))))
                                          12 "white")
                                )
                                 (/ width 2) 30 (draw-sims(world-sims w))))


;draw-world: world -> image

(define (draw-world w)
  (foldr (λ (f y) (htdp:place-image slice (posn-x f) (posn-y f)
                                    (htdp:place-image
                                     (htdp:circle prog-size "solid" "blue") (progress-bar w) (- height 20) y
                                    )))
           (draw-stats w) (world-field w)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIM MOVEMENT and SIGHT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;in-sight?: Sim Field -> Boolean
;returns true if food is in sight, false if not

(define (in-sight? s lof)
  (ormap (λ (f) (<= (dist (pworld-p (sim-pworld s)) f)
                    (*(traits-sight (sim-traits s)) sight-factor)))
         lof))

;in-sight: Sim Field -> Field
;takes a sim and field and returns all food which are in sight of sim

(define (in-sight s lof)
  (filter (λ (f) (<= (dist (pworld-p (sim-pworld s)) f)
                     (*(traits-sight (sim-traits s)) sight-factor)))
          lof))


;reached-goal?: Sim -> Boolean
(define (reached-goal? s)
  (< (dist (pworld-p(sim-pworld s))
           (pworld-goal(sim-pworld s))) (traits-size (sim-traits s))))
  


;reached-food?: sim lof -> Boolean
;determines if a sim has reached given food

                                                                              
(define (reached-food? s lof)
  (ormap (λ (f) (<=(dist f (pworld-p (sim-pworld s)))
                   (traits-size (sim-traits s)))) lof))

;next-goal: Sim Field -> Sim
;takes a sim which needs updated goal and field and returns sim with updated goal as first in-sight food in list
;;;;;;;;;;;;;;;; If no food is in sight, sim continues in straight line


(define (next-goal s lof)
  (cond [(in-sight? s lof) (make-sim (make-pworld (pworld-p (sim-pworld s)) (first (in-sight s lof))) (sim-traits s))]
        [else (make-sim (make-pworld (pworld-p (sim-pworld s)) (make-posn(random width gen1)
                                                                         (random height gen1)))
                        (sim-traits s))]))

;update-goal: Sim Field -> Sim
;updates goal if needed, does nothing if not

(define (update-goal s lof)
  (cond [(or (reached-goal? s) (reached-food? s lof) (in-sight? s lof)) (next-goal s lof)]
        [else s]))


;update-goals: World -> World
;updates all goals using update-goal

(define (update-goals w)
  (make-world (world-phase w) (world-field w)
              (map (λ (s) (update-goal s (world-field w))) (world-sims w))
              (world-dead w)
              (world-prng w)))
  


;move-sims: World  -> Sims
;moves all sims toward goal at their traits speed


(define (move-sims w)
  (map (λ (s) (make-sim (move-player (within-screen (sim-pworld s)) (traits-speed (sim-traits s)))
                        (sim-traits s))) (world-sims w)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIM HEALTH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;add-health: w -> Sims
;adds 1 to health when food is reached
(define (add-health w)
  (make-world (world-phase w) (world-field w) (map (λ (s) (cond [(reached-food? s (world-field w))
                                                                 (make-sim (sim-pworld s)
                                                                           (make-traits
                                                                            (traits-speed (sim-traits s))
                                                                            (traits-sight (sim-traits s))
                                                                            (traits-size (sim-traits s))
                                                                            (+ food/health-rate (traits-health (sim-traits s)))
                                                                            (traits-gene (sim-traits s))))]
                                                                [else s])) (world-sims w))
              (world-dead w)
              (world-prng w)))


;remove-health: World -> World
;removes death-rate each phase

(define (remove-health w)
  (make-world (world-phase w) (world-field w) (map (λ (s) (make-sim (sim-pworld s)
                                                                    (make-traits
                                                                     (traits-speed (sim-traits s))
                                                                     (traits-sight (sim-traits s))
                                                                     (traits-size (sim-traits s))
                                                                     (- (traits-health (sim-traits s)) death-rate)
                                                                     (traits-gene (sim-traits s))))) (world-sims w))
              (world-dead w)
              (world-prng w)))




 
              


;update-health: World -> World

(define (update-health w)
  (remove-health (add-health (send-dead w))))



;send-dead: World -> World
;sends all sims with less than 0 health to dead

(define (send-dead w)
  (make-world (world-phase w)
              (world-field w)
              (filter (λ (s) (> (traits-health(sim-traits s)) 0)) (world-sims w))
              (append (filter (λ (s) (not (> (traits-health(sim-traits s)) 0))) (world-sims w))
                      (world-dead w))
              (world-prng w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIM FERTILITY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;fertile?: Sim -> Boolean
;determines if a sim is fertile

(define (fertile? s)
  (< (random 10000 gen1) fertility-rate))

;any-fertile?: Sims -> Boolean
;determines if any sims are fertile

(define (any-fertile? los)
  (ormap (λ (s) (fertile? s)) los))

;mutate?: Sim -> Boolean
;determines if fertile sim will give birth to mutated offspring

(define (mutate? s)
  (< (random 100 gen1) mutation-rate))

;mutate: Sim -> Sim
;returns mutated offspring of given sim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MUTATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mutate s)
  (make-sim (make-pworld (pworld-p (sim-pworld s))
                         (make-posn (random height gen1)
                                    (random width gen1)))
            (make-traits (random (- (traits-speed(sim-traits s)) decay-rate)
                                 (+ (traits-speed(sim-traits s)) improvement-rate) gen1  )
                         (random (- (traits-sight(sim-traits s)) decay-rate)
                                 (+ (traits-sight(sim-traits s)) improvement-rate)gen1)
                         (random (- (traits-size(sim-traits s)) 1)
                                 (+ (traits-size(sim-traits s)) improvement-rate)gen1)
                         100
                         (mutate-gene (traits-gene(sim-traits s))))))

;give-birth: Sims -> Sims
;returns only offspring of fertile sims (give filtered fertile? list)

(define (give-birth los)
  (map (λ (s) (cond [(mutate? s) (mutate s)]
                    [else (make-sim (make-pworld (pworld-p (sim-pworld s))
                                                 (make-posn (random height gen1)
                                                            (random width gen1)))
                                    (make-traits (traits-speed(sim-traits s))
                                                 (traits-sight(sim-traits s))
                                                 (traits-size(sim-traits s))
                                                 (random 70 110 gen1)
                                                 (edit-gene (traits-gene(sim-traits s)))))])) los))





;edit-gene: gene -> gene
;adds 1 to given gene code

;(check-expect (edit-gene "A10")
;              "A11")
;(check-expect (edit-gene "A1")
;              "A2")

(define (edit-gene g)
  (cond [(string-contains? g "M") (string-append (substring g 0 1)
                                                 (number->string (+ 1(string->number (substring (first(string-split g "M")) 1))))
                                                 "M"
                                                 (list-ref(string-split g "M") 1))]
        [else  (string-append (substring g 0 1) (number->string(+ 1(string->number (substring g 1 )))))]))

;mutate-gene: gene -> gene
;adds M1 if gene has never mutated, adds 1 each subsequent mutated mutation

(define (mutate-gene g)
  (cond [(string-contains? g "M") (string-append (first(string-split g "M"))
                                                 "M"
                                                 (number->string(+ 1(string->number (list-ref(string-split g "M") 1)))))]
        [else (string-append g "M1")]))
                                  


;update-offspring: World -> World
;fertile sims give birth, non-fertile do not

(define (update-offspring w)
  (make-world (world-phase w)
              (world-field w)
              (append (give-birth (filter (λ (s) (fertile? s)) (world-sims w)))
                      (world-sims w))
              (world-dead w)
              (world-prng w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SIMS UPDATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;update-sims: w -> World
;updates goals which have been reached and moves all sims toward goals


(define (update-sims w)
  (make-world (world-phase w)
              (world-field w)
              (move-sims (update-goals(update-offspring (update-health (send-dead w)))))
              (world-dead w)
              (world-prng w)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FOOD PRODUCTION and DELETION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;food-remove: Food Sims -> Boolean
;determines if food has been reached 

;(check-expect (food-remove (make-posn 97 33) (list sim4 sim1)) true)

(define (food-reached? f los)
  (ormap (λ (s) (<=(dist f (pworld-p (sim-pworld s)))
                   (traits-size (sim-traits s)))) los))

;update-field: W Number -> World
;removes all food which has been reached by sims, maintains n as constant field size

;(check-expect (update-field (make-world (list food1 food2 food3) (list sim4 sim2 (make-sim (make-pworld (make-posn 100 59) food2)
;                                                                                     (sim-traits sim4))
;                                                                 (make-sim (make-pworld (make-posn 18 398) food2)
;                                                                                     (sim-traits sim4))))) (list food1))


(define (update-field w n)
  (new-food (filter (λ (f) (not (food-reached? f (world-sims w)))) (world-field w)) n))



;new-food: Field Number -> Field
;if number of food drops below n, new food spawns

;;;;;;;;;;;;;;;; SPLIT FIELD INTO QUADRANTS FOR SMOOTHER DIST

(define (new-food lof n)
  (cond [(<(length lof) n) (cons(make-posn (random width gen1) (random height gen1)) lof)]
        [else lof]))




;update-dead: World -> Dead
;adds all dead sims to dead
;
;(define dead-example (make-world (make-phase 200) (list (make-posn 1 2) (make-posn 4 5))
;                                       (list simA simB simC
;                                             (make-sim pworld4 (make-traits 4 5 2 0 "A3")))
;                                       (list (make-sim pworld4 (make-traits 4 5 2 0 "A2")))))
;
;(check-expect (update-dead (make-world (make-phase 200) (list (make-posn 1 2) (make-posn 4 5))
;                                       (list simA simB simC
;                                             (make-sim pworld4 (make-traits 4 5 2 0 "A2"))) empty))
;(list (make-sim pworld4 (make-traits 4 5 2 0 "A2"))))

(define (update-dead w)
              (append (filter (λ (s) (<= (traits-health(sim-traits s)) 0)) (world-sims w))
                      (world-dead w)))

;update-prng: World -> World-prng
;updates prng vector at phase 0

(define (save-prng w)
  (if (=(phase-timer(world-phase w))0)
       (pseudo-random-generator->vector gen1)
      (world-prng w)))


;update-phase: World -> Phase
;updates phase and stores key-world every 100 phases

(define (update-phase w)
  (make-phase
    (+ 1(phase-prog (world-phase w)))
   (+ 1 (phase-timer(world-phase w)))
   (cond [(=(modulo (phase-timer(world-phase w)) 100) 0) w]
         [else (phase-hist(world-phase w))])
   (cond [(=(modulo (phase-timer(world-phase w))10)0)

          (cons (list (phase-timer(world-phase w)) (world-sims w))
                (phase-stat(world-phase w)))]
         [else (phase-stat(world-phase w))])))
                     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLD UPDATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (update-world w)
  (begin(vector->pseudo-random-generator! gen1 (world-prng w))
        (let*([phase (update-phase w)]
             [field (update-field w field-count)]
             [sims (update-sims w)]
             [dead (send-dead w)])
        (make-world  phase field (world-sims sims) (world-dead dead) (pseudo-random-generator->vector gen1)))))
          
             
          
;  (make-world (update-phase w) (update-field w field-count)
;              (world-sims(update-sims w)) (world-dead (send-dead w)) (update-prng w)))



;timeline: World ke -> World
;update world if fforward, retrieve hist if rewind

(define (timeline w ke)
  (cond [(key=? ke "right")  (update-world w) ]
        [(and (key=? ke "left")(> (phase-timer(world-phase w)) 0))
         
                (run-n-times (-(-(phase-timer(world-phase w)) 1)
                               (phase-timer(world-phase(phase-hist(world-phase w)))))
                             (phase-hist(world-phase w)))]
        [else w]))



;get-phase: Number -> Number
;reverse (progress-bar) gets phase according to interp

(define (get-phase x)
  (convert 40 0 (- height 40) stop-phase x))

;me-keyframe: Phase World -> World
;gets appropriate keyframe for rewind (appropriate keyframe = closest keyframe which is less than (get-phase me))

(define (me-keyframe p w)
  (cond [(>= p (phase-timer(world-phase w))) w]
        [else (me-keyframe p (phase-hist(world-phase w)))]))

;prog-timeline: World me -> World

(define (prog-timeline w x y me)
  (cond [(and (mouse=? "drag" me) (>= y (- height 30)) (<= y (- height 10)) (> x 40) (< x (- width 40))
              (> (get-phase x) (phase-timer(world-phase w))))
         (run-n-times (floor(- (get-phase x)
                               (phase-timer(world-phase w))))
                      w)]
        [(and (mouse=? "drag" me) (>= y (- height 30)) (<= y (- height 10)) (> x 40) (< x (- width 40))
              (< (get-phase x) (phase-timer(world-phase w))))
         (run-n-times (floor(-(get-phase x)
                        (phase-timer(world-phase(me-keyframe(get-phase x) w)))))
                      (me-keyframe(get-phase x) w))]
        [else w]))
                        



;BIG-BANG: run-sim: World -> Image

(define (replay-sim w)
  (big-bang w
    (to-draw draw-world)
    ;(on-tick update-world)
    (on-key timeline)
    (on-mouse prog-timeline)
    (stop-when (λ (s) (= stop-phase (phase-timer(world-phase s)))))
    ))


;run-n-times: Number World -> World
;runs the simulation N times 

(define (run-n-times n w)
  (begin 
    (when (zero? (modulo n 100)) (printf "."))
    (cond [(= n 0) w]
          [else (run-n-times (sub1 n) (update-world w))])))
