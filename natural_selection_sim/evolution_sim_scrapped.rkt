;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname evolution_sim_scrapped) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define fast-sim (make-traits 4 6 7 2))
(define slow-sim (make-traits 1 3 10 4))
(define traits1 (make-traits 2 8 4 10))
(define traits2 (make-traits 3 12 5 7))

; a sim is a (make-sim pworld traits)

(define-struct sim [pworld traits])

(define sim1 (make-sim pworld1 traits1))
(define sim2 (make-sim pworld4 slow-sim))
(define sim3 (make-sim pworld2 fast-sim))
(define sim4 (make-sim pworld5 fast-sim))
(define sim5 (make-sim pworld5 traits2))
(define sim6 (make-sim pworld5 traits1))


(define (move-player pw s)
  (cond [(>=(posn-x(pworld-p pw)) width)
         (make-pworld
          (approach (pworld-p pw) (make-posn (-(posn-x(pworld-goal pw))
                                               (* 2(posn-x(pworld-goal pw))))
                                             (posn-y(pworld-goal pw)))s)
          (make-posn (-(posn-x(pworld-goal pw)) (* 2(posn-x(pworld-goal pw))))
                     (posn-y(pworld-goal pw))))]

        [(<=(posn-x(pworld-p pw)) 0)
         (make-pworld
          (approach (pworld-p pw) (make-posn (+(posn-x(pworld-goal pw))
                                               (abs(* 2(posn-x(pworld-goal pw)))))
                                             (posn-y(pworld-goal pw)))s)
          (make-posn (+(posn-x(pworld-goal pw)) (abs (* 2(posn-x(pworld-goal pw)))))
                     (posn-y(pworld-goal pw))))]
        
        [(>=(posn-y(pworld-p pw)) height)
         (make-pworld
          (approach (pworld-p pw) (make-posn (posn-x(pworld-goal pw))
                                             (-(posn-y(pworld-goal pw)) (* 2(posn-y(pworld-goal pw))))) s)
          (make-posn (posn-x(pworld-goal pw)) 
                     (-(posn-y(pworld-goal pw)) (* 2(posn-y(pworld-goal pw))))))]
        [(>=(posn-y(pworld-p pw)) 0) 
         (make-pworld
          (approach (pworld-p pw) (make-posn (posn-x(pworld-goal pw))
                                             (+(posn-y(pworld-goal pw)) (abs (* 2(posn-y(pworld-goal pw)))))) s)
          (make-posn (posn-x(pworld-goal pw)) 
                     (+(posn-y(pworld-goal pw)) (abs(* 2(posn-y(pworld-goal pw)))))))]))

;RANDOM DIRECTION ALGO
;(make-posn(* (posn-x(pworld-goal (sim-pworld s))) (/(random 1 10)(random 1 10)))
;                                                                    (* (posn-y(pworld-goal (sim-pworld s))) (/(random 1 10)(random 1 10)))))




[(empty? (in-sight s lof)) (make-sim (make-pworld (pworld-p(sim-pworld s))
                                                          ;NEEDS IMPROVEMENT
                                                          (make-posn 
                                                          (+ 10(posn-x (pworld-goal(sim-pworld s))))
                                                          (+ 10(posn-y (pworld-goal(sim-pworld s))))))
                                             (sim-traits s))]


(define(update-goals w)
  (append (map (λ (s) (food-goal s (world-field w)))
               (filter (λ (s) (reached-food s (world-field w))) (world-sims w)))
           (filter (λ (s) (not (reached-food s (world-field w)))) (world-sims w))))



(define (give-birth los)
  (cond [(empty? (fertile los)) los]
        [(cons? (fertile los)) (cons (make-sim (sim-pworld (first los))
                                     (make-traits
                                     (traits-speed (sim-traits (first los)))
                                     (traits-sight (sim-traits (first los)))
                                     (traits-size (sim-traits (first los)))
                                     (traits-health (sim-traits (first los)))
                                     0
                                     (edit-gene (traits-gene (sim-traits (first los))))))
                           (cons (first los)
                                 (give-birth (rest los))))]))





(define (in-sight? s lof)
  (ormap (λ (f) (<= (dist (pworld-p (sim-pworld s)) f)
                     (*(traits-sight (sim-traits s)) 10)))
          lof))

(define (in-sight s lof)
  (filter (λ (f) (<= (dist (pworld-p (sim-pworld s)) f)
                     (*(traits-sight (sim-traits s)) 10)))
          lof))


;food-goal: Sim Field -> Sim
;takes a sim and field and returns sim with updated goal as first in-sight food in list
;;;;;;;;;;;;;;;; If no food is in sight, sim continues in straight line

;(check-expect (food-goal sim1 (list food1 food2 food3)) (make-sim (make-pworld (pworld-p (sim-pworld sim1))
;                                                                               food1)
;                                                                  (sim-traits sim1)))
;
;(check-expect (food-goal sim4 (list food1 food2 food3)) (make-sim (make-pworld (pworld-p (sim-pworld sim4))
;                                                                               food2)
;                                                                  (sim-traits sim4)))

(define (food-goal s lof)
  (cond [(empty? (in-sight s lof)) (make-sim (make-pworld (pworld-p(sim-pworld s))
                                                         
                                                          (make-posn 
                                                           (+ 10(posn-x (pworld-goal(sim-pworld s))))
                                                           (+ 10(posn-y (pworld-goal(sim-pworld s))))))
                                             (sim-traits s))]
        [else (make-sim (make-pworld (pworld-p(sim-pworld s)) (first(in-sight s lof))) (sim-traits s))]))


;better-goal: World -> Sims
;returns sims which have food in sight

(define (better-goal w)
  (filter (λ (s) (in-sight? s (world-field w))) (world-sims w)))


;next-food: World -> Sims
;returns sims with better-goal with updated food-goal

(define (next-food w)
  (map (λ (s) (food-goal s (world-field w))) (better-goal w)))

;reached-goal?: Sim -> Boolean
(define (reached-goal? s)
  (< (dist (pworld-p(sim-pworld s))
           (pworld-goal(sim-pworld s))) (traits-size (sim-traits s))))

;next-goal: World -> Sims
;returns Sims who have reached goal with updated goal

(define (next-goal w)
  (map (λ (s) (food-goal s (world-field w)))
       (filter (λ (s) (reached-goal? s)) (world-sims w))))

;update-goals: world -> Sims
;updates goals which have been reached or nearer food is spotted

(define(update-goals w)
  (append (filter (λ (s) (and (not(reached-goal? s)) (not(in-sight? s (world-field w))))) (world-sims w))
          (next-food w)
          (next-goal w)))
                        
