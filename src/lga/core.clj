(ns aIo.lga.core)

(defn pow [base exp]
  (apply * (take exp (repeat base))))

(defn positive-numbers 
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(defn fit-func [x]
  (+ (* -1 (- x 10) (- x 100)) 200))

(defn individual-str [i]
  (assoc i
    :genome (format "%8s" (Integer/toString (:genome i) 2))))

(defn population-str [population]
  (let [population (map individual-str population)]
  (apply str (interleave (positive-numbers)
                         population
                         (repeat "\n")))))

(defn build-random-population [pop-size genome-size]
 (with-meta (take pop-size (repeatedly #(hash-map :genome (rand-int (pow 2 genome-size)) ;genomes are binary numbers, ie base 2
                                              :genome-size genome-size
                                              :fitness 0)))
            {:population-size pop-size}))

(defn rand-cx-point [i]
  (let [ x (rand-int (:genome-size i))]
    [(Integer/parseInt 
       (apply str (concat (take x                 (repeat 1))
                          (take (- (:genome-size i) x) (repeat 0))))
       2)
     
     (Integer/parseInt 
       (apply str (concat (take x                 (repeat 0))
                          (take (- (:genome-size i) x) (repeat 1))))
       2)]))

(defn crossover [i1 i2];TODO confirm this works as intended
  (let [crossover-point (rand-cx-point i1)]
    [{:genome (bit-or (bit-and (first crossover-point) (:genome i1))
                      (bit-and (last crossover-point)  (:genome i2)))
      :genome-size (:genome-size i1)
      :fitness 0}
     
     {:genome (bit-or (bit-and (first crossover-point) (:genome i2))
                      (bit-and (last crossover-point)  (:genome i1)))
      :genome-size (:genome-size i2)
      :fitness 0}]))

(defn breed-individuals [breed-pop]
  (loop [new-pop [] i 0]
    (if (= i (int (/ (:population-size (meta breed-pop)) 2)))
      (with-meta new-pop {:population-size (:population-size (meta breed-pop))})
      (recur (concat new-pop
                     (crossover (nth breed-pop i)
                                (nth breed-pop (+ (/ (:population-size (meta breed-pop)) 2) i))))
             (inc i)))))

(defn evaluate-individuals [func eval-pop]
  (loop [ret [] i 0]
    (if (= i (:population-size (meta eval-pop)))
    (with-meta ret {:population-size (:population-size (meta eval-pop))})
      (recur (conj ret (assoc (nth eval-pop i)
                                :fitness
                                (func (:genome (nth eval-pop i)))))
               (inc i)))))

(defn -main [pop-size genome-size generations] ; pop must be greater than genome, but why?
  (loop [i 0 current-pop (build-random-population pop-size genome-size)]
    (print "\n\n================================\ngen " i "current pop is\n" (population-str current-pop))
    (if (= i generations)
      (evaluate-individuals fit-func current-pop)
      (recur (inc i) (evaluate-individuals fit-func (breed-individuals current-pop))))))