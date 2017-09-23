(ns aIo.lga.core
  (:gen-class))

(def GENERATIONS 100)
(def POP_SIZE 26)
(def GENOME_SIZE 8)

(defn pow [base exp]
  (apply * (take exp (repeat base))))

(defn positive-numbers 
  ([] (positive-numbers 1))
  ([n] (lazy-seq (cons n (positive-numbers (inc n))))))

(defn individual-str [i]
  (assoc i
    :genome (format "%8s" (Integer/toString (:genome i) 2))
    :fitness (fit-func (:genome i))))

(defn pop-str [population]
  (let [population (map individual-str population)]
  (apply str (interleave (positive-numbers)
                         population
                         (repeat "\n")))))

(defn fit-func [x]
  (+ (* -1 (- x 10) (- x 100)) 200))

(defn build-random-pop [size]
 (take size (repeatedly #(hash-map :genome (rand-int (pow 2 GENOME_SIZE)) ;genomes are binary numbers, ie base 2
                                   :fitness 0))))

(defn rand-cx-point []
  (let [x (rand-int GENOME_SIZE)]
    [(Integer/parseInt 
       (apply str (concat (take x                 (repeat 1))
                          (take (- GENOME_SIZE x) (repeat 0))))
       2)
     
     (Integer/parseInt 
       (apply str (concat (take x                 (repeat 0))
                          (take (- GENOME_SIZE x) (repeat 1))))
       2)]))

(defn crossover [i1 i2];TODO confirm this works as intended
  (let [crossover-point (rand-cx-point)]
    [{:genome (bit-or (bit-and (first crossover-point) (:genome i1))
                      (bit-and (last crossover-point)  (:genome i2)))
      :fitness 0}
     
     {:genome (bit-or (bit-and (first crossover-point) (:genome i2))
                      (bit-and (last crossover-point)  (:genome i1)))
     :fitness 0}]))

(defn breed-individuals [breed-pop]
  (loop [new-pop [] i 0]
    (if (= i (int (/ POP_SIZE 2)))
      new-pop
      (recur (concat new-pop
                     (crossover (nth breed-pop i)
                                (nth breed-pop (+ (/ POP_SIZE 2) i))))
             (inc i)))))

(defn eval-individuals [func eval-pop]
    (loop [ret [] i 0]
      (if (= i POP_SIZE)
        ret
        (recur (conj ret (assoc (nth eval-pop i)
                                :fitness
                                (func (:genome (nth eval-pop i)))))
               (inc i)))))

(defn -main []
  (loop [i 0 current-pop (build-random-pop POP_SIZE)]
    (print "\n\n================================\ngen " i "current pop is\n" (pop-str current-pop))
    (if (= i GENERATIONS)
      current-pop
      (recur (inc i) (breed-individuals (eval-individuals fit-func current-pop))))))

;(load-file "/Users/alchemist/workspace/lga.clj")