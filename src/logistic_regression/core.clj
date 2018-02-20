(ns logistic-regression.core
  (:require
    [clojure.core.matrix :as matrix]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [incanter.charts :as charts]
    [incanter.core :refer [view]])
  (:gen-class))

(defn read-csv
  "Reads csv file and returns its data"
  []
  (with-open [in-file (io/reader "resources/data.csv")]
    (doall
      (csv/read-csv in-file))))

;; Parameters needed for logistic-regression
(defn convert-to-numbers
  [m]
  (mapv #(mapv read-string %) m))
(def data (convert-to-numbers (read-csv)))
(def data-count (count data))
(def train-percentage 0.8) ;; 80%
(def test-percentage 0.2) ;; 20%
(def alpha 0.01)
(def iterations 1500)
(def xs (map pop (take (* train-percentage data-count) data)))
(def y (map last (take (* train-percentage data-count) data)))
(def number-of-examples (count xs))
(def initial-theta (matrix/transpose (matrix/matrix [0 0 0])))

(defn prefix-one
  [X]
  (mapv #(cons 1 %) X))

(def X (matrix/matrix xs))

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn standarad-deviation
  [coll]
  (let [avg (average coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (dec total))
        (Math/sqrt))))

;; Now normalize the data
(defn normalise-row
  "Normalize the data so that gradient descent algo doesn't take too long to
  produce minima"
  [x]
  (let [mean (average x)
        std (standarad-deviation x)]
    (->> x
         (mapv #(- % mean))
         (mapv #(/ % std)))))

(defn normalise-features
  [X]
  (->> X
       (matrix/transpose)
       (mapv normalise-row)
       (matrix/transpose)))

(defn sigmoid
  "Returns the value of sigmoid function for given `z`"
  [z]
  (/ 1 (+ 1 (Math/exp (- z)))))

(defn hypothesis
  "Returns hypothesis based on the given `theta` and `X` features"
  [X theta]
  (let [X-theta (matrix/mmul X theta)
        expt (mapv sigmoid X-theta)]
    expt))

(defn compute-cost
  "Computes cost fn J(theta)"
  [X y theta]
  (let [predicted (hypothesis X theta)
        pred-1-y (matrix/mmul (matrix/sub 1 y)
                              (mapv #(Math/log %) (matrix/sub 1 predicted)))
        pred-y (matrix/mmul y (mapv #(Math/log %) predicted))]
    (matrix/mul (/ -1 number-of-examples)
                (matrix/add pred-y pred-1-y))))

(defn cost-derivative
  "Returns cost-derivative"
  [X y theta]
  (let [prediction (hypothesis X theta)]
    (-> prediction
        (matrix/sub y)
        (matrix/mmul X)
        (matrix/mul (/ alpha number-of-examples)))))

(defn gradient-descent
  "Gradient-descent algo entry point"
  [X y theta alpha number-of-iterations]
  (loop [remaining-iterations number-of-iterations
         theta theta]
    (if (zero? remaining-iterations)
      theta
      (recur (dec remaining-iterations)
             (matrix/sub theta (cost-derivative X y theta))))))

(defn costs
  "Keeps track of cost fn value at each step"
  [X y theta alpha number-of-iterations]
  (loop [remaining-iterations number-of-iterations
         theta theta
         costs []]
    (if (zero? remaining-iterations)
      costs
      (recur (dec remaining-iterations)
             (matrix/sub theta (cost-derivative X y theta))
             (conj costs (compute-cost X y theta))))))

(def Xa (-> X
            (normalise-features)
            (prefix-one)))

(def result (gradient-descent Xa y initial-theta alpha iterations))
(def cost-history (costs Xa y initial-theta alpha iterations))

;; ---------------OR-------------------
;; We can use normal equation[(inverse ((X-transpose).X)).(X-transpose).y]
;; since we have less number of features here
;; ------------------------------------
(def result-by-norm-eqn
  (matrix/mmul (matrix/inverse (matrix/mmul (matrix/transpose Xa) Xa))
               (matrix/transpose Xa) y))

result-by-norm-eqn

;; ------------------------------------
;; Plot graphs for const-fn and data
;; ------------------------------------
(defn plot-cost-history
  "Plots cost-history at each and every step"
  [y]
  (let [plot (charts/scatter-plot (range 0 iterations) y
                                  :x-label "Iteration"
                                  :y-label "Cost")]
    (doto plot view)))

(plot-cost-history cost-history)

(defn plot-data
  "Plots data"
  [x y]
  (let [plot (charts/scatter-plot x y
                                  :x-label "Population of City in 10,000s"
                                  :y-label "Profit in $10,000s")]
    (doto plot view)))

(plot-data (mapv first xs) (mapv second xs))

;; ------------------------------------
;; For testing-purpose
;; ------------------------------------
(def test-data (-> (map pop (take-last (* test-percentage data-count)
                                       data))
                   (matrix/matrix)
                   (normalise-features)
                   (prefix-one)))
(def test-y (map last (take-last (* test-percentage data-count) data)))

(defn accuracy
  "Returns the accuracy result for the algorithm"
  [final-theta]
  (let [predicted (hypothesis test-data final-theta)
        assign-class (mapv #(if (>= % 0.5) 1 0) predicted)
        accuracy (filter zero? (mapv compare test-y assign-class))
        percentage (* 100
                      (/ (count accuracy) (* test-percentage data-count)))]
    percentage))

(accuracy result)
(accuracy result-by-norm-eqn)
