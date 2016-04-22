(ns program-generator.core)
; (in-ns 'program-generator.core)

(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])
(require '[clojure.data.json :as json])


(defn add-vec [a b]
    (map
        (partial reduce +)
        (map vector a b)))

(defn split [v]
    (split-at
        (/ (count v) 2)
        v))


(defn rotate [v]
    (cons
        (last v)
        (reverse
            (rest (reverse v)))))

(defn add-a-b [v]
    (let [[a b] (split v)]
        (concat
            (add-vec a b)
            b)))

(defn rot-a [v]
    (let [[a b] (split v)]
        (concat
            (rotate a)
            b)))

(defn switch [v]
    (let [[a b] (split v)]
        (concat b a)))

(defn zero [v]
    (replicate (count v) 0))

(defn zero-a [v]
    (let [[a b] (split v)]
        (concat (zero a) b)))

(defn add-one [v]
    (cons
        (+ 1 (first v))
        (rest v)))

(defn swap-first [v]
    (cons
        (second v)
        (cons
            (first v)
            (nthrest v 2))))

(def primitives
    [rotate add-a-b rot-a switch zero zero-a add-one
        swap-first])

(def dimension 10)

(defn rand-vec []
    (take dimension
        (repeatedly rand)))

(def datafile "data.json")
(defn append-data [data] (spit datafile data :append true))
(defn delete-data [] (io/delete-file datafile))
(defn write-data [data]
    (spit datafile data))

(defn round [num]
    (let [sig 1000]
        (/
            (Math/round (float (* sig num)))
            sig)))

(defn generate-primitive-datapoint []
    (let [primitive-index (rand-int (count primitives))
            primitive (nth primitives primitive-index)
            input (rand-vec)]
        (vector
            primitive-index
            (into []
                (map round input))
            (into []
                (map round (primitive input))))))

(defn random-primitive []
    (let [primitive-index (rand-int (count primitives))]
        (vector primitive-index (nth primitives primitive-index))))
; (generate-primitive-datapoint)

; (json/write-str (generate-primitive-datapoint))

(defn generate-primitive-data [n]
    (let [dataset (take n (repeatedly generate-primitive-datapoint))]
        (write-data (json/write-str dataset))))

(defn generate-rotate-datapoint []
    (let [input (rand-vec)]
        (vector
            0
            (into []
                (map round input))
            (into []
                (map round (rotate input))))))

(defn generate-rotate-data [n]
    (let [dataset (take n (repeatedly generate-rotate-datapoint))]
        (spit "rotate.json" (json/write-str dataset))))

(defn generate-twostep-datapoint []
    (let [[first-primitive-index first-primitive]  (random-primitive)
          [second-primitive-index second-primitive]  (random-primitive)
          input (rand-vec)]
        (vector
            [first-primitive-index second-primitive-index]
            (into []
                (map round input))
            (into []
                (map round (first-primitive (second-primitive input)))))))

; (generate-primitive-data 100000)
(generate-rotate-data 10000)

; (random-primitive)
; (generate-twostep-datapoint)
; (nth [1 2 3] 1)
; ((nth primitives 0) ((nth primitives 1) [0.3650  0.4630  0.2320  0.8650  0.5530  0.7410  0.3880  0.1550  0.2220  0.3180]))

(defn generate-dataset [generator filename n]
    (let [dataset (take n (repeatedly generator))
          json-data (json/write-str dataset)]
        (spit filename json-data))


(defn p-norm [p x]
   (if (= p :infinity)
     (apply max (for [xi x] (Math/abs xi)))
     (Math/pow
       (reduce + (for [xi x] (Math/pow xi p)))
       (/ 1 p))))

(defn p-metric [p x y]
    (p-norm p (map - x y)))

; (p-norm 2 [1 0 0 0])
;
; (mean (repeatedly 10000
;         (fn []
;             (let [[primitive x y] (generate-primitive-datapoint)]
;                 (p-metric 2 (rotate x) y)))))
;
; (mean [1 1000 0])

(defn mean [v]
    (/
        (reduce + v)
        (count v)))


; (def a [0.44 -0.16 0.05 0.10 -0.05 -0.11 0.38 0.09 0.76 0.29])
; (def b [0.67 0.00 0.00 0.00 0.00 0.00 0.34 0.12 0.96 0.36])
;
; (defn abs [x] (max x (- x)))

; (abs -1.89)
;
; (reduce + (map abs (map - a b)))
; (reduce + (map abs b))
