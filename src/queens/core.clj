(ns queens.core
  (:require
    [manifold.stream :as s]
    [manifold.deferred :as d])
  (:gen-class))

(defn all-positions
  [n]
  (set
    (cons
      [0 0]
      (map
        (fn [i] [(mod i n) (int (/ i n))])
        (range 1 (* n n))))))

(defn generate-diag
  [position n xs ys]
  (loop [diags #{} [x y] position]
    (let [xn (+ x xs) yn (+ y ys)]
      (if (or (< xn 0) (>= xn n) (< yn 0) (>= yn n))
        diags (recur (conj diags [xn yn]) [xn yn])))))

(defn generate-all-invalid
  [n position]
  (conj
    (apply
      clojure.set/union
      (map
        (fn [[x y]] (generate-diag position n x y))
        [[0 -1] [-1 0] [0  1] [ 1  0]
         [1  1] [-1 1] [1 -1] [-1 -1]])) position))

(defn make-invalid-map
  [n]
  (reduce
    (fn [im p] (assoc-in im p (generate-all-invalid n p)))
    {} (all-positions n)))

(defn process-working
  [n invalid-map valid working [queens available]]
  (if (= (count queens) n)
    @(s/put! valid queens)
    (when (not (empty? available))
      (doseq [i available]
        (let [remaining (not-empty (clojure.set/difference
                                     available (get-in invalid-map i)))]
          (when (or (not (nil? remaining))
                    (= (+ (count queens) 1) n))
            @(s/put! working [(conj queens i) remaining])))))) true)

(defn keep-non-duplicates
  [tracker incoming next-level]
  (s/connect
    (s/map
      #(do (swap! tracker conj (first %)) %)
      (s/filter
        #(not (contains? @tracker (first %)))
        incoming))
    next-level))

(defn level-pairs
  [levels]
  (loop [result []
         [current-level next-level & r] levels]
    (if (not current-level)
      result
      (let [incoming (s/stream) tracker (atom #{})]
        (if (not (nil? next-level))
          (keep-non-duplicates tracker incoming next-level)
          (s/on-drained current-level #(s/close! incoming)))
        (recur
          (conj result (list current-level
                             (when (not (nil? next-level)) incoming)
                             tracker))
          (cons next-level r))))))

(defn next-solver-item
  [n invalid-map levels valid]
  (some
    (fn [[current-level next-level]]
      (when-let [i @(s/try-take! current-level nil 0 nil)]
        (process-working n invalid-map valid next-level i)))
    levels))

(defn start-solver!
  [n invalid-map levels valid]
  (d/loop []
    (d/chain
      (d/future (next-solver-item n invalid-map levels valid))
      (fn [rval]
        (if (not rval)
          :completed
          (d/recur))))))

(defn find-valid-positions
  [n]
  (let [invalid-map (make-invalid-map n)
        levels (reverse (level-pairs (take n (repeatedly #(s/stream 1e4))))) 
        initial-level (first (last levels))
        ap (all-positions n)
        valid (s/stream 1e7)
        results (atom #{})]
    (s/consume #(swap! results conj (set %)) valid)
    (doseq [i ap]
      @(s/put! initial-level
               [#{i} (clojure.set/difference
                       ap (get-in invalid-map i))]))
    (s/close! initial-level)
    {:valid valid :levels levels
     :solvers (take 8 (repeatedly #(start-solver! n invalid-map levels valid)))
     :result results}))

(comment

  (def all (all-positions 8))
  (def f1 (generate-all-invalid 8 [0 0]))
  (def r (clojure.set/difference all f1))
  (def f2 (generate-all-invalid 8 (first f1)))
  (def r2 (clojure.set/difference r f2))

  (def i (find-valid-positions 9))

  (clojure.pprint/pprint
    (update-in i [:levels] #(map butlast %)))

  (map
    #(count @%)
    (reverse (map last (:levels i))))

  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Starting queens.")
  (let [n 8 solvers 8]
    (println "Executing with:")
    (println (str " - Board size: " n))
    (println (str " - Solver count: " solvers))
    (println (str "Start: " (java.util.Date.)))
    ;;; Run code here and block on its completion.
    (println (str "End: " (java.util.Date.)))
    ;;; Take the difference and output it.
    ;;; Should we write out the results somewhere?
    ))

