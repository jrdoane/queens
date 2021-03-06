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
  [n tracker incoming next-level]
  (s/connect
    (s/map
      #(do (swap! tracker update-in (first %)
                  (fn [i] (if (nil? i) {} i)))
           (second %))
      (s/filter
        ;;; If there is no value at that position in the map, we can continue.
        #(nil? (get-in @tracker (first %)))
        (s/map
          ;;; Sort the queens set and put that at the beginning of the list.
          #(list (sort (first %)) %)
          incoming)))
    next-level))

(defn level-pairs
  [n levels tracker]
  (loop [result []
         [current-level next-level & r] levels]
    (if (not current-level)
      result
      (let [incoming (s/stream 50)]
        (if (not (nil? next-level))
          (keep-non-duplicates n tracker incoming next-level)
          (s/on-drained current-level #(s/close! incoming)))
        (recur
          (conj result (list current-level
                             (when (not (nil? next-level)) incoming)))
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
  [n solvers]
  (let [invalid-map (make-invalid-map n)
        tracker (atom {})
        levels (reverse (level-pairs n (take n (repeatedly #(s/stream 1e4))) tracker)) 
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

    (let [solvers (doall (take solvers (repeatedly #(start-solver! n invalid-map levels valid))))]
      {:valid valid :levels levels
     :solvers solvers 
     :result results :tracker tracker
     :complete? (apply d/zip solvers)})))

(comment

  (def all (all-positions 8))
  (def f1 (generate-all-invalid 8 [0 0]))
  (def r (clojure.set/difference all f1))
  (def f2 (generate-all-invalid 8 (first f1)))
  (def r2 (clojure.set/difference r f2))

  (def i (find-valid-positions 9))

  (clojure.pprint/pprint
    (dissoc i :tracker)
    )

  (map
    #(count @%)
    (reverse (map last (:levels i))))

  )

(defn -main
  "I don't do a whole lot ... yet."
  [& [n solvers]]
  (println "Starting queens.")
  (println "Executing with:")
  (println (str " - Board size: " n))
  (println (str " - Solver count: " solvers))
  (let [start-time (java.util.Date.)]
    (println (str "Start: " start-time))
    (time @(:complete? (find-valid-positions (Integer. n) (Integer. solvers))))
    (println (str "End: " (java.util.Date.)))))

