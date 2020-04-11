;;; wget https://covidtracking.com/api/v1/states/daily.json

(require [amb [*]])
(import [amb [*]] json)

(defn join-strs [coll]
  (reduce (fn [acc e] (+ acc (if (empty? acc) "" " ") e)) coll ""))

(defn scan [f coll init]
  (sv out [] acc init)
  (for [e coll]
    (sv acc (f acc e))
    (.append out acc))
  out)

(defn parse-pop []
  (sv fname "state-pop.txt"
      txt (.split (readall fname) "\n")
      d {})
  (for [ln txt]
    (when (empty? ln) (continue))
    (when (= (first ln) "#") (continue))
    (sv toks (.split ln "\t")
        state (str (join-strs (cut toks 0 -1)))
        pop (int (last toks)))
    (assoc d state pop))
  d)

(defn parse-daily [fname]
  (with [fp (open fname "r")]
    (json.load fp)))

(defn date->doy [date]
  (sv s (string date)
      yr (int (cut s 0 4))
      mo (int (cut s 4 6))
      da (int (cut s 6 8))
      adpm (scan (fn [acc e] (+ acc e)) [0 31 29 31 30 31 30] 0))
  (+ (* (get adpm (dec mo))) da))

(defn organize-daily [din]
  (sv d {}
      states (list (set (list-comp (str (get e "state")) [e din]))))
  (for [state states]
    (assoc d state {:date [] :deadcum [] :poscum [] :testcum []}))
  (for [e din]
    (try
      (do
        (sv s (get d (get e "state"))
            date (get e "date")
            hospcum (get e "hospitalizedCumulative")
            deadcum (get e "death")
            poscum (get e "positive")
            testcum (get e "total"))
        (.append (:date s) date)
        (.append (:deadcum s) deadcum)
        (.append (:poscum s) poscum)
        (.append (:testcum s) testcum))
      (except [])))
  (for [state (.keys d)]
    (sv d1 (get d state)
        date (list-comp (date->doy e) [e (:date d1)])
        (get d1 :date) date
        (, date p) (sort-with-p date))
    (for [f (.keys d1)]
      (assoc d1 f (get (npy.array (get d1 f)) p))))
  d)

(sv *state-pop* (parse-pop))

(defn get-state-pop [abbrev]
  (sv state (get {"IL" "Illinois" "CA" "California" "NY" "New York" "NM" "New Mexico"
                  "AZ" "Arizona" "CO" "Colorado" "LA" "Louisiana"}
                 abbrev))
  (get *state-pop* state))

(when-inp ["dev-pop"]
  (sv pop (parse-pop))
  (print pop))

(when-inp ["dev-doy"]
  (for [e [20200111 20200211 20200311 20200411]]
    (print e (date->doy e))))

(when-inp ["dev-daily"]
  (sv fname "daily.json"
      data (parse-daily fname)
      d (organize-daily data))
  (print (cut data 0 1))
  (print (get d "CA"))
  (with [(pl-plot (, 6 6) "fig/test")]
    (sv e (get d "IL")
        x (cut (:date e) 1))
    (pl.semilogy x (npy.diff (:testcum e)) "k:"
                 x (npy.diff (:poscum e)) "k--"
                 x (npy.diff (:deadcum e)) "k-")
    (my-grid)))

(when-inp ["p1" {:format string}]
  (sv fname "daily.json"
      data (parse-daily fname)
      d (organize-daily data)
      soi ["CA" "AZ" "NM" "CO" "IL" "LA" "NY"]
      clrs "krgbmcy")
  (for [yax (range 3)]
    (sv namelo (, "daily" "cumulative" "cumulative-abs")
        nameup (, "Daily" "Cumulative" "Cumulative")
        permil (< yax 2))
    (with [(pl-plot (, 6 9) (.format "fig/p1-{}" (get namelo yax)) :format format)]
      (for [row (range 3)]
        (pl.subplot 3 1 (inc row))
        (for [(, i state) (enumerate soi)]
          (sv e (get d state)
              x (case/in yax [(, 0) (cut (:date e) 1)] [(, 1 2) (:date e)])
              clr (get clrs (% i (len clrs)))
              fac (if permil (/ 1e6 (get-state-pop state)) 1)
              y (get e (get (, :testcum :poscum :deadcum) row))
              y (case/in yax [(, 0) (npy.diff y)] [(, 1 2) y])
              pat "-")
          (pl.semilogy x (* fac y) (+ clr pat) :label state))
        (pl.title (+ (get nameup yax) " "
                     (get (, "tests" "positive" "deaths") row)
                     (if permil " per million people" "")))
        (my-grid)
        (when (zero? row) (pl.legend :loc "best"))
        (when (= row 2) (pl.xlabel "Day of year"))))))
