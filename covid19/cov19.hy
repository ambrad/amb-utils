;;; wget https://covidtracking.com/api/v1/states/daily.json

(require [amb [*]])
(import [amb [*]] json)

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
        state (first toks)
        pop (int (last toks)))
    (assert (= (len toks) 2))
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
      states (list (set (list-comp (str (get e "state")) [e din])))
      keys (, :date :hospcur :deadcum :deadinc :poscum :posinc :testcum :testinc)
      strs (, "date" "hospitalizedCurrently" "death" "deathIncrease" "positive"
              "positiveIncrease" "totalTestResults" "totalTestResultsIncrease"))
  (for [state states
        key keys]
    (sv e {})
    (for [k keys] (assoc e k []))
    (assoc d state e))
  (for [e din]
    (sv fnd True)
    (for [s strs]
      (unless (in s e)
        (sv fnd False)
        (break)))
    (unless fnd (continue))
    ;; still here, so have all fields
    (sv s (get d (get e "state")))
    (for [i (range (len keys))]
      (.append (get s (get keys i))
               (get e (get strs i)))))
  (for [state (.keys d)]
    (sv d1 (get d state)
        date (list-comp (date->doy e) [e (:date d1)])
        (get d1 :date) date
        (, date p) (sort-with-p date))
    (for [f (.keys d1)]
      (sv y (npy.array (get d1 f))
          none-mask (= y None)
          (get y none-mask) 0)
      (assoc d1 f (get y p))))
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
  (print (get d "WI"))
  (with [(pl-plot (, 6 6) "fig/test")]
    (sv e (get d "WI")
        x (cut (:date e) 1))
    (pl.semilogy x (npy.diff (:testcum e)) "k:"
                 x (npy.diff (:poscum e)) "k--"
                 x (npy.diff (:deadcum e)) "k-")
    (my-grid)))

(defn semilogy-filter-drops [x y pat label]
  (cond [(npy.any (= y 0))
         (sv s 0 e 0)
         (while (< s (len x))
           (while (and (< e (len x)) (> (get y e) 0)) (inc! e))
           (when (= s e) (inc! s) (inc! e) (continue))
           (if (> e (inc s))
               (do (pl.semilogy (cut x s e) (cut y s e) pat :label label)
                   (sv label None))
               (pl.semilogy (cut x s e) (cut y s e) (+ (first pat) "o")))
           (sv s e))]
        [:else (pl.semilogy x y pat :label label)]))

(when-inp ["p1" {:format string}]
  (sv fname "daily.json"
      data (parse-daily fname)
      d (organize-daily data)
      soi ["CA" "AZ" "NM" "CO" "IL" "LA" "NY"]
      clrs "krgbmcy")
  (for [yax (range 3)]
    (sv namelo (, "daily" "cumulative" "cumulative-abs")
        nameup (, "Daily new" "Cumulative" "Cumulative")
        permil (< yax 2))
    (with [(pl-plot (, 6 (if (zero? yax) 12 9))
                    (.format "fig/p1-{}" (get namelo yax)) :format format)]
      (sv nrow (if (zero? yax) 4 3))
      (for [row (range nrow)]
        (sv ax (pl.subplot nrow 1 (inc row)))
        (for [(, i state) (enumerate soi)]
          (sv e (get d state)
              x (:date e)
              clr (get clrs (% i (len clrs)))
              fac (if permil (/ 1e6 (get-state-pop state)) 1)
              y (case/in yax
                         [(, 0) (get e (get (, :testinc :posinc :deadinc :hospcur) row))]
                         [(, 1 2) (get e (get (, :testcum :poscum :deadcum) row))])
              pat "-")
          (semilogy-filter-drops x (* fac y) (+ clr pat) state))
        (pl.title (+ (if (= row 3) 
                         "Currently hospitalized"
                         (+ (get nameup yax) " "
                            (get (, "tests" "positive" "deaths") row)))
                     (if permil " per million people" "")))
        (my-grid)
        (when (= row 0)
          (sv xl (pl.xlim))
          (when (= nrow 3) (pl.legend :loc "best"))
          (pl.text 0.7 1.12 "Updated 18 April 2020" :transform ax.transAxes))
        (when (= row (dec nrow))
          (pl.xlabel "Day of year"))
        (when (= row 3)
          (pl.legend :loc "best")
          (pl.xlim xl))))))
