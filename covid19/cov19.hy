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

(defn date->doy [date &optional [has-dash False]]
  (sv s (str date)
      pos (if has-dash [0 4 5 7 8 10] [0 4 4 6 6 8])
      yr (int (cut s (get pos 0) (get pos 1)))
      mo (int (cut s (get pos 2) (get pos 3)))
      da (int (cut s (get pos 4) (get pos 5)))
      adpm (scan (fn [acc e] (+ acc e)) [0 31 29 31 30 31 30 31 31 30 31 30 31] 0))
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
                  "AZ" "Arizona" "CO" "Colorado" "LA" "Louisiana" "FL" "Florida"
                  "MA" "Massachusetts" "NJ" "New Jersey" "WA" "Washington"
                  "WI" "Wisconsin" "MI" "Michigan" "GA" "Georgia" "PA" "Pennsylvania"
                  "CT" "Connecticut" "IN" "Indiana" "MN" "Minnesota" "OK" "Oklahoma"
                  "VT" "Vermont" "PR" "Puerto Rico" "OH" "Ohio" "MD" "Maryland"
                  "KY" "Kentucky" "DC" "District of Columbia" "ME" "Maine" "NV" "Nevada"
                  "VA" "Virginia" "GU" "Guam" "DE" "Delaware" "RI" "Rhode Island"
                  "ID" "Idaho" "TX" "Texas" "KS" "Kansas" "HI" "Hawaii" "AK" "Alaska"
                  "AR" "Arkansas" "ND" "North Dakota" "SD" "South Dakota" "IA" "Iowa"
                  "TN" "Tennessee" "NE" "Nebraska" "WY" "Wyoming" "UT" "Utah"
                  "VI" "Virginia" "WV" "West Virginia" "MO" "Missouri" "MT" "Montana"
                  "MS" "Mississippi" "SC" "South Carolina" "NC" "North Carolina"
                  "OR" "Oregon" "NH" "New Hampshire" "AL" "Alabama" "AS" "American Samoa"
                  "MP" "Northern Mariana Islands"}
                 abbrev))
  (get *state-pop* state))

(defn get-state-data []
  (sv fname "daily.json"
      data (parse-daily fname)
      d (organize-daily data))
  d)

(defn rank-by-roughly-current-cfr [d]
  (sv all-states (.keys d)
      states []
      cfrs [])
  (for [state all-states]
    (when (< (last (get d state :deadcum)) 50) (continue))
    (sv den (last (get d state :poscum)))
    (.append states state)
    (.append cfrs (cond [(zero? den) 0]
                        [:else (- (/ (last (get d state :deadcum)) den))])))
  (sv (, cfrs p) (sort-with-p cfrs)
      states (list-comp (get states e) [e p]))
  (dont for [(, s c) (zip states cfrs)] (print s c))
  states)

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

(defn plot-state-data [d soi filename format]
  (sv clrs "krgbmcy"
      namelo (, "daily" "cumulative" "cumulative-abs")
      nameup (, "Daily new" "Cumulative" "Cumulative"))
  (for [yax (range 3)]
    (sv permil (< yax 2)
        nrow (case/eq yax [0 4] [1 5] [2 3]))
    (with [(pl-plot (, 6 (* 3 nrow))
                    (.format "covid19/{}-{}" filename (get namelo yax)) :format format)]
      (for [row (range nrow)]
        (sv ax (pl.subplot nrow 1 (inc row)))
        (for [(, i state) (enumerate soi)]
          (try
            (do
              (sv e (get d state)
                  x (:date e)
                  clr (get clrs (% i (len clrs)))
                  fac (if permil (/ 1e6 (get-state-pop state)) 1)
                  y (case/in yax
                             [(, 0)
                              (get e (get (, :testinc :posinc :deadinc :hospcur) row))]
                             [(, 1 2)
                              (case/in row
                                       [(, 0 1 2)
                                        (get e (get (, :testcum :poscum :deadcum) row))]
                                       [(, 3)
                                        (sv den (:testcum e) mask (= den 0))
                                        (when (npy.any mask)
                                          (sv den (.copy den)
                                              (get den mask) 1))
                                        (/ (:poscum e) den)]
                                       [(, 4)
                                        (sv den (:poscum e) mask (= den 0))
                                        (when (npy.any mask)
                                          (sv den (.copy den)
                                              (get den mask) 1))
                                        (/ (:deadcum e) den)])])
                  pat (get (, "-" "--" ":" "-.") (// i (len clrs))))
              (cond [(or (zero? yax) (in row (, 0 1 2 5)))
                     (semilogy-filter-drops x (* fac y) (+ clr pat) state)]
                    [:else
                     (pl.plot x y (+ clr pat) :label state)]))
            (except [err Exception]
              (print err yax row state))))
        (pl.title (cond [(< row 3)
                         (+ (get nameup yax) " "
                            (get (, "tests" "positive" "deaths") row)
                            (if permil " per million people" ""))]
                        [(and (zero? yax) (= row 3))
                         (+ "Currently hospitalized"
                            (if permil " per million people" ""))]
                         [(= row 3)
                          (.format "{lbl:s} positive / {lbl:s} tests" :lbl (get nameup yax))]
                        [(= row 4)
                         (.format "{lbl:s} deaths / {lbl:s} positive cases"
                                  :lbl (get nameup yax))]))
        (my-grid)
        (when (= row 0)
          (sv xl (pl.xlim)
              xl (, 69 (dec (second xl))))
          (pl.legend :loc "best" :fontsize (if (< (len soi) 8) None 8)
                     :ncol (if (< (len soi) 8) 1 2))
          (dont pl.text 0.7 1.12 "Updated 2 May 2020" :transform ax.transAxes))
        (pl.xticks (range 70 300 10))
        (pl.xlim xl)
        (when (= row (dec nrow))
          (pl.xlabel "Day of year"))
        (when (!= yax 0)
          (case/eq row
                   [3 (pl.ylim 0 0.45)]
                   [4 (sv yl (pl.ylim)) (pl.ylim 0 (min (last yl) 0.1))]))))))

(defn parse-county-data []
  (sv filename "covid-19-data/us-counties.csv"
      d {})
  (with [f (open filename "r")]
    (.readline f)
    (while True
      (sv ln (.readline f))
      (when (empty? ln) (break))
      (try
        (sv (, date county state fips cases deaths) (.split ln ",")
            doy (date->doy date True)
            cases (int cases) deaths (int deaths))
        (except []
          (continue)))
      (sv key (, state county))
      (unless (in (, state county) d)
        (assoc d key {:date [] :case [] :death []}))
      (.append (get d key :date) doy)
      (.append (get d key :case) cases)
      (.append (get d key :death) deaths)))
  d)

(when-inp ["dev-pop"]
  (sv pop (parse-pop))
  (print pop))

(when-inp ["dev-doy"]
  (for [e [20200111 20200211 20200311 20200411 20200531 20200704]]
    (print e (date->doy e)))
  (for [e ["2020-01-11" "2020-02-11" "2020-03-11" "2020-04-11" "2020-07-04"]]
    (print e (date->doy e True))))

(when-inp ["dev-daily"]
  (sv fname "daily.json"
      data (parse-daily fname)
      d (organize-daily data))
  (print (cut data 0 1))
  (print (get d "WI"))
  (with [(pl-plot (, 6 6) "covid19/test")]
    (sv e (get d "WI")
        x (cut (:date e) 1))
    (pl.semilogy x (npy.diff (:testcum e)) "k:"
                 x (npy.diff (:poscum e)) "k--"
                 x (npy.diff (:deadcum e)) "k-")
    (my-grid)))

(when-inp ["p1" {:format str}]
  (plot-state-data (get-state-data) ["NM" "AZ" "CA" "CO" "IL"] "p1" format))

(when-inp ["p1a" {:format str}]
  (plot-state-data (get-state-data) ["GA" "NV" "LA" "TX" "IN" "MI" "FL"] "p1a" format))

(when-inp ["p1b" {:format str}]
  (sv d (get-state-data)
      soi (rank-by-roughly-current-cfr d)
      div (// (len soi) 2))
  (plot-state-data d (cut soi 0 div) "p1hicfr" format)
  (plot-state-data d (cut soi div) "p1locfr" format))

(sv *counties* (, (, "New Mexico" "Bernalillo" 679121)
                  (, "Arizona" "Maricopa" 4485414)
                  (, "California" "Orange" 3185968)
                  (, "Colorado" "Boulder" 326196)
                  (, "Illinois" "Cook" 5150233)
                  (, "California" "Yolo" 220500)
                  (, "California" "Santa Clara" 1927852)
                  (, "New Mexico" "McKinley" 71367)
                  (, "New Mexico" "San Juan" 123958)
                  (, "Colorado" "Douglas" 351154)))

(when-inp ["dev-county"]
  (sv d (parse-county-data)
      coi *counties*)
  (for [i (range (len coi))]
    (sv e (get coi i))
    (print (get d (first e) (second e)))))

(when-inp ["county-stuff"]
  (sv d (parse-county-data)
      n 400
      counties [] deaths [] cases [])
  (for [c (.items d)]
    (.append counties (first c))
    (.append deaths (last (:death (second c))))
    (.append cases (last (:case (second c)))))
  (sv (, - p) (sort-with-p deaths)
      p (reverse p)
      deaths (list-get-list deaths p)
      cases (list-get-list cases p)
      counties (list-get-list counties p))
  (for [i (range n)]
    (sv c (get counties i))
    (prf "{:20s} {:20s} {:8d} {:4.1f}"
         (first c) (second c) (get deaths i)
         (if (> (get cases i) 0)
             (* 100 (/ (get deaths i) (get cases i)))
             -1))))

(when-inp ["plot-county-data" {:format str}]
  (sv d (parse-county-data)
      coi (cut *counties* 0 -4)
      clrs "krgcymb")
  (for [(, im measure) (enumerate (, "daily" "cumulative"))]
    (with [(pl-plot (, 12 6) (+ "covid19/county-" measure) :format format)]
      (for [row (range 2)
            col (range 2)]
        (sv idx (inc (+ (* 2 row) col)))
        (pl.subplot 2 2 idx)
        (for [(, ic county) (enumerate coi)]
          (sv dc (get d (, (first county) (second county)))
              pop (last county)
              x (npy.array (:date dc))
              y (* (npy.array ((if (zero? row) :case :death) dc))
                   (if (= col 1) (/ 1e6 pop) 1)))
          (when (zero? im)
            (sv x (cut x 1)
                y (npy.diff y)))
          (pl.semilogy x y (get clrs ic) :label (second county)))
        (my-grid)
        (sv xl (pl.xlim))
        (pl.xticks (range 70 300 10))
        (pl.xlim 70 (second xl))
        (pl.title (+ (if (zero? im)
                         "Daily new"
                         "Cumulative")
                     " "
                     (get (, "cases" "cases per million people"
                             "deaths" "deaths per million people") (dec idx))))
        (when (= row 1) (pl.xlabel "Day of Year"))
        (when (= idx 3) (pl.legend :loc "best"))))))

(when-inp ["glance" {:format str}]
  (sv d (get-state-data)
      states (.keys d)
      deadcums [])
  (for [rm (, "AS" "MP")]
    (.remove states rm))
  (for [s states]
    (.append deadcums (/ (last (- (get d s :deadcum)))
                         (get-state-pop s))))
  (sv (, - p) (sort-with-p deadcums)
      states (list-get-list states p))
  (for [(, im meas) (enumerate (, "daily" "cumulative"))]
    (sv max-pos-per-capita 0
        max-deaths-per-capita 0
        max-hosp-per-capita 0
        possym (if (zero? im) :posinc :poscum)
        deadsym (if (zero? im) :deadinc :deadcum)
        hospsym :hospcur)
    (for [s states]
      (sv state-pop (get-state-pop s)
          max-pos-per-capita (max max-pos-per-capita
                                  (/ (max (get d s possym)) state-pop))
          max-deaths-per-capita (max max-deaths-per-capita
                                     (/ (max (get d s deadsym)) state-pop))
          max-hosp-per-capita (max max-hosp-per-capita
                                   (/ (max (get d s hospsym)) state-pop))))
    (when (zero? im)
      (sv max-pos-per-capita 8e-4
          max-deaths-per-capita 5e-5))
    (sv fac (/ max-pos-per-capita max-deaths-per-capita)
        fac1 (/ max-pos-per-capita max-hosp-per-capita))
    (with [(pl-plot (, 14 (if (zero? im) 14.25 9.5)) (+ "covid19/glance-" meas) :format format)]
      (for [(, i s) (enumerate states)]
        (sv e (get d s)
            x (:date e)
            pop (get-state-pop s)
            deaths (/ (deadsym e) pop)
            y-deaths (* fac deaths)
            y-pos (/ (possym e) pop)
            y-hosp (* fac1 (/ (hospsym e) pop))
            plot-thin (< (max deaths) (* (if (zero? im) 0.2 0.1) max-deaths-per-capita))
            plot-thin True)
        (when (and (zero? im) (in s (, "KY" "MD" "KS" "IN")))
          (sv (get y-pos (slice 0 15)) 0))
        (pl.subplot 6 9 (inc i))
        (pl.plot x (* 0 x) "k-" x (* max-pos-per-capita (npy.ones (len x))) "k-"
                 :linewidth 0.5)
        (when (zero? im)
          (pl.plot x (* 2 max-pos-per-capita (npy.ones (len x))) "k-"
                   :linewidth 0.5))
        (sv base 0 cnt 2)
        (when (zero? im)
          (when (> (max y-hosp) 0)
            (pl.plot x y-hosp "g-")  
            (when plot-thin
              (pl.plot x (* max-pos-per-capita (/ y-hosp (max y-hosp))) "g-"
                       :linewidth 0.5)))
          (sv base max-pos-per-capita
              cnt 3))
        (pl.plot x (+ base max-pos-per-capita y-deaths) "r-" x (+ base y-pos) "b-")
        (when plot-thin
          (pl.plot x (+ base max-pos-per-capita (* max-pos-per-capita (/ y-deaths (max y-deaths)))) "r-"
                   x (+ base (* max-pos-per-capita (/ y-pos (max y-pos)))) "b-"
                   :linewidth 0.5))
        (pl.xlim (, 70 (inc (last x))))
        (pl.ylim (, (* -0.03 max-pos-per-capita) (* (+ cnt 0.01) max-pos-per-capita)))
        (pl.xticks []) (pl.yticks [])
        (pl.axis "off")
        (pl.text 72 (* (- cnt 0.25) max-pos-per-capita) s :fontsize 12)))))
