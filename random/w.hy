(require [amb3 [*]])
(import [amb3 [*]])

(defn get-context []
  {:word-file "/usr/share/dict/words"})

(defn get-valid-word-list [word-file &optional [wlen 6]]
  (sv d-all (.split (readall word-file) "\n")
      d-v [])
  (for [w d-all]
    (when (or (!= (len w) wlen) (in "'" w) (<= (first w) "Z"))
      (continue))
    (.append d-v w))
  d-v)

(defn sort-by-letter-score [words &optional [verbose False]]
  (sv freqs {})
  (for [w words]
    (for [c w]
      (if (in c freqs)
          (inc! (get freqs c))
          (assoc freqs c 0))))
  (when verbose
    (sv keys (sort (list (.keys freqs))))
    (for [k keys]
      (print k (get freqs k))))
  (sv scores [])
  (for [w words]
    (sv score 0)
    (for [c w]
      (+= score (get freqs c)))
    (.append scores score))
  (sv perm (second (sort-with-p scores)))
  (.reverse perm)
  (sv ws [])
  (for [p perm]
    (.append ws (get words p)))
  (sv wsu [] wsn [])
  (for [i (range (len (first words)))]
    (for [w ws]
      (when (= (len (set w)) (- (len w) i))
        (.append wsn w))))
  (sv out (+ wsu wsn))
  (assert (= (len out (len words))))
  out)

(defclass Game []
  (defn --init-- [me words word]
    (assert (in word words))
    (sv me.words words
        me.word word
        me.K (len me.word)
        me.nturn 999
        me.state 'play me.cnt 0))

  (defn input [me word &optional [quiet False]]
    (inc! me.cnt)
    (unless (and (= (len me.word) me.K))
      (me.you-lose (.format "word '{}' doesn't have {} letters" word me.K))
      (return))
    (sv in-list [] at-list [] out-list [] ln "")
    (for [(, i c) (enumerate word)]
      (cond [(= (get me.word i) (get word i))
             (.append at-list (, i c))
             (+= ln "a")]
            [(in c me.word)
             (.append in-list c)
             (+= ln "i")]
            [:else
             (.append out-list c)
             (+= ln " ")]))
    (unless quiet
      (print word)
      (print ln))
    (when (= (len at-list) me.K)
      (me.you-win :quiet quiet))
    (when (= me.cnt me.nturn)
      (me.you-lose "out of turns"))
    (, at-list in-list out-list))

  (defn still-playing? [me] (= me.state 'play))

  (defn turns [me] (return me.cnt))

  (defn you-win [me &optional [quiet False]]
    (sv me.state 'win)
    (unless quiet (print "win" me.cnt)))
  
  (defn you-lose [me msg]
    (sv me.state 'lose)
    (print "lose:" msg)))

(defclass Player []
  (defn --init-- [me words]
    (sv me.words words me.in (set) me.out (set) me.at (set) me.idx 0))

  (defn turn [me]
    (for [idx (range me.idx (len me.words))]
      (sv w (get me.words idx)
          c False)
      (for [a me.at]
        (unless (= (get w (first a)) (second a))
          (sv c True) (break)))
      (when c (continue))
      (for [i me.in]
        (unless (in i w)
          (sv c True) (break)))
      (when c (continue))
      (for [o me.out]
        (unless (not (in o w))
          (sv c True) (break)))
      (when c (continue))
      (sv me.idx (inc idx))
      (return w))
    (assert False))

  (defn inform [me at-list in-list out-list]
    (.update me.in in-list)
    (.update me.out out-list)
    (.update me.at at-list)))

(defn play [idx]
  (sv c (get-context)
      d-v (get-valid-word-list (:word-file c))
      word (get d-v idx)
      d-v (sort-by-letter-score d-v)
      g (Game d-v word)
      p (Player d-v))
  (print (len d-v))
  (print ">" word)
  (while (g.still-playing?)
    (sv word (p.turn)
        out (g.input word))
    (p.inform #*out)))

(when-inp ["print-valid-dict"]
  (sv c (get-context)
      d-v (get-valid-word-list (:word-file c)))
  (for [w d-v]
    (print w)))

(when-inp ["dev"]
  (sv c (get-context)
      d-v (get-valid-word-list (:word-file c))
      d-v (sort-by-letter-score d-v :verbose True))
  (print (cut d-v 0 100))
  (print (cut d-v -100 None)))

(when-inp ["play" {:idx int}]
  (play idx))

;; play every game and sort by number of turns.
(when-inp ["stats"]
  (sv c (get-context)
      d-v (get-valid-word-list (:word-file c))
      d-v (sort-by-letter-score d-v)
      stats [])
  (print (len d-v))
  (for [i (range (len d-v))]
    (sv word (get d-v i)
        g (Game d-v word)
        p (Player d-v))
    (while (g.still-playing?)
      (sv word (p.turn)
          out (g.input word :quiet True))
      (p.inform #*out))
    (.append stats (, word (g.turns))))
  (.sort stats :key second)
  (for [s stats]
    (print (first s) (second s))))
