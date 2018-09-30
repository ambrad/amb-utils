;;; Collection of utils.

(import sys)

(defmacro sdo [&rest code]
  "Scoped do. Just like do, but vars def'ed inside stay there."
  `((fn () ~@code)))

(defmacro sv [&rest code]
  "The ultimate in laziness."
  `(setv ~@code))

(defmacro svifn [var val]
  `(if (none? ~var) (sv ~var ~val)))

(defmacro/g! expect [expr &optional [answer True]]
  (setv expr-code (str expr))
  (setv answer-code (str answer))
  `(sdo (setv ~g!got ~expr)
        (setv ~g!want ~answer)
        (if (not (= ~g!got ~g!want))
          (print (.format "ERROR: {0:s} = {1:s} NOT EQUAL TO {2:s} = {3:s}"
                          ~expr-code (str ~g!got) ~answer-code (str ~g!want))))))

(defmacro/g! in-require [expr]
  (setv expr-code (str expr))
  `(sdo (setv ~g!value ~expr)
        (unless ~g!value
          (sdo (import inspect)
               (setv ~g!frame (inspect.currentframe)
                     ~g!file (. ~g!frame f-code co-filename)
                     ~g!lineno (. ~g!frame f-lineno))
               (raise (Exception (.format "IN-REQUIRE {:s} {:d}: {:s}"
                                          ~g!file ~g!lineno ~expr-code)))))))

(defmacro dont [&rest code]
  "No-op."
  `((fn ())))

(defmacro raisefmt [&rest args]
  `(raise (Exception (.format ~@args))))

(defmacro interact [&rest code]
  "Block code for interactive eval, but that is silenced when the .hy file is
  run as a program."
  `(if (= --name-- "__main__")
    (dont ~@code)
    (do ~@code)))

(defmacro if-main [&rest code]
  "Block code when run with a main, but silence on import."
  `(if (= --name-- "__main__")
    (do ~@code)
    (dont ~@code)))

(defmacro prf [&rest args]
  `(print (.format ~@args)))
(defmacro prfno [&rest args]
  `(print (.format ~@args) :end ""))
(defmacro prff [fid &rest args]
  `(print (.format ~@args) :file ~fid))
(defmacro prffno [fid &rest args]
  `(print (.format ~@args) :end "" :file ~fid))

(defmacro mapply [func &rest args]
  "Apply func. Works for keyword args, unlike apply. (Probably broken in many
  ways compared with apply, however.)"
  `(~func ~@args))

(defmacro dispatch-dict [d m]
  "Function object dispatch helper."
  `(try (get ~d ~m)
        (except [] (print "No function" (name ~m)) None)))

(defmacro/g! inc! [x &optional [increment 1]]
  `(do (setv ~x (+ ~x ~increment))
       ~x))

(defmacro/g! case/test [op keyform &rest entries]
  "Case based on a test with op. Use :else as default action."
  `(do (setv ~g!keyform-result ~keyform)
       (cond ~@(map (fn [entry]
                      (+ (if (= (first entry) ':else)
                           '[True] ; If :else, use True in cond.
                           `[(~op ~g!keyform-result ~(first entry))])
                         `[~@(rest entry)]))
                    entries)
             ;; If no case matches, return None.
             [True None])))

(defmacro/g! case/eq [&rest forms]
  "Case using = for the key. Thus, the 'keylist' is not really a list, but an
  atom."
  `(case/test = ~@forms))

(defmacro/g! case/in [&rest forms]
  "Case using 'in' for the key. Thus, each keylist *must* indeed be a list."
  `(case/test in ~@forms))

(defmacro geton [&rest forms]
  "Like get, but return instead of raising KeyError on failure."
  `(try (get ~@forms)
        (except [] None)))

(defn ppme [quoted-form]
  (defn pr [&rest args] (print #*args :end ""))
  (defn prl [e indent ldelim rdelim]
    (prf "{:s}" ldelim)
    (for [li e] (rec li (inc indent)))
    (pr (* indent " "))
    (prf "{:s}" rdelim))
  (defn rec [e indent]
    (setv t (type e))
    (pr (* indent " "))
    (case/eq t
             [HyExpression (prl e indent "(" ")")]
             [HyList (prl e indent "[" "]")]
             [:else (print e)]))
  (setv h (macroexpand quoted-form))
  (print h)
  (rec h 0))

(dont ppme '(case/in 'hi (['hi 'bye] 'hi)))

(defclass Box []
  "A box to hold values to be written in closures."
  (defn --repr-- [me]
    (str me.--dict--)))
(defn class? [o] (= (type o) (type (Box))))
(defn has-field? [o field] (.has-key o.--dict-- field))
(defn pod-number? [n] (in (type n) (, int long float)))
(defn list? [coll] (= (type coll) list))
(defn dict? [coll] (= (type coll) dict))

(defmacro/g! box-slots [&rest slots]
  "Example: (setv hi 3 bye \"so long\" b (box-slots 'hi 'bye))"
  `(do (setv ~g!box (Box))
       ~@(map (fn [s]
                (setv g!field (string (second s)) ; handles _ vs - in names
                      g!value (second s))         ; inject symbol directly
                `(assoc (. ~g!box --dict--) ~g!field ~g!value))
              slots)
       ~g!box))

(expect (sdo (setv hi "so long" foo-bar 3 b (box-slots 'hi 'foo-bar))
             b.foo-bar)
        3)

(defn strleq (s ref)
  (if (< (len s) (len ref))
    False
    (= (cut s 0 (len ref)) ref)))

(defn assoc-nested [d keys val]
  "Associate in a nested dict, creating new sub-dicts as needed."
  (setv dn d)
  (for [key (cut keys 0 -1)]
    (if-not (.has_key dn key)
            (assoc dn key {}))
    (setv dn (get dn key)))
  (assoc dn (get keys -1) val))

;; Very nice for writing .py files from C++ with data, and then running in a
;; parse/plot program to load the data.
(defn get-python-mod [filename &optional [basedir ""]]
  "Return the module for basedir/filename."
  (defn get-module-basename [fn]
    (import os)
    (setv name ((. os path basename) fn))
    (get (.split name ".") 0))
  (setv name (get-module-basename filename))
  (if (not (empty? basedir))
    (+= basedir "."))
  (import importlib)
  (importlib.import-module (.format "{:s}{:s}" basedir name)))

;; Set up the path so that os.* calls know where to find .so files.
(defn set-ld-path []
  (import os re)
  (setv (, _ stdout) (os.popen2 ". ~/sems/source.sh; env|grep LD_LIBRARY_PATH")
        paths (stdout.read)
        paths (cut paths (inc (.find paths "=")) -1)
        (get os.environ "LD_LIBRARY_PATH") paths))

;; Probably already exists, but haven't looked.
(defn unzip [coll-of-tups]
  "Unzip [(a1 b1 ...) (a2 b2 ...) ...] into [[a1 a2 ...] [b2 b2 ...] ...]."
  (if (empty? coll-of-tups)
    coll-of-tups
    (do (setv uz [])
        (for [i (range (len (get coll-of-tups 0)))]
          (.append uz (list (list-comp (get e i) [e coll-of-tups]))))
        uz)))

;; Basic cmp function that wrappers can call.
(defn cmp-fn [a b]
  (int (cond [(< a b) -1]
             [(> a b) 1]
             [:else 0])))

(defn sort [coll]
  "Functional sort."
  (import copy)
  (setv c (copy.deepcopy coll))
  (.sort c)
  c)

(defn safe-len [x]
  (try (len x) (except [] 1)))

(defn sscanf [str-to-parse fmt &optional split]
  """
  Kind of like sscanf. Format is like this: 'i,if,f,s', where if is for
  int(float(.)).
  """
  (setv str2type {"i" int
                  "if" (fn [x] (int (float x)))
                  "f" float
                  "s" str})
  (setv ts (list-comp (get str2type s)
                      [s (.split fmt ",")]))
  (list-comp (t s)
             [(, t s) (zip ts (.split str-to-parse split))]))

(defn split-convert [ln conversion]
  "Example:
      (split-convert \"COMPOSE> ne 24 nmax 480 qsize 5 slmpi 1\"
                     \"ssisisisi\")
   'conversion' can be shorter than (.split ln), in which case the remainder
   of line is omitted."
  (try
   (list-comp ((get {"i" int "s" identity "f" float}
                    (get conversion i)) tkn)
              [(, i tkn) (enumerate (cut (.split ln) 0 (len conversion)))])
   (except []
     (prf "split-convert failed on:\n  {:s}\nlen ln {:d} len conversion {:d}"
          ln (len (.split ln)) (len conversion)))))

(import copy math)

(defn mean [coll]
  (/ (sum coll) (len coll)))

(defn median [coll]
  (setv c (list (copy.deepcopy coll)))
  (.sort c)
  (if (odd? (len c))
    (get c (int (math.floor (/ (len c) 2))))
    (sdo (setv i (dec (int (/ (len c) 2))))
         (mean (cut c i (+ i 2))))))

(defn variance [coll]
  (setv mu (mean coll))
  (/ (sum (map (fn [e] (** (- e mu) 2)) coll))
     (len coll)))

(defn grep-str [pattern str]
  (import re)
  (re.findall (+ "(?:" pattern ").*") str :flags re.MULTILINE))

(defn grep [pattern filename]
  (grep-str pattern (with [f (open filename "r")] (f.read))))

(defn re-split-convert [converts pat ln]
  "Ex: (re-split-convert (, int float) regex ln)"
  (try
   (list-comp (convert e)
              [(, convert e) (zip converts
                                  (first (re.findall pat ln)))])
   (except [e Exception]
     (print ln))))

(defn ooa [y &optional [xfac 2] [x None]]
  (setv r [])
  (for [i (range (dec (len y)))]
    (.append r (/ (- (math.log (get y i)) (math.log (get y (inc i))))
                  (if x
                    (- (math.log (get x i)) (math.log (get x (inc i))))
                    (math.log xfac)))))
  r)

(setv *when-inp-verbosity* 2)

(defn inp [name]
  (setv b (in name sys.argv))
  (when (or (and b (> *when-inp-verbosity* 0))
            (> *when-inp-verbosity* 1))
    (prf "{:s}: {:s}" (if b "DO" "av") name))
  b)

(defmacro/g! when-inp [fn-name-args &rest body]
  "Example:
     (when-inp [\"hi\" {:hi int :bye float}]
     (print hi bye))"
  (if (> (len fn-name-args) 2)
    (raise (Exception "(when-inp [fn-name &optional args] body)")))
  (unless (or (= (len fn-name-args) 1)
              (is (type (second fn-name-args)) hy.models.HyDict))
    (raise (Exception "args must be a dict")))
  (setv fn-name (first fn-name-args)
        args (if (> (len fn-name-args) 1) (second fn-name-args)))
  (defn dissect-args [args]
    (setv alist [] arg-str "")
    (for [(, i e) (enumerate (zip (cut args 0 (len args) 2)
                                  (cut args 1 (len args) 2)))]
      (.append alist
               `(setv
                 ;; grab "kw" from ":kw" and make it a symbol
                 ~(HySymbol (cut (first e) 2))
                 ;; apply type conversion
                 (try (~(second e) (get sys.argv (+ ~i 2)))
                      (except []
                        (.format "Could not parse sys.argv {:d}: {:s}"
                                 (+ ~i 2)
                                 (get sys.argv (+ ~i 2)))))))
      (+= arg-str (+ " " (name (first e)) ": " (name (second e)))))
    (, alist arg-str))
  (if args
    (do
     (setv (, alist arg-str) (dissect-args args))
     `(sdo
       (import amb)
       (setv ~g!b (in ~fn-name sys.argv))
       (when (or (and ~g!b (> amb.*when-inp-verbosity* 0))
                 (> amb.*when-inp-verbosity* 1))
         (prf "{:s}: {:s}:{:s}" (if ~g!b "DO" "av")
              ~fn-name
              ~arg-str))
       (when ~g!b
         (if (< (- (len sys.argv) 2) (len ~args))
           (raise (Exception (+ "in " ~fn-name
                                " args has more entries than"
                                " are available in sys.argv"))))
         ~@alist
         ~@body)))
    `(sdo (when (inp ~fn-name) ~@body))))

(if-main
 (when-inp ["hi" {:bye int :aye float}] (print bye))
 (when-inp ["hello"] (print "hello")))

;;; Numpy utils.

(try
 (do
  (import [numpy :as npy])

  (defn array-range [&rest args]
    (npy.array (list (apply range args)) :dtype int))

  (defn sort-with-p [ai]
    "Return sorted ai and permutation array. Each entry of a must have the same
    type."
    (if (empty? ai)
      (, ai [])
      (do (setv dtype [(, (str "data") (type (get ai 0))) (, (str "p") int)]
                a (npy.array (list-comp (, e i) [(, i e) (enumerate ai)])
                            :dtype dtype))
          (setv a (npy.sort a :order (str "data")))
          (tuple (unzip a)))))

  (defn epsilon [&optional [type float]]
    (. (npy.finfo type) eps))

  (defn np-map [f a]
    (npy.array (list (map f a))))

  (defn dbg-array->np-array [a m n]
    (npy.transpose (npy.reshape (npy.array a) (, n m))))

  (defn reldif [a b]
    (if (and (pod-number? a) (pod-number? b))
      (/ (abs (- a b)) (max (abs a) (abs b)))
      (sdo (setv aa (npy.array a)
                 ba (npy.array b))
           (/ (npy.linalg.norm (- aa ba)) (npy.linalg.norm aa)))))

  (defn np-set-print []
    (setv float-format (fn [x]
                         (cond [(zero? x) (+ " ." (* " " 8))]
                               [(= x 1) (+ " 1" (* " " 8))]
                               [:else (.format "{:10.3e}" x)]))
          complex-format (fn [x]
                           (cond [(zero? x) (+ " ." (* " " 19))]
                                 [(= x 1) (+ " 1" (* " " 19))]
                                 [:else (if (zero? x.imag)
                                          (.format (+ "{:10.3e}" (* " " 11)) x.real)
                                          (.format "{:21.3e}" x))]))
          int-format (fn [x]
                       (if (zero? x)
                         (+ " ." (* " " 1))
                         (.format "{:2d} " x))))
    (npy.set-printoptions
     :precision 2
     :linewidth 1000
     :formatter {"float" float-format
                 "complexfloat" complex-format
                 "int" int-format}))

  (defn triple-read-file [fn]
    (setv triple [])
    (with [f (open fn "r")]
      (while True
        (setv ln (f.readline))
        (if (empty? ln) (break))
        (setv d (sscanf ln "i,i,f"))
        (.append triple (tuple d))))
    triple)

  (defn triple->dense [triple &optional [base 0]]
    (setv (, row col val) (unzip triple)
          A (sdo (setv d (if (= base 0) 1 0)
                       M (+ (max row) d)
                       N (+ (max col) d))
                 (npy.zeros (, M N))))
    (for [e triple]
      (setv r (get e 0)
            c (get e 1))
      (if (> base 0)
        (setv r (- r base)
              c (- c base)))
      (setv v (get e 2)
            (get A r c) v))
    A)

  (defn dense-extract-block-diag [A bs]
    (setv D (npy.zeros (npy.shape A)))
    (for [br (range (// (npy.size A 0) bs))]
      (setv r0 (* br bs)
            cs (list (range r0 (+ r0 bs))))
      (for [i (range bs)]
        (setv r (+ r0 i)
              (get (get D r) cs) (get (get A r) cs))))
    D)

  (defn dense-max-norm [A]
    (setv m1n 0)
    (for [r (range (npy.size A 0))]
      (setv r1n (as-> (get A r) it
                      (npy.abs it)
                      (npy.sum it))
            m1n (max m1n r1n)))
    m1n)

  (defn pod-number? [n]
    (in (type n) (, int long float npy.int64 npy.float64)))

  (defn np-array? [a] (= (type a) npy.ndarray))

  (defn conforms? [v u]
    (and (np-array? u) (= u.shape v.shape)))

  (defn s-all [] (slice None))
  (defn s-all-rev [] (slice None None -1))

  (defn antidiag [v]
    (get (npy.diag (get (npy.array v) (s-all-rev))) (s-all-rev)))
  )
 (except []
   (do
    (defn np-array? [a] False)
    )))

;;; Matplotlib utils.

(try
 (do
  (import matplotlib [matplotlib.pyplot :as pl])

  (defn my-grid []
    (pl.grid True :lw 0.5 :ls "-" :color (, 0.8 0.8 0.8) :zorder -1
             :which "both")
    (.set_axisbelow (pl.gca) True))

  (defn dispfig (&optional fn-prefix [format "pdf"] [tight True])
    (if tight (pl.tight_layout))
    (if (or (not fn_prefix) (empty? fn-prefix))
      (pl.show)
      (pl.savefig (+ fn-prefix (+ "." format))
                  :format format :bbox-inches "tight")))

  (defclass pl-plot []
    (defn --init-- [me figsize filename &optional [format None]
                    [tight True]]
      (setv me.filename filename
            me.format (if (none? format) "pdf" format)
            me.tight tight)
      (pl.close)
      (pl.figure :num 1 :figsize figsize))
    (defn cleanup [me]
      (dispfig me.filename :format me.format :tight me.tight))
    (defn --enter-- [me] me)
    (defn --exit-- [me &rest args])
    (defn --del-- [me] (me.cleanup)))

  ;; To get Type 1 fonts only. From
  ;;     http://nerdjusttyped.blogspot.com/2010/07/type-1-fonts-and-matplotlib-figures.html
  ;; The third one in particular really blows up rendering time, so switch this
  ;; block to False during development iterations.
  (defn pl-require-type1-fonts []
    (import matplotlib)
    (assoc matplotlib.rcParams
           "ps.useafm" True
           "pdf.use14corefonts" True
           "text.usetex" True))

  (defn imshow-matrix [A]
    (pl.imshow A :interpolation "none")
    (pl.show))

  (defn iml [A]
    (pl.imshow (npy.log10 (npy.abs A)) :interpolation "none"))

  (defn pad-lim [lim &optional [pad 0.05] [mult False]]
    (if mult
      (do (, (* (first lim) (- 1 pad))
             (* (second lim) (+ 1 pad))))
      (do (setv d (- (second lim) (first lim))
                delta (* pad d))
          (, (- (first lim) delta)
             (+ (second lim) delta)))))

  (defn axis-tight-pad [&optional [pad 0.05] [mult False]]
    (pl.axis "tight")
    (setv xl (pl.xlim) yl (pl.ylim))
    (pl.xlim (pad-lim xl pad mult))
    (pl.ylim (pad-lim yl pad mult)))

  (defn reset-colors []
    (.set-color-cycle (pl.gca) None))

  (defn good-subplot-dims [n &optional [pref-horiz False]]
    (setv d (cond [(< n 5) (case/eq n [1 (, 1 1)] [2 (, 2 1)]
                                    [3 (, 3 1)] [4 (, 2 2)])]
                  [(< n 7) (, 3 2)]
                  [(< n 10) (, 3 3)]
                  [(< n 13) (, 4 3)]
                  [(< n 17) (, 4 4)]
                  [:else (, 5 (int (math.ceil (/ n 5))))]))
    (if pref-horiz
      (, (second d) (first d))
      d))

  (defn get-linestyle-word [char]
    (get {"-" "solid" "--" "dashed" ":" "dotted" "-." "dashdot"} char))

  (defn set-tick-fontsize [fs]
    (for [ax (, "xaxis" "yaxis")]
      (sv ticks ((. (get (. (pl.gca) --dict--) ax)
                    get-major-ticks)))
      (for [tick ticks]
        ((. tick label set-fontsize) fs)))))

 (except [] ))

;;; More extensive unit tests.

(expect
 (case/eq 'hi
          ('hit 'nope)
          ('hi 'bark 'yep))
 'yep)
(expect
 (case/in 'hello
          ['(bye hello) 'nope]
          ('(hi) 'yep)
          ('(4) 'another-nope))
 'nope)
(expect
 (case/in 'hi
          ('(bye hello) 'nope)
          ('(hi) 'yep)
          ('(4) 'another-nope))
 'yep)
(expect
 (case/in 'hi
          ('(bye hello) 'nope)
          ('hi 'yep)
          ('(4) 'another-nope))
 'yep)
(expect
 (case/test in 'hi
            ('(bye hello) 'nope)
            ('hi 'yep)
            ('(4) 'another-nope))
 'yep)
(expect (case/eq 4 [5 'bye] [4 'hi])
        'hi)
(expect
 (case/in 'hi
          ('(bye hello) 'nope))
 None)
(expect
 (case/in 'hi
          ('(bye hello) 'nope)
          (:else 'woot))
 'woot)
(expect
 (sdo (setv key 'hi)
      (case/in key
               ('(bye hello) 'nope)
               ([key] 'yup)))
 'yup)
