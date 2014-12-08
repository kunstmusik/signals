(ns signals.core
  (:import [clojure.lang IDeref Atom]))

(defprotocol Signal
  (get-time-counter [s])
  (changed? [s time-counter])
  (add-time-watcher [s watcher])
  (remove-time-watcher [s watcher]))

(defprotocol MutableSignal
  (reset!*! [s v])
  (swap!*! [s v]))

(defprotocol Reactor
  (signal-updated [x v])
  (deactivate [r])
  (activate [r])
  (activated? [r])
  )

;; extensions
(defn add-time-watcher-impl [obj v]
  (locking obj 
    (let [t (get (meta obj) :time-watchers #{})]
      (alter-meta! obj #(assoc % :time-watchers (assoc t v))))))

(defn remove-time-watcher-impl [obj v]
  (locking obj 
    (let [t (get (meta obj) :time-watchers #{})]
      (alter-meta! obj #(assoc % :time-watchers (disj t v))))))

(defn notify-time-watchers [obj] 
  (doseq [x (get (meta obj) :time-watchers)]
    (signal-updated x obj)))

(defn get-time-impl [obj]
  (locking obj
    (let [t (:time-counter (meta obj))]
      (if t t 0))))

(defn inc-time-impl [obj]
  (locking obj
    (let [t (get (meta obj) :time-counter 0)]
      (alter-meta! obj #(assoc % :time-counter (inc t))))))

(extend-type Atom
  Signal
  (get-time-counter [s] (get-time-impl s))
  (changed? [s time-counter] (= (get-time-impl s) time-counter))
  (add-time-watcher [s watcher]
    (add-time-watcher-impl s watcher))
  (remove-time-watcher [s watcher]
    (remove-time-watcher-impl s watcher)) 
  MutableSignal
  (reset!*! [s v] 
    (locking s 
      (reset! s v) 
      (inc-time-impl s) 
      (notify-time-watchers s)))
  (swap!*! [s v] 
    (locking s 
      (swap! s v) 
      (inc-time-impl s) 
      (notify-time-watchers s))))

;Dynamic argument resolution
(deftype DynamicArg  [func args]) 
(defn !*! 
  [func & args]
  (DynamicArg. func args))

(defn apply!*!
  ([func] (func))
  ([func args]
   (if args
     (->> (if (sequential? args) args [args])
        (map #(if (satisfies? Signal %)
                @%
                %))
        (apply func))
     (func)))
  ([func arg & args]
   (apply!*! func  (list* arg args))))

(defn partial!*!
  [& args]
  (apply partial apply!*! args))

(defn dependencies-updated? 
  [time-cache]
  (some (fn [[a b]] (not= (get-time-counter a) b)) time-cache))

(defn create-reactor [func args]
  (let [sigs (filter #(satisfies? Signal %) args)
        time-cache (volatile! (reduce #(assoc %1 %2 -1) {} sigs))
        cur-value (volatile! nil)
        activate-state (volatile! false)
        ] 
    (reify 
      IDeref 
      (deref [x]  
        (locking x 
          (when (dependencies-updated? @time-cache)
            (let [v (apply!*! func args)]
              (vreset! cur-value v)
              (vreset! time-cache (reduce #(assoc %1 %2 (get-time-counter %2)) {} sigs)))) 
          @cur-value))

      Signal
      (get-time-counter [s] (get-time-impl s))
      (changed? [s time-counter] (= (get-time-impl s) time-counter))
      (add-time-watcher [s watcher]
        (add-time-watcher-impl s watcher))
      (remove-time-watcher [s watcher]
        (remove-time-watcher-impl s watcher)) 

      Reactor
      (signal-updated [x v]
        (inc-time-impl x)
        (notify-time-watchers x))
      (deactivate [r] (doseq [x sigs] remove-time-watcher r))
      (activate [r] (doseq [x sigs] add-time-watcher r))
      (activated? [r] @activate-state)
      )))

(defn r!*! 
  "Creates a Reactor block.  Reactors will signal updates when signals they depend on
  are updated. Values should be dereferenced from the Reactor. Reactors are also Signals
  but not MutableSignals."
  [func & args] 
  (let [reactor (create-reactor func args)]
    (activate reactor)
    reactor)) 

;; testing code

(def a (atom []))

(def b 
  (r!*! 
    into [] 
    (comp 
      (map (comp clojure.string/lower-case :first-name))
      (map clojure.string/lower-case)
      (filter #(< (count %) 4))) 
    a))

(reset!*! 
  a 
  [{:first-name "sue"}
   {:first-name "maria"}
   {:first-name "ted"}
   {:first-name "Bob"} 
   {:first-name "PAN"} 
   ])

(println "First b" @b)

(reset!*! a [
   {:first-name "maria"}
   {:first-name "PAN"} 
   ])

(def print-status 
  (partial!*! println "Latest Users: " b))

(defn print-status2
  []
  (println "Latest Users: " @b))

(def print-status3
  #(println "Latest Users: " @b))

@b
(print-status)
(print-status2)
(print-status3)


(def actions
  [[println "Latest Users: " b]
   [println "Original Value: " a]
   ])

(defn process-actions 
  [actions]
  (doseq [[func & args] actions]
    (apply!*! func args)))
(process-actions actions)



(def input (r!*! read-line))

(defn seq-iter [s] 
  (let [head (atom s)]
    (reify IDeref
      (deref [_] (let [[x & xs] @head] 
                   (reset! head xs)
                   x)))))


(defn range!*! [& args] 
  (seq-iter (apply range args)))

(def t (range!*! 5))

(defn process [src]
  (loop [line 0 v @src]
    (when v 
      (println line ") " v) 
      (recur (inc line) @src))))

(def s (atom 0))

(defn source!*! [sig]
  (fn [] @sig))

(deftype Last [a]
  IDeref
  (deref [_] a))

(defn last!*! 
  [a]
  (Last. a))

(defn is-last? [a]
  (instance? Last a))


(defn pushify
  [src dest]
  (fn [a]
    ;(if (or (= :CONTINUE a) (= :DONE a))
    ;  a
    ; (dest (src a)) )
    (let [v (src a)]
      (if (or (= :CONTINUE v) (= :DONE v) (is-last? v))
        v 
        (dest v)))
    ))

(defn maybe-pushify
  [src dest]
  (fn [a])
  )

((pushify #(- % 5) #(+ 45 %)) 10)

(defn ||> 
  [& args] 
  (let [[a b & c] args
        [pushify-func push-funcs]
        (if (= :pushifier a) 
          [b c]
          [pushify args])
        [x y & z] push-funcs]
    (reduce
      #(pushify-func %1 %2)
      (pushify-func x y)
      z)))

(def comp-chain
  (||> 
     #(+ 45 %)
     #(* 3 %)))

(comp-chain 45)


;; experiments with reductions:
;; so far, doesn't handle EOF, or if func produces multiple outs for single in
;; probably better at this point to just focus on the pushy chain and context

(defn reduce!*! 
  [pform red-fn initial source-sig!*!]
  (loop [accum initial v @source-sig!*!]
    (if v
      (let [pform-v (pform v)] 
        (cond 
          (is-last? pform-v) (red-fn accum @pform-v)
          (= :DONE pform-v) accum 
          (= :CONTINUE pform-v) (recur accum @source-sig!*!)
          :else (recur (red-fn accum pform-v) @source-sig!*!)))
      ;; need to do EOF kind of stuff here
      accum)))

(reduce!*! comp-chain conj [] (range!*! 50))

(defn take!*!
  [num-to-take]
  (let [v (volatile! num-to-take)] 
    (fn [a]
      (let [n (vswap! v dec)]
        (if (> n 0)
          a
          (last!*! a))))))

(defn partition!*! 
  [n]
  (let [counter (volatile! 0)
        v (volatile! [])]
    (fn [a]
      (let [indx (vswap! counter inc)]
        (if (>= indx n)
          (let [out (conj @v a)]
            (vreset! v [])
            (vreset! counter 0)
            out) 
          (do 
            (vswap! v conj a)
            :CONTINUE))))))

(def chain
  (||> 
    #(do (println ">>> " %) %)
    #(* % 10) 
    (partition!*! 3)
    (take!*! 5)
    #(do (println "~~~ " %) %)
    ))

(reduce!*! chain conj [] (range!*! 50))


