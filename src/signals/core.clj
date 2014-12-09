(ns signals.core
  (:import [clojure.lang IDeref Atom]))

;; to be used to do batch firing of signal updates...
(def ^:dynamic batch-update false)

(defprotocol Signal
  (get-time-counter [s])
  (changed? [s time-counter])
  (add-time-watcher [s watcher])
  (remove-time-watcher [s watcher]))

(defprotocol MutableSignal
  (reset!*! [s v])
  (swap!*! [s v]))

(defprotocol Reactor
  (signal-updated [r sig])
  (deactivate [r])
  (activate [r])
  (activated? [r]))

;; UTILITY FUNCTIONS FOR IMPLEMENTING SIGNALS

(defn notify-time-watchers [sig watchers] 
  (doseq [x watchers]
    (signal-updated x sig)))

;; For extended types, use meta map (not ideal, but not sure of other way at the moment)
(defn add-time-watcher-impl [obj v]
  (locking obj 
    (let [t (get (meta obj) :time-watchers #{})]
      (alter-meta! obj #(assoc % :time-watchers (conj t v))))))

(defn remove-time-watcher-impl [obj v]
  (locking obj 
    (let [t (get (meta obj) :time-watchers #{})]
      (alter-meta! obj #(assoc % :time-watchers (disj t v))))))

(defn notify-time-watchers-impl [sig] 
  (when-let [watchers (:time-watchers (meta sig))]
    (notify-time-watchers sig watchers)))

(defn get-time-impl [obj]
  (locking obj
    (get (meta obj) :time-counter -1)))

(defn inc-time-impl [obj]
  (locking obj
    (let [t (get (meta obj) :time-counter -1)]
      (alter-meta! obj #(assoc % :time-counter (inc t))))))

;; EXTENSIONS TO EXISTING TYPES

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
      (notify-time-watchers-impl s)
      v))
  (swap!*! [s v] 
    (locking s 
      (swap! s v) 
      (inc-time-impl s) 
      (notify-time-watchers-impl s)
      @s)))

(defn apply!*!
  ([func] (func))
  ([func args]
   (if args
     (->> (if (sequential? args) args [args])
        (map #(if (instance? IDeref %)
                (deref %)
                %))
        (apply func))
     (func)))
  ([func arg & args]
   (apply!*! func (list* arg args))))

(defn partial!*!
  [& args]
  (apply partial apply!*! args))

(defn dependencies-updated? 
  [time-cache]
  (some (fn [[a b]] (not= (get-time-counter a) b)) time-cache))

(defn create-lazy-signal-reactor [func args]
  (let [sigs (into #{} (filter #(satisfies? Signal %) args))
        time-cache (volatile! (reduce #(assoc %1 %2 -1) {} sigs))
        cur-value (volatile! nil)
        activate-state (volatile! false)
        time-atom (atom -1)
        watchers-atom (atom #{})] 
    (reify 
      IDeref 
      (deref [x]  
        (locking x 
          (when (dependencies-updated? @time-cache)
            (let [v (apply apply!*! func args)]
              (vreset! cur-value v)
              (vreset! time-cache (reduce #(assoc %1 %2 (get-time-counter %2)) {} sigs)))) 
          @cur-value))

      Signal
      (get-time-counter [s] @time-atom)
      (changed? [s time-counter] (= @time-atom time-counter))
      (add-time-watcher [s watcher]
        (swap! watchers-atom conj watcher))
      (remove-time-watcher [s watcher]
        (swap! watchers-atom disj watcher)) 

      Reactor
      (signal-updated [x v]
        (swap! time-atom inc)
        (notify-time-watchers x @watchers-atom))
      (deactivate [r] 
        (doseq [x sigs] (remove-time-watcher x r)) 
        (vreset! activate-state false))
      (activate [r] 
        (doseq [x sigs] (add-time-watcher x r)) 
        (vreset! activate-state true))
      (activated? [r] @activate-state)

      Object
      (toString [r] "Lazy Signal Reactor")
      )))

(defn create-signal-reactor [func args]
  (let [sigs (into #{} (filter #(satisfies? Signal %) args))
        time-cache (volatile! (reduce #(assoc %1 %2 -1) {} sigs))
        cur-value (volatile! nil)
        activate-state (volatile! false)
        time-atom (atom -1)
        watchers-atom (atom #{})] 
    (reify 
      IDeref 
      (deref [x] @cur-value)

      Signal
      (get-time-counter [s] 
        (locking s 
          @time-atom))
      (changed? [s time-counter] (= @time-atom time-counter))
      (add-time-watcher [s watcher]
        (swap! watchers-atom conj watcher))
      (remove-time-watcher [s watcher]
        (swap! watchers-atom disj watcher)) 

      Reactor
      (signal-updated [x v]
        (locking x 
          (let [v (apply apply!*! func args)]
            (vreset! cur-value v)
            (vreset! time-cache (reduce #(assoc %1 %2 (get-time-counter %2)) {} sigs)))
        (swap! time-atom inc))
        (notify-time-watchers x @watchers-atom))
      (deactivate [r] 
        (doseq [x sigs] (remove-time-watcher x r)) 
        (vreset! activate-state false))
      (activate [r] 
        (doseq [x sigs] (add-time-watcher x r)) 
        (vreset! activate-state true))
      (activated? [r] @activate-state)

      Object
      (toString [r] "Signal Reactor")
      )))


(defn create-reactor [func args]
  (let [sigs (into #{} (filter #(satisfies? Signal %) args))
        activate-state (volatile! false)] 
    (reify 
      Reactor
      (signal-updated [x v]
        (apply!*! func args))
      (deactivate [r] 
        (doseq [x sigs] (remove-time-watcher x r)) 
        (vreset! activate-state false))
      (activate [r] 
        (doseq [x sigs] (add-time-watcher x r)) 
        (vreset! activate-state true))
      (activated? [r] @activate-state)

      Object
      (toString [r] "Event Reactor")

      )))

(defn r!*! 
  "Creates a Signal Reactor block.  Reactors will signal updates when signals
  they depend on are updated. New values are calculated on update signal from 
  dependency.  Signal Reactors are Signals but not MutableSignals. This should
  be used for values that are coordinated with a reactive graph."
  [func & args] 
  (let [reactor (create-signal-reactor func args)]
    (activate reactor)
    reactor)) 

(defn l!*! 
  "Creates a Lazy Signal Reactor block.  Reactor will signal updates when
  signals they depend on are updated. New value will not be calculated until
  first deref. Lazy Signal Reactors are Signals but not MutableSignals. 
  This should be used for situations where reads are not coordinated with 
  reactive graph and less frequent than writes to l!*! block."
  [func & args] 
  (let [reactor (create-lazy-signal-reactor func args)]
    (activate reactor)
    reactor)) 

(defn d!*!
  "Creates a deref block from time-varying function.  Each deref results in an
  apply!*! of given func and args.  These are PullSignals, and are not Signals
  nor Reactor. These are not used for reactive purposes, though may be a part of 
  a reactive graph."
  [func & args]
  (reify 
      IDeref
      (deref [s] 
        (apply!*! func args))))

(def !*! d!*!)

(defn e!*!
  "Creates an event block using a function with time-varying args. 
  This is not a Signal, but is a Reactor, and will fire its functions and args when
  an update is triggered by signal dependencies."
  [func & args]
  (let [reactor (create-reactor func args)]
    (activate reactor)
    reactor))

;; testing code

(def a (atom []))
(def e (e!*! println "TEST>>>" a))

(def b 
  (r!*! 
    into [] 
    (comp 
      (map (comp clojure.string/lower-case :first-name))
      (map clojure.string/lower-case)
      (filter #(< (count %) 4))) 
    a))

(def l 
  (r!*! 
    into [] 
    (comp 
      (map (comp clojure.string/lower-case :first-name))
      (map clojure.string/lower-case)
      (filter #(< (count %) 4))) 
    a))

(println "First b" @b)
(println "First l" @l)

(reset!*! 
  a 
  [{:first-name "sue"}
   {:first-name "maria"}
   {:first-name "ted"}
   {:first-name "Bob"} 
   {:first-name "PAN"} 
   ])

(println "second b" @b)
(println "second l" @l)

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

(println "X: " @b @l)
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


(def input (d!*! read-line))

(defn seq->signal [s] 
  (let [head (atom s)]
    (reify IDeref
      (deref [_] (let [[x & xs] @head] 
                   (reset! head xs)
                   x)))))

(defn range!*! [& args] 
  (seq->signal (apply range args)))

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


;; experiments with reductions, iteratees:
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


