(ns signals.core
  (:require [signals.protocols :refer :all])
  (:import [clojure.lang IDeref IRef]))

;; TODO 
;; * Activate/Deactivate should bubble up throughd dependents
;; * dependents need to check if all dependencies are activated to activate themselves
;; * use extend-type to do activate/deactivate on Atom, Ref, etc.

;; to be used to do batch firing of signal updates...
;; (def ^:dynamic batch-update false)

(defn apply!*!
  "Apply function with dynamic args (IDerefs).  Before functions are applied, arguments 
  are checked to see if they are instances of IDeref.  If so, deref the item to get its
  value and use that as the argument, otherwise use the argument as-is."
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

(defn- notify-watches 
  "Notifies watches with key, ref, old-val, new-val. Watches should be a map."
  [watches r o n]
  (doseq [[k watch-fn] watches]
    (watch-fn k r o n )))

(defn- create-lazy-signal-reactor [func args]
  (let [sigs (into #{} (filter #(instance? IDeref %) args))
        cur-value (atom (delay (apply!*! func args)))
        activate-state (volatile! false)
        watches-atom (atom {})
        ref-atom (atom nil) ;; chicken and egg...
        watch-fn (fn [k r o n] 
                    (reset! cur-value (delay (apply!*! func args)))
                    (notify-watches @watches-atom @ref-atom nil nil))
        r (reify 
            IDeref 
            (deref [x]  
              @(deref cur-value))

            IRef
            (setValidator [x validator])
            (getValidator [x] nil)
            (getWatches [x] @watches-atom)
            (addWatch [x k callback]
              (swap! watches-atom assoc k callback) 
              x)
            (removeWatch [x k]
              (swap! watches-atom dissoc k) 
              x)

            Reactor
            (deactivate [r] 
              (locking r 
                (when @activate-state 
                  (doseq [x sigs] (remove-watch x r)) 
                  (vreset! activate-state false))))
            (activate [r] 
              (locking r
                (when-not @activate-state
                  (doseq [x sigs] (add-watch x r watch-fn)) 
                  (vreset! activate-state true))))
            (activated? [r] 
              (locking r
                @activate-state))

            Object
            (toString [r] "Lazy Signal Reactor")
            )
        ] 
     (reset! ref-atom r)))

(defn- create-signal-reactor [func args]
  (let [sigs (into #{} (filter #(instance? IRef %) args))
        cur-value (atom (apply!*! func args))
        activate-state (volatile! false)
        watches-atom (atom {})
        ref-atom (atom nil)
        watch-fn (fn [k r o n] 
                    (let  [old-val @cur-value
                           new-val (apply!*! func args)] 
                      (reset! cur-value new-val)
                    (notify-watches @watches-atom @ref-atom old-val new-val)))
        r (reify 
            IDeref 
            (deref [x] @cur-value)

            IRef
            (setValidator [x validator])
            (getValidator [x] nil)
            (getWatches [x] @watches-atom)
            (addWatch [x k callback]
              (swap! watches-atom assoc k callback) 
              x)
            (removeWatch [x k]
              (swap! watches-atom dissoc k) 
              x)

            Reactor
            (deactivate [r] 
              (locking r 
                (when @activate-state 
                  (doseq [x sigs] (remove-watch x r)) 
                  (vreset! activate-state false))))
            (activate [r] 
              (locking r
                (when-not @activate-state
                  (reset! cur-value (apply!*! func args))
                  (doseq [x sigs] (add-watch x r watch-fn)) 
                  (vreset! activate-state true))))
            (activated? [r] 
              (locking r
                @activate-state))

            Object
            (toString [r] "Signal Reactor")
            ) 
        ] 
    (reset! ref-atom r)))


(defn- create-reactor [func args]
  (let [sigs (into #{} (filter #(instance? IRef %) args))
        activate-state (volatile! false)
        watch-fn (fn [k r o n] (apply!*! func args))] 
    (reify 
      Reactor
      (deactivate [r] 
        (locking r 
          (when @activate-state 
            (doseq [x sigs] (remove-watch x r)) 
            (vreset! activate-state false))))
      (activate [r] 
        (locking r
          (when-not @activate-state
            (doseq [x sigs] (add-watch x r watch-fn)) 
            (vreset! activate-state true))))
      (activated? [r] 
        (locking r
          @activate-state))

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

