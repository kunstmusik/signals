(ns signals.demo
  (:require [signals.core :refer :all]
            [signals.protocols :refer :all]
            [clojure.set :refer :all]))

(comment


;; ====================================
;; Example: e!*!, l!*!, and r!*! blocks 
;; ====================================

  (def source (atom []))

  ;; Will be fired each time source changes
  (def value-printer (e!*! println "TEST>>> " source))


  ;; Will be updated each time source changes
  (def reactive-signal 
    (r!*! 
      into [] 
      (comp 
        (map (comp clojure.string/lower-case :first-name))
        (map clojure.string/lower-case)
        (filter #(< (count %) 4))) 
      source))

  ;; Will be notified and fire notifications when updated, but
  ;; updated each time block is dereferenced 
  (def lazy-rs
    (l!*! 
      into [] 
      (comp 
        (map (comp clojure.string/lower-case :first-name))
        (map clojure.string/lower-case)
        (filter #(< (count %) 4))) 
      source))

  (println "First b" @reactive-signal)
  (println "First l" @lazy-rs)

  (reset!
    source 
    [{:first-name "sue"}
     {:first-name "maria"}
     {:first-name "ted"}
     {:first-name "Bob"} 
     {:first-name "PAN"} 
     ])

  (println "second b" @reactive-signal)
  (println "second l" @lazy-rs)

  (reset! source [
             {:first-name "maria"}
             {:first-name "PAN"} 
             ])


  (println "third b" @reactive-signal)
  (println "third l" @lazy-rs)

;; ================================
;; Example: Signals as data sources 
;; ================================

  (def data1 
    (atom #{ {:a 1 :b 2} {:a 2 :b 3} {:a 3 :b 4 }}))
  (def data2 
    (atom #{ {:k :first :b 3} {:k :second :b 4 }}))

  (def selection
    (l!*! select #(even? (:b %)) data1))

  (def projection 
    (l!*! project data1 [:a]))

  (def joint 
    (l!*! join data1 data2 ))

  (def data-proc0 
    (e!*! println "Handling code change on selection: " selection))

  (def data-proc1 
    (e!*! println "Handling code change on project: " projection))

  (def data-proc2 
    (e!*! println "Handling code change on join: " joint))

  @selection ;; returns #{{:b 4, :a 3} {:b 2, :a 1}}
  @projection ;; returns #{{:a 1} {:a 3} {:a 2}}
  @joint ;; returns #{{:k :second, :b 4, :a 3} {:k :first, :b 3, :a 2}}

  ;; e!*! blocks will fire when the following changes the sources
  (swap! data1 conj {:a 4 :b 5})
  (swap! data2 conj {:k :third :b 5})

  @selection ;; returns #{{:b 4, :a 3} {:b 2, :a 1}}
  @projection ;; returns #{{:a 1} {:a 3} {:a 4} {:a 2}}
  @joint ;; returns #{{:k :second, :b 4, :a 3} {:k :first, :b 3, :a 2} 
         ;; {:a 4, :k :third, :b 5}}


;; ===========================================
;; Example: Single App State and CursorSignals
;; ===========================================

(def app-state
  (atom {:high-scores []
         :running-status false
         :user-location [0 0]
         }))

;; setup signals that are sub-sets of the global app-state
(def high-scores (c!*! app-state [:high-scores]))
(def running-status (c!*! app-state [:running-status]))
(def user-location (c!*! app-state [:user-location]))

; prints "[]"
(println @high-scores)

;; update the high-scores
(swap!*! high-scores conj [:user :me :score 1000])

; prints "[[:user :me :score 1000]]"
(println @high-scores)


;; prints [0 0]
(println @user-location)

;; updated the 2nd part of location to 10
(swap!*! user-location assoc 1 10)

;; prints [0 10]
(println @user-location)

;; prints false
(println @running-status)

;; resets value to true
(reset!*! running-status true)

;; prints true 
(println @running-status)

;; All changes from sub-keypath-signals are reflected in the global app-state
;; {:high-scores [[:user :me :score 1000]], :running-status true, :user-location [0 10]}
(println @app-state)


;; ========================================================
;; Example: Writing functions that use time-varying signals 
;; ========================================================

  ;; Note with partial!*!, pass in the IDeref itself
  (def print-status 
    (partial!*! println "Latest Users: " reactive-signal))

  (defn print-status2
    []
    (println "Latest Users: " @reactive-signal))

  (def print-status3
    #(println "Latest Users: " @reactive-signal))

  (println "X: " @reactive-signal @lazy-rs)
  (print-status)
  (print-status2)
  (print-status3)



;; ==========================
;; Example: Actions in a List 
;; ==========================

  (def actions
    [[println "Latest Users: " reactive-signal]
     [println "Original Value: " source]
     ])

  (defn process-actions 
    [actions]
    (doseq [[func & args] actions]
      (apply!*! func args)))

  (process-actions actions)


;; =========================================
;; Example: Same function, different sources 
;; =========================================

  (def input (d!*! read-line))
  (def t (seq!*! (range 5)))

  (defn process [src]
    (loop [line 0 v @src]
      (when v 
        (println line ") " v) 
        (recur (inc line) @src))))

  (process t)
  ;; evaluate in REPL to grab input from stdin
  (process input)

;; ==================
;; Example: Blog Post
;; ==================

(def author (atom ""))
(def title (atom ""))
(def body (atom ""))


;; Pure Function...
(defn create-blog-post [author title body]
  (format "Author: %s\nTitle: %s\nBody:\n%s" author title body))

;; ... lifted into a reactive Signal Reactor 
(def blog-post (l!*! create-blog-post author title body))

;; @blog-post is always in sync with author, title, body
(print @blog-post)

(reset! author "Steven")
(reset! title "My Article")
(reset! body "Body of text.\n Line 2.")

(print @blog-post)


;; ================
;; Speculative Work 
;; ================

;; experiments with reductions, iteratees: so far, doesn't handle EOF, or
;; if func produces multiple outs for single in. Not sure if any of this 
;; is necessary, but leaving here for now until a decision is made.


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


)

;)
