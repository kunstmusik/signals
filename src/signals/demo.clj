(ns signals.demo
  (:require [signals.core :refer :all]))

(comment


;; ====================================
;; Example: e!*!, l!*!, and r!*! blocks 
;; ====================================

  (def a (atom []))
  (def e (e!*! println "TEST>>>" a))

  (def reactive-signal 
    (r!*! 
      into [] 
      (comp 
        (map (comp clojure.string/lower-case :first-name))
        (map clojure.string/lower-case)
        (filter #(< (count %) 4))) 
      a))

  (def lazy-rs
    (l!*! 
      into [] 
      (comp 
        (map (comp clojure.string/lower-case :first-name))
        (map clojure.string/lower-case)
        (filter #(< (count %) 4))) 
      a))

  (println "First b" @reactive-signal)
  (println "First l" @lazy-rs)

  (reset!
    a 
    [{:first-name "sue"}
     {:first-name "maria"}
     {:first-name "ted"}
     {:first-name "Bob"} 
     {:first-name "PAN"} 
     ])

  (println "second b" @reactive-signal)
  (println "second l" @lazy-rs)

  (reset! a [
             {:first-name "maria"}
             {:first-name "PAN"} 
             ])

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
        (deref [sig] 
          (locking sig 
            (let [[x & xs] @head] 
                     (reset! head xs)
                     x))))))

  (defn range!*! [& args] 
    (seq->signal (apply range args)))

  (def t (range!*! 5))

  (defn process [src]
    (loop [line 0 v @src]
      (when v 
        (println line ") " v) 
        (recur (inc line) @src))))

;; ==================
;; Example: Blog Post
;; ==================

(def author (atom ""))
(def title (atom ""))
(def body (atom ""))
(defn create-blog-post [author title body]
  (format "Author: %s\nTitle: %s\nBody:\n%s" author title body))
(def blog-post (l!*! create-blog-post author title body))

(print @blog-post)

(reset! author "Steven")
(reset! title "My Article")
(reset! body "Body of text.\n Line 2.")

(print @blog-post)


;; experiments with reductions, iteratees: so far, doesn't handle EOF, or
;; if func produces multiple outs for single in probably better at this
;; point to just focus on the pushy chain and context, most likely don't
;; need this


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
