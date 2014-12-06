(ns signals.core
  (:import [clojure.lang IDeref]))

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
        (map #(if (instance? DynamicArg %)
                (let [f (.func ^DynamicArg %)
                      a (.args ^DynamicArg %)] 
                  (if a
                    (apply!*! f a)
                    (f)))
                %))
        (apply func))
     (func)))
  ([func arg & args]
   (apply!*! func  (list* arg args))))

(defn partial!*!
  [& args]
  (apply partial apply!*! args))

(defn r!*! 
  [func & args] 
  (reify IDeref 
    (deref  [_]  (apply!*! func args)))) 

(defn sig
 [s] 
 (!*! deref s)) 

;; testing code

(def a (atom []))

(def b 
  (r!*! 
    into [] 
    (comp 
      (map (comp clojure.string/lower-case :first-name))
      (map clojure.string/lower-case)
      (filter #(< (count %) 4))) 
    (sig a)))

(reset! 
  a 
  [{:first-name "sue"}
   {:first-name "maria"}
   {:first-name "ted"}
   {:first-name "Bob"} 
   {:first-name "PAN"} 
   ])

(def print-status 
  (partial!*! println "Latest Users: " (sig b)))

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
  [[println "Latest Users: " (sig b)]
   [println "Original Value: " (sig a)]
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

(def source (atom 0))

(defn source!*! [sig]
  (fn [] @sig))

(defn pushify
  [src dest]
  (fn [a]
    (let [v (src a)]
      (if (or (= :CONTINUE v) (= :DONE v))
        v 
        (dest v))
      )))

((pushify #(- % 5) #(+ 45 %)) 10)

;; this could be rewritten using loop and make it non stack consuming...
;; This could also be rewritten to remove the source function and move it to the caller 
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
  [pform red-fn initial source-sig]
  (loop [accum initial v @source-sig]
    (if v
      (let [pform-v (pform v)] 
        (case pform-v
          :DONE accum
          :CONTINUE (recur accum @source-sig)
          (recur (red-fn accum pform-v) @source-sig)))
      accum)))

(reduce!*! comp-chain conj [] (range!*! 50))

(defn take!*!
  [num-to-take]
  (let [v (volatile! num-to-take)] 
    (fn [a]
      (let [n (vswap! v dec)]
        (if (>= n 0)
          a
          :DONE)))))

(defn partition!*! 
  [n]
  (let [counter (volatile! n)
        v (volatile! [])]
    ;(fn [a]
    ;  (let [indx (vswap! counter dec) 
            
    ;        ]) 
    ;  )
    )  
  )

(def maybe-chain
  (||> 
    #(do (println ">>> " %) %)
    #(* % 10) 
    (take!*! 5)
    #(do (println "~~~ " %) %)
    ))


(reduce!*! maybe-chain conj [] (range!*! 50))


