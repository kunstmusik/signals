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

(defn process [src]
  (loop [line 0 v @src]
    (when v 
      (println line ") " v) 
      (recur (inc line) @src))))


