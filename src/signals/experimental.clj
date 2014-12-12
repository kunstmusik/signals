(ns signals.experimental
  (:require [clojure.core.async :as async :refer [go >! <! close!]])
  (:require [signals.core :refer :all])
  (:import [clojure.lang IDeref IRef]))

(defprotocol ChanSignal
  (put!*! [cs v])
  (close!*! [cs]))


(defn- get-and-set!
  [a new-v]
  (loop [v @a]
     (if (compare-and-set! a v new-v)
       v
       (recur @a))))


(defn chan!*!
  "Creates a core.async channel backed signal. Use put!*! to push a value to the
  core.async channel. Should only use one Reactor per chan!*! signal."
  []
  (let [ch (async/chan)
        cur-value (atom nil)
        watches-atom (atom {})
        chan-sig
        (reify
          IDeref 
          (deref [x] (get-and-set! cur-value nil))

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

          ChanSignal
          (put!*! [x v]
            (go (>! ch v)))
          (close!*! [x]
            (close! ch))) ]
    (go 
      (loop []
        (let [v (<! ch)]
          (when v
            (reset! cur-value v)   
            (notify-watches @watches-atom chan-sig nil v)
            (recur)))))
    chan-sig
    ))


