(ns signals.core-test
  (:require [clojure.test :refer :all]
            [signals.core :refer :all]
            [signals.protocols :refer :all]
            ))

(deftest apply!*!-test 
  (testing "apply!*! processes IDeref args before applying function"
    (is (= 4 (apply!*! + (atom 1) (atom 3))))
    ))

(deftest partial!*!-test 
  (testing "partial!*! processes IDeref args before applying function"
    (let [a (atom 1)
          b (atom 3)
          temp (partial!*! + a b)] 
      (is (= 4 (temp) )))
    ))

(deftest r!*!-test
  (testing "Strict reactor block calculates on depency changes"
    (let [a (atom 2)
          b (ref 3)
          out (atom 0)
          write-fn (fn [a b] (reset! out (+ a b)))
          r-block (r!*! write-fn a b)
          ]
      (is (= 5 @r-block)) 
      (swap! a inc)    
      (is (= 6 @out))
      (is (= 6 @r-block)) 

      )))

(deftest l!*!-test
  (testing "Lazy reactor block calculates on read"
    (let [a (atom 2)
          b (ref 3)
          out (atom 0)
          write-fn (fn [a b] (reset! out (+ a b)))
          l-block (l!*! write-fn a b)
          ]
      (is (= 0 @out))
      (is (= 5 @l-block)) 
      (swap! a inc)    
      (is (= 5 @out))
      (is (= 6 @l-block)) 
      (is (= 6 @out))
      )))

(deftest chain-test
  (testing "Reactor chain calculates down pipeline"
    (let [a (atom 2)
          b (ref 3)
          out (atom 0)
          write-fn (fn [a b] (reset! out (+ a b)))
          r-block (r!*! write-fn a b)
          out2 (atom 0)
          write-fn2 (fn [a b] (reset! out2 (* a b)))
          l-block (l!*! write-fn2 r-block 4) ]
      (is (= 5 @out))
      (is (= 5 @r-block)) 
      (is (= 0 @out2))
      (is (= 20 @l-block)) 
      (is (= 20 @out2)) 
      (swap! a inc)    
      (is (= 6 @out))
      (is (= 20 @out2))
      (is (= 6 @r-block)) 
      (is (= 6 @out))
      (is (= 24 @l-block)) 
      (is (= 24 @out2))
      )))

(deftest activation-test
  (testing "Reactors activate/deactivate"
    (let [a (atom 2)
          b (ref 3)
          out (atom 0)
          write-fn (fn [a b] (reset! out (+ a b)))
          r-block (r!*! write-fn a b)]
      (is (= 5 @out))
      (is (= 5 @r-block)) 
      (swap! a inc)    
      (is (= 6 @out))
      (is (= 6 @r-block)) 
      (is (= true (activated? r-block)))
      (deactivate r-block)
      (swap! a inc)    
      (is (= 6 @out))
      (is (= 6 @r-block)) 

      (is (= false (activated? r-block)))
      (activate r-block)
      (is (= 7 @out))
      (is (= 7 @r-block)) 
      (swap! a inc)    
      (is (= 8 @out))
      (is (= 8 @r-block)) 
      )))


(deftest foldp!*!-test
  (testing "foldp!*! single signal"
    (let [a (atom 0)
          f (foldp!*! + 10 a)]
     
      (is (= 10 @f)) 
      (reset! a 2)
      (is (= 12 @f)) 
      (reset! a 2)
      (is (= 14 @f)) 
      (swap! a inc)
      (is (= 17 @f)) 
      ))
  (testing "foldp!*! two signals"
    (let [a (atom 0)
          b (atom 0)
          f (foldp!*! + 10 a b)]
      (is (= 10 @f)) 
      (reset! a 2)
      (is (= 12 @f)) 
      (reset! a 2)
      (is (= 14 @f)) 
      (swap! a inc)
      (is (= 17 @f)) 
      (reset! b 2)
      (is (= 22 @f)) 
      (reset! b 0)
      (is (= 25 @f)) 
      )))

(deftest seq!*!-test
  (testing "seq!*! test"
    (let [sig (seq!*! (range 5))]
      (is (= 0 @sig)) 
      (is (= 1 @sig)) 
      (is (= 2 @sig)) 
      (is (= 3 @sig)) 
      (is (= 4 @sig)) 
      (is (= nil @sig)) 
      )))


(deftest c!*!-test
  (testing "c!*! CursorSignal test"
    (let [app-state (atom {:test {:a 1 :b {:c 30}}})
          cursor0 (c!*! app-state [:test])
          cursor1 (c!*! cursor0 [:b])
          cursor2 (c!*! cursor1 [:c])
          ]
      (is (= {:a 1 :b {:c 30}} @cursor0)) 
      (is (= {:c 30} @cursor1)) 
      (is (= 30 @cursor2)) 
      (reset!*! cursor2 40)
      (is (= {:a 1 :b {:c 40}} @cursor0)) 
      (is (= {:c 40} @cursor1)) 
      (is (= 40 @cursor2)) 
      )))
