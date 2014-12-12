(ns signals.core-test
  (:require [clojure.test :refer :all]
            [signals.core :refer :all]
            [signals.protocols :refer :all]
            ))

(deftest apply!*!-test 
  (testing "apply!*! processes IRef args before applying function"
    (is (= 4 (apply!*! + (atom 1) (atom 3))))
    ))

(deftest partial!*!-test 
  (testing "partial!*! processes IRef args before applying function"
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
