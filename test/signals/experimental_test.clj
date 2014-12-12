(ns signals.experimental-test
  (:require [clojure.test :refer :all]
            [signals.core :refer :all]
            [signals.experimental :refer :all]
            [signals.protocols :refer :all]
            ))

(deftest chan!*!-test
  (testing "chan!*! single signal"
    (let [a (chan!*!)
          f (foldp!*! + 10 a)]
      (is (= 10 @f)) 
      (put!*! a 2)
      (Thread/sleep 20)
      (is (= 12 @f)) 
      (put!*! a 2)
      (Thread/sleep 20)
      (is (= 14 @f)) 
      (put!*! a 3)
      (Thread/sleep 20)
      (is (= 17 @f)) 
      (close!*! a)
      )))
