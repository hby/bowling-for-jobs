(ns centrobowling.core-test
  (:require [clojure.test :as t]
            [clojure.test.check.clojure-test :as tct]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [centrobowling.core :refer :all]))

(t/deftest one-then-gutter
  (t/testing "one pin per frame"
    (t/is (= (score (take 20 (cycle [1 0])))
           10))))

(t/deftest perfect-game
  (t/testing "all strikes"
    (t/is (= (score (repeat 12 10))
           300))))

(t/deftest five-four
  (t/testing "all nines (5-4)"
    (t/is (= (score (take 20 (cycle [5 4])))
           90))))

(t/deftest four-five
  (t/testing "all nines (4-5"
    (t/is (= (score (take 20 (cycle [5 4])))
           90))))

(t/deftest all-high-spares
  (t/testing "high spares (9-1)"
    (t/is (= (score (take 20 (cycle [9 1])))
           (+ 19 19 19 19 19 19 19 19 19 10)))))

(t/deftest all-low-spares
  (t/testing "low spares (1-9)"
    (t/is (= (score (take 20 (cycle [1 9 ])))
             (+ 11 11 11 11 11 11 11 11 11 10)))))

(t/deftest hanging-strike-no-future-frame
  (t/testing "strike only"
    (t/is (= (score [1 2 10])
             (+ 3 10)))))

(t/deftest hanging-strike-one-future-frame
  (t/testing "strike plus one frame"
    (t/is (= (score [1 2 10 3 4])
             (+ 3 17 7)))))

(t/deftest hanging-strike-spare
  (t/testing "strike plus spare"
    (t/is (= (score [1 2 10 6 4])
             (+ 3 20 10)))))

(t/deftest hanging-strike-spare-one-future-frame
  (t/testing "strike plus spare"
    (t/is (= (score [1 2 10 6 4 5 5])
             (+ 3 20 15 10)))))

(t/deftest hanging-spare-no-future-frame
  (t/testing "spare only"
    (t/is (= (score [1 2 4 6])
             (+ 3 10)))))

(t/deftest hanging-spare-one-future-frame
  (t/testing "spare plus one frame"
    (t/is (= (score [1 2 6 4 3 4])
             (+ 3 13 7)))))

(t/deftest hanging-3-strikes-gutter
  (t/testing "spare plus one frame"
    (t/is (= (score [10 10 10 0])
             (+ 30 20 10)))))

(t/deftest no-rolls
  (t/testing "no rolls"
    (t/is (= (score [])
             0))))

(t/deftest one-gutter
  (t/testing "one gutter"
    (t/is (= (score [0])
             0))))

(t/deftest eleven-gutters
  (t/testing "eleven gutters"
    (t/is (= (score [0 0 0 0 0 0 0 0 0 0 0])
             0))))

(t/deftest incomplete-tenth-frame
  (t/testing "incomplete tenth frame"
    (t/is (= (score [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])
             0))))

(tct/defspec no-strikes-or-spares-is-simple-sum
             100
             (prop/for-all [rt (gen/vector (gen/choose 0 9) 10)]
                           (let [rolls (interleave rt (repeat 0))]
                             (= (score rolls)
                                (apply + rolls)))))

(tct/defspec partial-game-no-strikes-or-spares-is-simple-sum
             100
             (prop/for-all [rolls (gen/vector (gen/choose 0 4) 1 19)]
                           (= (score rolls)
                              (apply + rolls))))

