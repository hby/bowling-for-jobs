(ns centrobowling.core)


;; Given a sequences of rolls (integers from 0 to 10) in full bowling game
;; compute the score

(defn zero-for-nil
  [n]
  (if (nil? n) 0 n))

(defn build-list
  "Builds (concat [first second] rest) taking
  into account first and second could be nil so make sure
  there are no nils at the beginning of the returned list.

  Note: If (nil? first) then (nil? second)
        If (nil? second) then (nil? rest)"
  [first second rest]
  (cond
    (nil? first)
    nil

    (nil? second)
    (list first)

    :else
    (concat [first second] rest)))

(defn take-frame
  "return 2 element vector of next frame score and the
  rest of the rolls"
  [[r r1 r2 & rs] fnum]
  (cond
    (= 10 fnum)
    (cond
      (nil? r1)
      [r nil]

      (nil? r2)
      [(+ r r1) nil]

      (nil? rs)
      [(+ r r1 r2) nil])

    (= r 10) ; strike
    [(+ 10 (zero-for-nil r1) (zero-for-nil r2)) (build-list r1 r2 rs)]

    (and (not (nil? r1)) (= (+ r r1) 10)) ; spare
    [(+ 10 (zero-for-nil r2)) (build-list r2 (first rs) (rest rs))]

    :else
    [(+ r (zero-for-nil r1)) (build-list r2 (first rs) (rest rs))]))

(defn frames
  "sequence of frame scores"
  [rolls]
  (loop [frames [] rs rolls fnum 1]
    (if (nil? rs)
      frames
      (let [[fs rrs] (take-frame rs fnum)]
        (recur (conj frames fs) rrs (inc fnum))))))

(defn score [rolls]
  "add up frames"
  (reduce + 0 (frames rolls)))