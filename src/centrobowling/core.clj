(ns centrobowling.core)


;; Given:
;;   a sequences of rolls (integers from 0 to 10) in a, possibly partial, bowling game
;; compute:
;;  the score so far

(defn zeronil+
  [& args]
  (apply + (filter identity args)))

(defn list*-xnil
  "Works like list* but eliminates trailing nils.
  Assumes all nils after first nil"
  [& args]
  (take-while #(not (nil? %)) (apply list* args)))

(defn take-frame
  "from a sequence of rolls,
  return 2 element vector of next frame score and the rest of the rolls"
  [[r r1 r2 & rs] fnum]
  (cond
    (= 10 fnum)
    [(zeronil+ r r1 r2) nil]

    (= r 10) ; strike
    [(zeronil+ 10 r1 r2) (list*-xnil r1 r2 rs)]

    (= (zeronil+ r r1) 10) ; spare
    [(zeronil+ 10 r2) (list*-xnil r2 rs)]

    :else
    [(zeronil+ r r1) (list*-xnil r2 rs)]))

(defn frames
  "sequence of frame scores"
  [rolls]
  (loop [frames [] rs rolls fnum 1]
    (if (empty? rs)
      frames
      (let [[fs rrs] (take-frame rs fnum)]
        (recur (conj frames fs) rrs (inc fnum))))))

(defn score [rolls]
  "add up frames"
  (reduce + 0 (frames rolls)))