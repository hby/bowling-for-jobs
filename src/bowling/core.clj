(ns bowling.core)


;; Given:
;;   a sequences of rolls (integers from 0 to 10) in a, possibly partial, bowling game
;; compute:
;;  the score so far

(defn fnil*
  "like fnil but applies to all args"
  [f x]
  (fn [& args]
    (apply f (map (fnil identity x) args))))

(defn +nil
  [& args]
  (apply (fnil* + 0) args))

(defn list*-xnil
  "Works like list* but eliminates trailing nils.
  Assumes all nils after first nil"
  [& args]
  (take-while #(not (nil? %)) (apply list* args)))

(defn take-frame
  "from a sequence of rolls,
  take one frame (the sum of the rolls that add up for the frame score)"
  [[r r1 r2 & _]]
  (cond
    (= r 10) ; strike
    (+nil 10 r1 r2)

    (= (+nil r r1) 10) ; spare
    (+nil 10 r2)

    :else
    (+nil r r1)))

(defn drop-frame
  "from a sequence of rolls,
  drop the number of rolls that make up one frame"
  [[r r1 r2 & rs]]
  (cond
    (= r 10) ; strike
    (list*-xnil r1 r2 rs)

    (nil? r1)
    nil

    :else
    (list*-xnil r2 rs)))

(defn frames
  "sequence of frame scores"
  [rolls]
  (if (empty? rolls)
    nil
    (lazy-seq (cons (take-frame rolls)
                    (frames (drop-frame rolls))))))

(defn score [rolls]
  "add up frames"
  (reduce + (take 10 (frames rolls))))
