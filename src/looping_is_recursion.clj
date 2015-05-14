(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [x n k]
                 (if (zero? k)
                 x
                 (recur (* n x) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (not (= (count seq1) (count seq2)))
    false
   (and (empty? seq1) (empty? seq2))
     true
   (not (= (first seq1) (first seq2)))
     false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         x 0]
    (cond
     (empty? a-seq)
       nil
     (not (pred (first a-seq)))
      (recur (rest a-seq) (inc x))
     :else x)))

(defn avg [a-seq]
  (loop [a-seq a-seq
         total 0
         size 0]
    (if (empty? a-seq)
      (/ total size)
      (recur
       (rest a-seq)
       (+ total (first a-seq))
       (inc size)))))

(defn parity [a-seq]
  (loop [a-seq a-seq
         new #{}]
    (cond
     (empty? a-seq)
      new
     (contains? new (first a-seq))
      (recur (rest a-seq) (disj new (first a-seq)))
     :else
      (recur (rest a-seq) (conj new (first a-seq))))))

(defn fast-fibo [n]
 (loop [x 0
        n-1 0
        n-2 1]
   (if (< x n)
     (recur (inc x) (+ n-1 n-2) n-1)
     n-1)))

(defn cut-at-repetition [a-seq]
 (loop [set-1 #{}
        help a-seq
        x 0]
   (if (contains? set-1 (first help))
     (take x a-seq)
     (recur (conj set-1 (first help)) (rest help) (inc x)))))
