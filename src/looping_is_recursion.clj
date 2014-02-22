(ns looping-is-recursion)

(defn power [base exp]
  (loop [total base
         exp exp]
    (cond
     (< exp 1) 1
     (= exp 1) total
     :else (recur (* base total) (dec exp)))))

(defn last-element [a-seq]
  (loop [coll a-seq]
    (if (< (count coll) 2)
      (first coll)
      (recur (rest coll)))))

(defn seq= [seq1 seq2]
  (loop [a seq1
         b seq2]
    (cond
     (and (empty? a) (empty? b)) true
     (or (empty? a) (empty? b)) false
     (== (first a) (first b)) (recur (rest a) (rest b))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [coll a-seq
         pos 0]
    (cond
     (empty? coll) nil
     (pred (first coll)) pos
     :else (recur (rest coll) (inc pos)))))

(defn avg [a-seq]
  (loop [coll a-seq
         acc 0]
    (if (empty? coll)
      (/ acc (count a-seq))
      (recur (rest coll) (+ acc (first coll))))))

(defn parity [a-seq]
  (loop [coll a-seq
         acc #{}]
    (if (empty? coll) acc
        (recur (rest coll)
               (if (contains? acc (first coll))
                 (disj acc (first coll))
                 (conj acc (first coll)))))))

(defn fast-fibo [n]
  (loop [i 0
         a 0
         b 1]
    (if (= i n) a
        (recur (inc i) b (+ a b)))))

(defn cut-at-repetition [a-seq]
  (loop [coll a-seq
         seen []]
    (if (or (empty? coll) (some #{(first coll)} seen))
      seen
      (recur (rest coll) (conj seen (first coll))))))
