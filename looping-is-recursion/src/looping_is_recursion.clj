(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [n k]
  	(cond 
  	  (zero? k) 1
  	  (== 1 k) n 
  	  :else (recur (* n base) (dec k))))]
  (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [frst b-seq]
  	(if (empty? b-seq) frst (recur (first b-seq) (rest b-seq))))]
  (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [x y]
  	(cond
  		(empty? x) (if (empty? y) true false)
  		(empty? y) false
  		(== (first x) (first y)) (recur (rest x) (rest y))
  		:else false))]
  (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [b-seq a-seq
  	     pred pred
  	     ind 0]
  	(cond
  		(empty? b-seq) nil
  		(pred (first b-seq)) ind 
  		:else (recur (rest b-seq) pred (inc ind)))))

(defn avg [a-seq]
  (loop [b-seq a-seq
  	     sum 0
  	     ind 0]
  	(cond
  		(empty? b-seq) (/ sum ind)
  		:else (recur (rest b-seq) (+ sum (first b-seq)) (inc ind)))))

(defn parity [a-seq]
  (loop [b-seq a-seq
  	     c-seq '()]
  	(cond
  		(empty? b-seq) c-seq
  		(contains? (set c-seq) (first b-seq)) 
  		  (recur (rest b-seq) (disj c-seq (first b-seq)))
  		:else (recur (rest b-seq) (conj (set c-seq) (first b-seq))))))

(defn fast-fibo [n]
  (loop [n n
  	     f1 0
  	     f2 1]
  	(cond
  		(zero? n) 0
  		(== n 1) 1
  		(== n 2) 1
  		(== n 3) (+ f1 f2 f2)
  		:else (recur (dec n) f2 (+ f1 f2)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
  	     result '()]
  	(cond
  		(empty? a-seq) (sort result)
  		(contains? (set result) (first a-seq)) 
  		  (sort-by number? (sort-by str (set result)))
  		:else (recur (rest a-seq) (conj result (first a-seq))))))

