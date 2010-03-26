(ns #^{:author "Szymon Witamborski"
       :doc "Coding Ambiguity"}
  ambcodes
  (:require (clojure.contrib [seq :as seq])))

(defn starts-with
  "Returns true if sequence a starts with b"
  [a b]
  (= (take (count b) a)
     b))

(defn suffix
  [a b]
  (let [a (seq a) b (seq b)]
    (let [f #(if (starts-with %1 %2)
	       (drop (count %2) %1))]
      (or (f a b) (f b a)))))

(defn filter-not-nil [coll]
  (filter #(not= nil %) coll))

(defn find-suffixes
  ([code suffixes]
     (distinct
      (filter-not-nil
       (for [c code s suffixes :when (not= c s)]
	 (let [new-suffix (suffix c s)]
	   (if (code new-suffix)
	     [c s new-suffix]
	     new-suffix))))))
  ([code] (find-suffixes code code)))

(defn make-chars-set [code]
  (into #{} (map seq code)))

(defn first-repetition [code]
  (ffirst (filter (fn [[_ v]] (> v 1))
		  (seq/frequencies code))))

(defn ambiguous?
  "Return first ambiguous word (if doubled) or [word1 word2 suffix]
when two words have suffix that is a word in code"
  [code]
  (if-let [repetition (first-repetition code)]
    repetition
    (let [code (make-chars-set code)] ; converting to chars sets just in case
      (loop [suffixes #{}
	     candidates (find-suffixes code)]
	(let [amb (first (filter vector? candidates))]
	  ;(println suffixes \tab candidates \tab amb)
	  (cond amb amb ;; return first ambiguity
		(every? suffixes candidates) false
		(empty? candidates) false
		:default (let [new-suffixes (into suffixes candidates)]
			   (recur new-suffixes
				  (find-suffixes code new-suffixes)))))))))

(defn rand-word [max-len]
  (take (inc (rand-int max-len))
	(repeatedly #(rand-int 2))))

(defn rand-word-fixed-len [len]
  (repeatedly len #(rand-int 2)))

(defn next-word [word]
  (if (== (first word) 0)
    (conj (rest word) 1)
    (conj word 0)))

(def cardinal-words (iterate next-word '(0)))

(defn rand-alphabet [n max-len]
  (repeatedly n #(rand-word max-len)))

(defn stupid-generator
  "Stupid generator, just generating random alphabets of n length with
words of max-len length.
Stops after max-tries"
  ([n max-len max-tries]
     (loop [alphabet (rand-alphabet n max-len)
	    tries (int 0)]
       (cond (== (inc tries) max-tries) nil
	     (ambiguous? alphabet) (recur (rand-alphabet n max-len) (inc tries))
	     :default (with-meta alphabet {:tries tries}))))
  ([n] (stupid-generator n (int (* n 3/4)) (* n n n))))

(defn smart-generator
  ([n words strategy]
     (loop [code #{}
	    word (first words)
	    words (rest words)]
       (cond (== n (count code)) code
	     (code word) (recur code (first words) (rest words))
	     :default
	     (let [new-code (conj code word)
		   amb (ambiguous? new-code)]
	       (if (vector? amb)
		 (recur (disj new-code (strategy amb))
			(first words) (rest words))
		 (recur new-code
			(first words) (rest words)))))))
  ([n strategy] (smart-generator
		 n (repeatedly #(rand-word (int (* n 3/4))))
		 strategy)))

(defmacro time-only [exp]
  `(do (time (dorun ~exp)) nil))

(defmacro with-time
  ""
  [exp]
  `(let [time# (System/nanoTime)
	 val# ~exp
	 meta# (meta val#)]
     (with-meta val#
       (assoc meta# :nano-time (- (System/nanoTime) time#)))))
       
(defn average-len [code]
  (let [sum (reduce + (map count code))]
    (/ sum (count code))))

(defn test-strategy [t n strategy]
  (let [codes (repeatedly t #(with-time (smart-generator n strategy)))
	times (map #(:nano-time (meta %)) codes)
	avgtime (/ (reduce + times) t)
	lengths (map average-len codes)
	avglen (/ (reduce + lengths) t)]
     {:avgtime (double avgtime)
      :avglen (double avglen)}))

(defn test-cardinal [n strategy]
  (let [code (with-time (smart-generator n cardinal-words strategy))
	time (:nano-time ^code)
	avglen (average-len code)]
    {:avgtime (double time) ; not really average...
     :avglen (double avglen)}))