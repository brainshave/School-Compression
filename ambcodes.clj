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
       (for [c code s suffixes]
	 (let [new-suffix (suffix c s)]
	   (if (code new-suffix)
	     [c s new-suffix]
	     new-suffix))))))
  ([code]
     (distinct
      (filter-not-nil
       (for [c1 code c2 code :when (not= c1 c2)]
	 (let [new-suffix (suffix c1 c2)]
	   (if (code new-suffix)
	     [c1 c2 new-suffix]
	     new-suffix)))))))

(defn make-chars-set [code]
  (into #{} (map seq code)))

(defn first-repetition [code]
  (ffirst (filter (fn [[_ v]] (> v 1))
		  (seq/frequencies code))))

(defn ambiguous? [code]
  (if-let [rep (first-repetition code)]
    rep
    (loop [code (make-chars-set code)
	   suffixes #{}]
      (let [new-suffixes (find-suffixes code suffixes)]
	)))) ;; TODO
      
      

(defn unambiguous?-old2
  [code]
  (if-not (not-any? (fn [[_ v]] (> v 1))
		    (seq/frequencies code))
    
    (let [code (make-chars-set code)
	  ]
      (if-not (apply distinct? code)
	false
	(loop [suffixes (find-suffixes code)])))))
  
  

(defn find-suffixes-old
  ([code suffixes]
     (let [words (for [c code s suffixes]
		   (suffix c s))]
       (if (not-any? #(= :same-words %) words)
	 (into #{} (filter-not-nil words))
	 :same-words)))
  ([code] (into #{} (filter-not-nil
		     (for [c1 code c2 code :when (not= c1 c2)]
		       (suffix c1 c2))))))

(defn unambiguous?-old
  "Is given code unambiguous?"
  [code]
  (if-not (apply distinct? code)
    false
    (loop [suffixes (find-suffixes code)]
      (let [new-suffixes (find-suffixes code suffixes)]
	;(println suffixes \tab new-suffixes)
	(cond (= :same-words new-suffixes) false
	      (every? suffixes new-suffixes) true
	      (empty? new-suffixes) true
	      true (recur (into suffixes new-suffixes)))))))

(defn rand-word [max-len]
  (take (inc (rand-int max-len))
	(repeatedly #(rand-int 2))))

(defn next-word [word]
  (if (== (first word) 0)
    (conj (rest word) 1)
    (conj word 0)))

(defn new-alphabet2 [n]
  (loop [code #{} word (next-word ())]
    (if (== (count code) n)
      code
      (let [new-code (conj code word)
	    new-word (next-word word)]
	(if (unambiguous? new-code)
	  (recur new-code new-word)
	  (recur (rest code) new-word))))))

(defn new-alphabet [n max-len]
  (loop [code #{} suffixes #{} word (rand-word max-len) i (int 0)]
    (cond (== i n) code
	  (code word) (recur code suffixes (rand-word max-len) i)
	  (unambiguous? (conj code word)) (recur (conj code word)
						 suffixes
						 (rand-word max-len)
						 (inc i))
	  true (recur code suffixes (rand-word max-len) i))))


(comment
	  true (let [new-code (conj code word)
		     new-suffixes (find-suffixes new-code suffixes)]
		 (if (unambiguous? new-code) ;(= :same-words new-suffixes)
		   (recur new-code (into suffixes new-suffixes) (rand-word max-len) (inc i))
		   (recur code suffixes (rand-word max-len) i) ; take another word
		   )))
