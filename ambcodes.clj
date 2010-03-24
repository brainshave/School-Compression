(ns #^{:author "Szymon Witamborski"
       :doc "Coding Ambiguity"}
  ambcodes)

(defn starts-with
  "Returns true if sequence a starts with b"
  [a b]
  (= (take (count b) a) (seq b)))

(defn suffix
  [a b]
  (let [a (seq a) b (seq b)]
    (if (= a b)
      :same-words
      (let [f #(if (starts-with %1 %2) (drop (count %2) %1))]
	(or (f a b) (f b a))))))

(defn find-suffixes
  ([code suffixes]
     (let [words (for [c code s suffixes]
		   (suffix c s))]
       (if (not-any? #(= :same-words %) words)
	 (into #{} words)
	 :same-words)))
  ([code] (into #{} (filter #(not (nil? %))
			    (for [c1 code c2 code :when (not= c1 c2)]
			      (suffix c1 c2))))))

(defn unambiguous?
  "Is given code unambiguous?"
  [code]
;  {:pre [(not-any? true? (for [c1 code c2 code]
;			   (= c1 c2)))]}
  (loop [suffixes (find-suffixes code)]
    (let [new-suffixes (find-suffixes code suffixes)]
      (cond (= :same-words new-suffixes) false
	    (every? new-suffixes suffixes) true
	    (empty? new-suffixes) true
	    true (recur (into suffixes new-suffixes))))))
	    
    
    
