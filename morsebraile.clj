(ns #^{:author "Szymon Witamborski (santamon)"
       :doc "Converts text to Braile and Morse codes. Performs some statistics as well."}
  morsebraile
  (:require (clojure.contrib [string :as ccs])))

(def #^{:doc "Sorted map with Morse codes"
	:test (fn [] (map #(when (not (= (last %) \space)) %) (vals morse)))}
     morse-data {\a ".- "	    \b "-... "	    \c "-.-. "
		 \d "-.. "	    \e ". "	    \f "..-. "	    \g "--. "
		 \h ".... "	    \i ".. "	    \j ".--- "	    \k "-.- "
		 \l ".-.. "	    \m "-- "	    \n "-. "	    \o "--- "
		 \p ".--. "	    \q "--.- "	    \r ".-. "	    \s "... "
		 \t "- "	    \u "..- "	    \v "...- "	    \w ".-- "
		 \x "-..- "	    \y "-.-- "	    \z "--.. "	    \ą ".-.- "
		 \ć "-.-.. "	    \ę "..-.. "	    \ł ".-..- "	    \ń "--.-- "
		 \ó "---. "	    \ś "...-... "   \ż "--..-. "    \ź "--..- "
		 \1 ".---- "	    \2 "..--- "	    \3 "...-- "	    \4 "....- "
		 \5 "..... "	    \6 "-.... "	    \7 "--... "	    \8 "---.. "
		 \9 "----. "	    \0 "----- "	    \. ".-.-.- "    \, "--..-- "
		 \' ".----. "       \_ "..--.- "    \: "---... "    \? "..--.. "
		 \- "-....- "       \/ "-..-. "	    \( "-.--. "	    \) "-.--.- "
		 \= "-...- "	    \@ ".--.-. " \space \space \newline \newline})
(def morse-data-inv
     (apply hash-map (interleave (map str (vals morse-data)) (keys morse-data))))

(defn morse
  "Converts to morse code using morse-data as a refernce and checking for 'ch'"
  [s]
  (let [ss (conj (apply vector \0 (.toLowerCase s)) \0)] ; add \0 to the end and begining
    (apply str (map #(cond (and (= %2 \c) (= %3 \h)) "---- " ;; ch
			   (and (not= %1 \c) (not= %2 \h)) (morse-data %2))
		   ss (next ss) (nnext ss)))))
(defn unmorse
  ""
  [s]
  (apply str (map morse-data-inv (ccs/partition #"[.-]+\s" s))))
  