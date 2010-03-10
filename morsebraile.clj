(ns #^{:author "Szymon Witamborski (santamon)"
       :doc "Converts text to Braile and Morse codes. Performs some statistics as well."}
  morsebraile)

(def #^{:doc "Sorted map with Morse codes"
	:test (fn [] (map #(when (not (= (last %) \space)) %) (vals morse))) }
     morse (sorted-map-by #(let [c1 (count %1) c2 (count %2)]
				 (cond (== c1 c2) (.compareTo %1 %2)
				       (> c1 c2) -1
				       (< c1 c2) 1))
			  "a" ".- "
			  "b" "-... "
			  "c" "-.-. "
			  "d" "-.. "
			  "e" ". "
			  "f" "..-. "
			  "g" "--. "
			  "h" ".... "
			  "i" ".. "
			  "j" ".--- "
			  "k" "-.- "
			  "l" ".-.. "
			  "m" "-- "
			  "n" "-. "
			  "o" "--- "
			  "p" ".--. "
			  "q" "--.- "
			  "r" ".-. "
			  "s" "... "
			  "t" "- "
			  "u" "..- "
			  "v" "...- "
			  "w" ".-- "
			  "x" "-..- "
			  "y" "-.-- "
			  "z" "--.. "
			  "ą" ".-.- "
			  "ć" "-.-.. "
			  "ę" "..-.. "
			  "ch" "---- "
			  "ł" ".-..- "
			  "ń" "--.-- "
			  "ó" "---. "
			  "ś" "...-... "
			  "ż" "--..-. "
			  "ź" "--..- "))
