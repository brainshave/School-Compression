(ns #^{:author "Szymon Witamborski (santamon)"
       :doc "Converts text to Braile and Morse codes. Performs some statistics as well."}
  morsebraille
  (:require (clojure [set :as set]
		     [stacktrace :as stacktrace])
	    (clojure.contrib [string :as string]
			     [io :as io]))
  (:import (java.awt BorderLayout
		     Dimension
		     Font)
	   (javax.swing JFrame
			JToolBar
			JLabel
			JPanel
			BoxLayout
			JButton
			JTextArea
			JScrollPane
			JSplitPane
			SwingConstants)
	   (java.awt.event ActionListener
			   ActionEvent)))

(def #^{:doc "Map with Morse codes"
	:test (fn [] (map #(when (not (= (last %) \space)) %) (vals morse-data)))}
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
		 \= "-...- "	    \@ ".--.-. "
		 \space \space      \newline \newline})

(def morse-data-inv (set/map-invert morse-data))

(defn morse
  "Converts to morse code using morse-data as a reference and checking for 'ch'"
  [s]
  (let [ss (conj (apply vector \0 (.toLowerCase s)) \0)] ; add \0 to the end and begining
    (apply str (map #(cond (and (= %2 \c) (= %3 \h)) "---- " ;; ch
			   (or (not= %1 \c) (not= %2 \h)) (morse-data %2))
		    ss (next ss) (nnext ss)))))

(defn decode
  "Decodes s partitionning s with re using m[ap] as a reference for each item."
  [m re s]
  (apply str (map #(let [thing (m %)]
		     (if thing thing %))
		  (string/partition re s))))

(defn unmorse
  "Decodes Morse code."
  [s]
  (decode morse-data-inv #"[.-]+\s" s))

(def capital-mark "000001")
(def digit-mark   "001111")

(def #^{:doc "Map with Braile codes"}
     braille-data
     (let [original-data {\1 "100000" \2 "110000" \3 "100100" \4 "100110" \5 "100010"
			  \6 "110100" \7 "110110" \8 "110010" \9 "010100" \0 "010110"
			  \a "100000" \b "110000" \c "100100" \d "100110" \e "100010"
			  \f "110100" \g "110110" \h "110010" \i "010100" \j "010110"
			  \k "101000" \l "111000" \m "101100" \n "101110" \o "101010"
			  \p "111100" \q "111110" \r "111010" \s "011100" \t "011110"
			  \u "101001" \v "111001" \x "101101" \y "101111" \z "101011"
			  \ż "111101" \ź "011101" \ś "010101" \w "010111" \ó "001101"
			  \ą "100001" \ł "110001" \ć "100101" \ń "100111" \ę "100011"
			  \, "010000" \; "011000" \: "010010" \? "010001" \! "011010"
			  \( "011011" \) "011011" \„ "011001" \" "001011" \” "001011"
			  \. "010011" \- "001001" \newline "001000" \space "000000"}
	   offspring-data ;; add chars with capital marks and number marks
	   (filter vector? (map (fn [[k v]]
				  (cond (Character/isLetter k) [(Character/toUpperCase k)
								(str capital-mark v)]
					(Character/isDigit k) [k (str digit-mark v)]))
				original-data))]
       (into original-data offspring-data)))

(def braille-data-inv (set/map-invert braille-data))

(defn braille
  "Converts to Braille"
  [s]
  (apply str (map braille-data s)))

(defn unbraille
  "Decodes from Braille"
  [s]
  (apply str (let [ss (map #(apply str %) (partition 6 (str "______" s)))]
	       (map #(some braille-data-inv (list (str %1 %2) %2))
		    ss (rest ss)))))

;; GUI

(defn make-button
  "Create button with label name and function f called upon click"
  [name f]
  (doto (JButton. name)
    (.addActionListener
     (proxy [ActionListener] []
       (actionPerformed [#^ActionEvent e]
			(f))))))
(defn make-tarea
  "Create JTextArea with monospaced font"
  ([editable?]
     (doto (JTextArea.)
       (.setFont (Font. Font/MONOSPACED Font/PLAIN 14))
       (.setLineWrap true)
       (.setEditable editable?)))
  ([] (make-tarea true)))

(defn with-scrollbars
  "Wraps panel with scrollbars, sets minimum size to 200x200"
  [p]
  (let [d (Dimension. 200 200)]
    (doto (JScrollPane. p)
      (.setMinimumSize d)
      (.setPreferredSize d))))

(defn make-convert-f
  [from f to]
  #(.setText to (f (.getText from))))

(defn make-convert-fs
  "bindings => f to"
  [from & bindings]
  (let [fs (map #(apply make-convert-f from %) (partition 2 bindings))]
    #(doseq [f fs] (f))))
  
(defn main
  "Main function, if exit? then after closing window application will exit."
  ([exit?]
     (try
      (doseq [laf (javax.swing.UIManager/getInstalledLookAndFeels)]
	(when (= (.getName laf) "Nimbus")
	  (javax.swing.UIManager/setLookAndFeel (.getClassName laf))))
      (catch Exception e))
     (let [frame (doto (JFrame. "Morse and Braille by Szymon Witaborski")
		   (.setDefaultCloseOperation (if exit?
						JFrame/EXIT_ON_CLOSE
						JFrame/DISPOSE_ON_CLOSE))
		   (.setSize 700 600))
	   input-area (make-tarea)
	   braille-area (make-tarea false)
	   morse-area (make-tarea false)
	   outs-split (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT)
			(.add (with-scrollbars braille-area))
			(.add (with-scrollbars morse-area)))
	   main-split (doto (JSplitPane. JSplitPane/HORIZONTAL_SPLIT)
			(.add (with-scrollbars input-area))
			(.add outs-split))
	   toolbar (doto (JToolBar.)
		     (.setFloatable false)
		     (.add (make-button "Open" #()))
		     (.add (make-button "Save" #()))
		     (.addSeparator)
		     (.add (make-button "Morse!" (make-convert-f
						  input-area morse morse-area)))
		     (.add (make-button "Braille!" (make-convert-f
						    input-area braille braille-area)))
		     (.add (make-button "All!" (make-convert-fs input-area
								morse morse-area
								braille braille-area)))
		     (.addSeparator)
		     (.add (make-button "Save Braille" #()))
		     (.add (make-button "Save Morse" #())))
	   cpane (doto (.getContentPane frame)
		   (.setLayout (BorderLayout. 5 5))
		   (.add toolbar BorderLayout/NORTH)
		   (.add main-split BorderLayout/CENTER))]
       (.setVisible frame true)))
  ([] (main true)))
