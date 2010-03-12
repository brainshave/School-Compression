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
			SwingConstants
			JOptionPane
			JDialog
			JFileChooser)
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

(defn unmorse
  "Decodes Morse code."
  [s]
  (apply str (map #(or (if-let [a (= % "---- ")] "ch") (morse-data-inv %) %)
		  (string/partition #"[.-]+\s" s))))


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
  (apply str (interleave (map #(apply str %) (partition 10 10 (repeat nil)
							(map braille-data s)))
			 (repeat \newline))))

(defn unbraille
  "Decodes from Braille"
  [s]
  (apply str (let [ss (map #(apply str %)
			   (partition 6 (apply str "______"
					       (filter #(not= \newline %) s))))]
	       (map #(some braille-data-inv (list (str %1 %2) %2))
		    ss (rest ss)))))

(defn statistics
  [[orignal-text braille-text morse-text :as params]]
  (let [[orc brc moc :as cs] (map count params)]
    {:braile (/ brc orc)
     :morse (/ moc orc)}))


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
					;(.setLineWrap true)
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

(defn help [parent]
  (JOptionPane/showMessageDialog
   parent
   "Funkcje programu:
  Kodowanie Braille'a:
  - tekst wynikowy jest podzielony po 10 znaków kodu w jednej linii,
    automatyczne zawijanie linii w edytorze było zbyt wolne;
  - funkcja 'Decode Braille' odkodowuje Braille'a
    - nie jest przeprowadzane sprawdzanie czy nawias jest zamykający czy otwierający.
  Kodowanie Morse'a:
  - zachowywany jest podział linii z pliku wejściowego;
  - analogicznie istnieje funkcja 'Decode Morse'.
  Statystyki:
  - funkcja 'Stats' oblicza średnią ilość bajtów w poszczególnych kodach
    przypadających na jeden bajt pliku wejściowego.

Dla większych plików kodowanie/dekodowanie może trwać nawet kilkanaście sekund.

Myślę, że typowe użycie programu złoży się z użycia trzech przycisków:
  Load (załadowanie pliku),
  Both (zakodowanie w obu systemach)
  i Stats (wyświetlenie podsumowania).

Autor: Szymon Witamborski, santamon@gmail.com"
   "Info/Help"
   JOptionPane/INFORMATION_MESSAGE))

(defn make-show-inv-f
  [parent from f to]
  (let [dialog (doto (JDialog. parent "Inverted" false)
		 (.setSize 500 300))
	tarea (make-tarea)
	cpane (doto (.getContentPane dialog)
		(.setLayout (BorderLayout.))
		(.add (with-scrollbars tarea) BorderLayout/CENTER)
		(.add (make-button "Save" #())))
	convert-f (make-convert-f from f to)]
    #(do (convert-f)
	 (.setVisible dialog true))))

(def chooser (JFileChooser.))

(defn choose-file
  [parent open?]
  (let [;;chooser (JFileChooser.)
	status (if open?
		 (.showOpenDialog chooser parent)
		 (.showSaveDialog chooser parent))]
    (if (= status JFileChooser/APPROVE_OPTION)
      (.getAbsolutePath (.getSelectedFile chooser)))))

(defn make-open-f
  [parent tarea]
  #(if-let [fpath (choose-file parent true)]
     (.setText tarea (slurp fpath))))

(defn make-save-f
  [parent tarea]
  #(if-let [fpath (choose-file parent false)]
     (io/spit fpath (.getText tarea))))

(defn make-stats-f
  [parent input-area braille-area morse-area]
  #(let [in-count (count (.getText input-area))
	 br-c (count (.getText braille-area))
	 mo-c (count (.getText morse-area))
	 br-av (/ br-c in-count)
	 mo-av (/ mo-c in-count)]
     (JOptionPane/showMessageDialog
      parent
      (str in-count " original characters\n"
	   br-c " Braille characters\n"
	   mo-c " Morse characters\n\n"
	   "For one original byte we got:\n"
	   (float br-av) " Braille bytes (" br-av ")\n"
	   (float mo-av) " Morse bytes (" mo-av ")"))))


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
	   braille-area (make-tarea)
	   morse-area (make-tarea)
	   outs-split (doto (JSplitPane. JSplitPane/VERTICAL_SPLIT)
			(.setResizeWeight 0.5)
			(.add (with-scrollbars braille-area))
			(.add (with-scrollbars morse-area)))
	   main-split (doto (JSplitPane. JSplitPane/HORIZONTAL_SPLIT)
			(.setResizeWeight 0.5)
			(.add (with-scrollbars input-area))
			(.add outs-split))
	   toolbar (doto (JToolBar.)
		     (.setFloatable false)
		     (.add (make-button "Info" #(help frame)))
		     (.add (make-button "Open" (make-open-f frame input-area)))
		     (.add (make-button "Save" (make-save-f frame input-area)))
		     (.add (make-button "Stats" (make-stats-f frame input-area
							      braille-area
							      morse-area)))
		     (.addSeparator)
		     (.add (make-button "Braille" (make-convert-f
						   input-area braille braille-area)))
		     (.add (make-button "Morse" (make-convert-f
						 input-area morse morse-area)))
		     (.add (make-button "Both" (make-convert-fs input-area
								morse morse-area
								braille braille-area)))
		     (.addSeparator)
		     (.add (make-button "A" #(.setText input-area (.toUpperCase (.getText input-area)))))
		     (.add (make-button "a" #(.setText input-area (.toLowerCase (.getText input-area)))))
		     (.addSeparator)
		     (.add (make-button "Decode Braille" (make-convert-f
							  braille-area unbraille braille-area)))
		     (.add (make-button "Decode Morse" (make-convert-f
							morse-area unmorse morse-area)))
		     (.addSeparator)
		     (.add (make-button "Save Braille" (make-save-f frame braille-area)))
		     (.add (make-button "Save Morse" (make-save-f frame morse-area))))
	   cpane (doto (.getContentPane frame)
		   (.setLayout (BorderLayout.))
		   (.add toolbar BorderLayout/NORTH)
		   (.add main-split BorderLayout/CENTER))]
       (.setVisible frame true)))
  ([] (main true)))
