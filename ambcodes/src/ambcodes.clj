(ns #^{:author "Szymon Witamborski"
       :doc "Coding Ambiguity"}
  ambcodes
  (:use (clojure.contrib pprint
			 [string :only (split-lines split)]
			 [io :only (spit)]))
  (:require (clojure.contrib [seq :as seq])
	    (sw [gui :as gui]))
  (:import (java.awt BorderLayout)
	   (javax.swing JFileChooser
			JSplitPane
			JOptionPane
			JToolBar
			Box BoxLayout
			SpinnerNumberModel)))

(defn starts-with
  "Returns true if sequence a starts with b"
  [a b]
  (= (take (count b) a)
     b))

(def *suffixes-area* nil)

(defn suffix
  [a b]
  (let [a (seq a) b (seq b)]
    (let [f #(if (starts-with %1 %2)
	       (drop (count %2) %1))
	  s (or (f a b) (f b a))]
      s)))

(defn filter-not-nil [coll]
  (filter #(not= nil %) coll))

(defn find-suffixes
  ([code suffixes]
     (distinct
      (filter-not-nil
       (for [c code s suffixes :when (not= c s)]
	 (let [new-suffix (suffix c s)]
	   (if (and *suffixes-area* (not (empty? new-suffix)))
	     (.append *suffixes-area* (-> (apply str new-suffix) (str \newline))))
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
	  ;;(println suffixes \tab candidates \tab amb)
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
  (seq (Integer/toBinaryString
	(inc (Integer/valueOf (apply str word) 2)))))

(def cardinal-words (iterate next-word '(0)))

(defn rand-alphabet [max-len]
  (repeatedly #(rand-word max-len)))

(defn stupid-generator
  "Stupid generator, just generating random alphabets of n length with
words of max-len length.
Stops after max-tries"
  ([n max-len max-tries]
     (loop [alphabet (take n (rand-alphabet max-len))
	    tries (int 0)]
       (cond (== (inc tries) max-tries) nil
	     (ambiguous? alphabet) (recur (take n (rand-alphabet max-len)) (inc tries))
	     :default (with-meta alphabet {:tries tries}))))
  ([n] (stupid-generator n (int (* n 3/4)) (* n n n))))

(defn remove-ambiguities [code strategy]
  (loop [code (make-chars-set code)]
    (let [amb (ambiguous? code)]
      ;(println amb)
      ;(Thread/sleep 100)
      (cond
       (false? amb) code
       (vector? amb) (recur (if (code (second amb))
			      (disj code (strategy amb))
			      (disj code (first amb))))
       :word (recur (disj code amb))))))

(defn smart-generator
  ([n words strategy]
     (loop [code #{}
	    word (first words)
	    words (rest words)]
       (cond (== n (count code)) code
	     (code word) (recur code (first words) (rest words))
	     :default (recur (remove-ambiguities (conj code word)
						 strategy)
			     (first words) (rest words)))))
  ([n strategy] (smart-generator
		 n (repeatedly #(rand-word (if (< n 10) n
					       (int (* n 3/4)))))
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

(defn average [nums]
  (/ (reduce + nums) (count nums)))

(defn average-len [code]
  (average (map count code)))


(defn stddist
  ([avg nums]
     (Math/sqrt (double (/ (reduce #(let [a (- %2 avg)] (+ %1 (* a a)))
				   0 nums)
			   (count nums)))))
  ([nums] (stddist (average nums) nums)))

(defn test-strategy [t n strategy]
  (let [codes (repeatedly t #(with-time (smart-generator n strategy)))
	times (map #(:nano-time (meta %)) codes)
	avgtime (/ (reduce + times) t)
	disttime (stddist avgtime times)
	lengths (map count (reduce concat codes))
	avglen (average lengths)
	distlen (stddist avglen lengths)]
    ["Kody:" (map (fn [c] (map #(apply str %) c)) codes)
     "Średnia dł. wyrazu:" (double avglen)
     "Odchylenie std. dł. wyrazu:" (double distlen)
     "Średni czas [ns]:" (double avgtime)
     "Odchylenie std. czasu:" (double disttime)]))

(defn test-cardinal
  ([n strategy]
     (let [code (with-time (smart-generator n cardinal-words strategy))
	   code-lengths (map count code)
	   avglen (average code-lengths)
	   distlen (stddist avglen code-lengths)
	   time (:nano-time (meta code))]
       ["Kod:" (map #(apply str %) code)
	"Śr. dł. wyrazu:" (double avglen)
	"Odchylenie std. dł. wyrazu:" distlen
	"Czas [ns]:" (double time)]))
  ([t n strategy] (test-cardinal n strategy)))

(def chooser (JFileChooser.))

(defn choose-file
  "Open chooser for opening or saving"
  [parent open?]
  (let [;;chooser (JFileChooser.)
	status (if open?
		 (.showOpenDialog chooser parent)
		 (.showSaveDialog chooser parent))]
    (if (= status JFileChooser/APPROVE_OPTION)
      (.getAbsolutePath (.getSelectedFile chooser)))))

(def stupid-name "Losowe wyrazy")
(def cardinal-name "Kolejne wyrazy")

(def mode-names {"Losowe wyrazy" test-strategy
		 "Kolejne wyrazy" test-cardinal})

(def strategy-names {"Usuwaj dłuższe" (fn [[f s]]
					(if (> (count f) (count s))
					  f s))
		     "Usuwaj krótsze" (fn [[f s]]
					(if (< (count f) (count s))
					  f s))})

(def generator-frame
     [:frame {:id :main-frame}
      [Box {:id :generator-toolbar}
       [:combo-box {:id :mode-combo}]
       [:combo-box {:id :strategy-combo}]
       [:label {:text "Ilość słów: "}]
       [:spinner {:id :words-count}]
       [:label {:text "Ilość testów: "}]
       [:spinner {:id :tests-count}]
       [:button {:id :generate-button}]
       [:button {:id :save-button}]]
      [:scroll-pane {:constraint BorderLayout/CENTER
		     :params [[:text-area {:id :generator-output}]]}]])

(defn generator-go [e this ids & _]
  (let [words (.getValue (@ids :words-count))
	tests (.getValue (@ids :tests-count))
	f (mode-names (.. (@ids :mode-combo) getModel getSelectedItem))
	strategy (strategy-names (.. (@ids :strategy-combo) getModel getSelectedItem))
	]
    (.setText (@ids :generator-output)
	      (with-out-str (pprint (f tests words strategy))))))

(defn file-dialog
  [open? area-id e this ids & _]
  (if-let [f (choose-file (@ids :main-frame) open?)]
    (if open?
      (-> @ids area-id (.setText (slurp f)))
      (->> @ids area-id .getText (spit f)))))

(def generator-styles
     [[:main-frame] {:title "Generowanie kodów"
		     :visible true
		     :size [500 300]}
      [:generator-toolbar] {:constraint BorderLayout/WEST
			    :params [BoxLayout/Y_AXIS]}
      [:mode-combo] {:params [(into-array Object (keys mode-names))]}
      [:strategy-combo] {:params [(into-array Object (keys strategy-names))]}
      [:words-count :tests-count] {:params [[SpinnerNumberModel {:params [10 1 1000000 1]}]]}
      [:generate-button] {:text "Generuj" :onmcc generator-go}
      [:save-button] {:text "Zapisz wyjście"
		      :onmcc (partial file-dialog false :generator-output)}
      ])


(def main-frame
     [:frame {:id :main-frame}
      [:tool-bar {:id :main-toolbar}
       [:button {:id :open-generator-button}]
       [:button {:id :open-button}]
       [:button {:id :save-button}]
       [:button {:id :check-button}]]
      [:split-pane {:id :main-split}
       [:scroll-pane
	{:id :main-scroll
	 :params [[:text-area {:id :code-area}]]}]
       [:scroll-pane
	{:id :suffixes-scroll
	 :params [[:text-area {:id :suffixes}]]}]]
      [:label {:id :status-bar}]])


(def invite-text "Wpisz tu kod lub załaduj z pliku.
Słowa kodowe rozdzielaj dowolnymi białymi znakami.
Wynik działania algorytmu pojawi się poniżej.")

(def styles
     [[:main-frame] {:title "Niejednoznaczność kodów"
		     :visible true
		     :size [400 500]}
      [:main-toolbar] {:constraint BorderLayout/NORTH}
      [:main-split] {:constraint BorderLayout/CENTER
		     :params [JSplitPane/HORIZONTAL_SPLIT]
		     :resize-weight (float 2/3)}
					;[:main-scroll] {}
      [:status-bar] {:constraint BorderLayout/SOUTH
		     :text " "}
      [:code-area] {:text invite-text}
      [:suffixes] {:editable false}
      [:open-generator-button] {:text "Otwórz generator"}
      [:open-button] {:text "Otwórz"}
      [:save-button] {:text "Zapisz"}
      [:check-button] {:text "Sprawdź"}])


(defn run-check [e this ids groups & _]
  (.setText (@ids :suffixes) "")
  (binding [*suffixes-area* (@ids :suffixes)]
    (let [code (split #"\s+" (.getText (@ids :code-area)))
	  amb (ambiguous? code)]
      (.setText (@ids :status-bar)
		(cond
		 (false? amb) "Kod jednoznaczny."
		 (string? amb) (str "W kodzie powtarza się wyraz " amb)
		 (vector? amb) (str "W kodzie "
				    (reduce str (amb 0)) " i "
				    (reduce str (amb 1)) " mają sufiks "
				    (reduce str (amb 2)) " będący słowem kodowym."))))))

(def actions
     [[:code-area]
      {:onmcc (fn [e this & _]
		(if (= (.getText this) invite-text)
		  (.setText this "")))}
      [:check-button]
      {:onmcc run-check}
      [:open-button]
      {:onmcc (partial file-dialog true :code-area)}
      [:save-button]
      {:onmcc (partial file-dialog false :code-area)}
      [:open-generator-button]
      {:onmcc (fn [& _] (gui/parse-gui generator-frame generator-styles))}])


(defn start []
  (gui/parse-gui main-frame (concat styles actions)))