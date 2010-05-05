(ns series.core
  (:use (sw gui)
	(clojure inspector))
  (:import (java.io FileInputStream
		    BufferedInputStream)
	   (java.awt Dimension
		     Color
		     Font
		     BorderLayout)
	   (javax.swing JPanel
			JFrame
			Box
			BoxLayout
			JFileChooser)))

(defn find-series [f]
  (with-open [bytes (-> f FileInputStream. BufferedInputStream.)]
    (loop [prev-byte -1
	   byte (.read bytes)
	   serie-count 0
	   series ()
	   loose-bytes -1]
      (let [same-bytes (== prev-byte byte)
	    serie-full (<= 255 serie-count)
	    serie (<= 4 serie-count)]
	(cond
	 (neg? byte)
	 (if serie
	   {:series (cons serie-count series) :loose-bytes loose-bytes}
	   {:series series :loose-bytes (+ loose-bytes serie-count)})
	   
	 (and same-bytes
	      (not serie-full))
	 (recur byte
		(.read bytes)
		(inc serie-count)
		series
		loose-bytes)

	 (or serie-full
	     (and (not same-bytes)
		  serie))
	 (recur byte
		(.read bytes)
		1
		(cons serie-count series)
		loose-bytes)
	 
	 :default
	 (recur byte
		(.read bytes)
		1
		series
		(+ loose-bytes serie-count)))))))


(defn get-ext [f]
  (let [ext (re-find #"\.[^.]+$" (.getName f))]
    (when ext
      (.toLowerCase (if (= \~ (last ext))
		      (apply str (butlast ext))
		      ext)))))

(defn file-info [f]
  {:file-name (.getName f)
   :file-ext (get-ext f)
   :size (.length f)})

(defn series-by-ext [files]
  (group-by :file-ext
	    (map merge (map file-info files) (map find-series files))))

(defn series-dir [dir]
  (series-by-ext (filter #(.isFile %) (file-seq (java.io.File. dir)))))

(defn sum-up-series [series]
  (let [loose-bytes (apply + (map :loose-bytes series))
	size (apply + (map :size series))
	files (count series)
	all-series (apply concat (map :series series))
	freqs (frequencies all-series)
	series-count (count all-series)
	average (if (not= 0 series-count)
		  (/ (apply + all-series) series-count)
		  0)
	maximum (if (not= 0 series-count)
		  (apply max (keys freqs))
		  0)
	ratio (if (not= 0 size)
		(/ (- size loose-bytes) size)
		0)]
    {:loose-bytes loose-bytes
     :size size
     :files files
     :freqs (into (sorted-map) freqs)
     :average (double average)
     :maximum maximum
     :file-ext (:file-ext (first series))
     :ratio (double ratio)}))

(defn merge-by-ext [series-map]
  (zipmap (keys series-map)
	  (map sum-up-series (vals series-map))))

(defn paint-histogram [g series]
  (let [{:keys [maximum freqs ratio files size file-ext average]} series
	max-height (apply max 0 (vals freqs))
	strings ["Plików: " (str " " files)
		 "Łączny rozmiar: " (format " %.3f MB" (double (/ size 1024 1024)))
		 "Procentowy udział serii:" (format " %.2f %%" (* 100 ratio))
		 "Średnia dł. serii:" (format " %.2f" average)
		 "Maksymalna dł. serii:" (str " " maximum)]]
    (doto g
      (.setColor Color/white)
      (.fillRect 0 0 519 299)
      (.setColor (Color. 245 255 245))
      (.fillRect 4 20 256 256)
      (.setColor (Color. 100 140 100))
      (.drawLine 4 19 260 19)
      (.drawString (str max-height " [ilość wystąpień serii o danej długości]") 4 17)
      (.drawLine 4 20 4 280)
      (.drawString "0" 0 292)
      (.drawLine 132 276 132 280)
      (.drawString "128" 120 292)
      (.drawLine 260 20 260 280)
      (.drawString "255 [długość serii]" 246 292)
      (.drawLine 4 276 260 276)
      (.setColor Color/black))
    (doseq [[s y] (map vector strings (iterate #(+ 20 %) 70))]
      (.drawString g (str s) 270 y))
    (doto g
      (.setFont (-> g .getFont (.deriveFont (float 40))))
      (.drawString (or file-ext "Bez rozsz.") 270 40))
    (doseq [[x y] freqs]
      (.drawLine g
		 (+ x 4) 276
		 (+ x 4) (- 276 (* y (/ 256 max-height)))))))

(defn panel-histogram [series]
  (let [size (Dimension. 500 300)]
    (doto (proxy [JPanel] []
	    (paint [g]
		   (paint-histogram g series)))
      (.setPreferredSize size)
      (.setMinimumSize size))))
		   
(defn test-histogram [series]
  (parse-gui
   [:frame {:size [420 400] :title "Test Histogramu" :visible true}
    (panel-histogram series)]))

(def chooser
     (:root (parse-gui
	     [:file-chooser
	      {:dialog-title "Wybierz katalog lub pliki do przeskanowania"
	       :dialog-type JFileChooser/OPEN_DIALOG
	       :file-selection-mode JFileChooser/FILES_AND_DIRECTORIES
	       :multi-selection-enabled true}])))
				
(defn scan-dir-action [e this ids & _]
  (when (== (.showDialog chooser this nil)
	    JFileChooser/APPROVE_OPTION)
    (let [inner (:main-scroll-inner @ids)]
      (.removeAll inner)
      (->> (.getSelectedFiles chooser)
	   (map file-seq)
	   (apply concat)
	   (filter #(.isFile %))
	   series-by-ext
	   merge-by-ext
	   vals
	   (map panel-histogram)
	   (map #(.add inner %))
	   doall)
      (.validate (:main-scroll @ids)))))

(def main-frame
     [:frame {:size [540 500]
	      :title "Badanie długości serii by SW"
	      :visible true}
      [:tool-bar {:constraint BorderLayout/NORTH}
       [:button {:text "Skanuj katalog..."
		 :onmcc scan-dir-action}]
       [:button {:text "Informacje"}]]
      [:scroll-pane {:constraint BorderLayout/CENTER
		     :id :main-scroll
		     :params [[Box {:id :main-scroll-inner
				    :params [BoxLayout/Y_AXIS]}]]}]])
(defn start []
  (parse-gui main-frame))
  