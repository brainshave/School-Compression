(ns swzad4
  (:use (sw gui)
	(clojure.contrib pprint)
	(clojure inspector))
  (:require (clojure.contrib (string :as string)))
  (:import (java.io FileInputStream
		    FileOutputStream
		    BufferedInputStream
		    BufferedOutputStream)
	   (javax.swing Box
			BoxLayout
			JFileChooser
			JFrame
			WindowConstants)))

(def *full-set*
     (reduce conj #{} (range 255)))

(defn file-byte-seq 
  "Czyta plik f i zamienia go na leniwą sekwencję bajtów."
  [f]
  (let [in (-> f FileInputStream. BufferedInputStream.)]
    (-> () ((fn f [s]
	      (let [b (.read in)]
		(if (== b -1)
		  (.close in)
		  (cons b (lazy-seq (f s))))))))))

(defn write-seq
  "Zapisuje strumien s do pliku f."
  [f s]
  (with-open [out (-> f FileOutputStream. BufferedOutputStream.)]
    (doseq [b s]
      (.write out b))))

(defn next-block
  "Zwraca wektor [reszta blok znacznik],
   gdzie blok + reszta = s,
   znacznik - nieuzyty bajt w bloku"
  [s]
  (let [opt-1 (int 1)
	opt-2-16 (int 65535)]
    (loop [s s,
	   block (transient []),
	   block-size (int 0),
	   flags (transient *full-set*)]
      (if (or (== opt-2-16 block-size)
	      (== opt-1 (count flags))
	      (empty? s))
	[s
	 (persistent! block)
	 (first (persistent! flags))]
	(let [b (first s)]
	  (recur (rest s)
		 (conj! block b)
		 (inc block-size)
		 (disj! flags b)))))))

(defn block-seq
  "Zwraca leniwą sekwencję wektorów [blok znacznik]"
  [s]
  (-> s ((fn f [s]
	   (when-not (empty? s)
	     (let [[s-rest block tag] (next-block s)]
	       (cons [block tag] (lazy-seq (f s-rest)))))))))

(defn concat-blocks
  "Skleja leniwie bloki, bez tagów."
  [blocks]
  (->> blocks ((fn f [[[block] & blocks-rest]]
		 (when-not (empty? block)
		   (lazy-cat block
			     (f blocks-rest)))))))

(defn encode-block-seq
  "Leniwie koduje bloki w jedna sekwencje, kolejne bajty:
  1. tag
  2. długość - starsze 8 bitów
  3. długość - młodsze 8 bitów
  4- blok
  Po bloku tag kolejnego bloku"
  [blocks]
  (-> blocks ((fn f [[[block tag] & blocks-rest]]
		(when-not (empty? block)
		  (let [length (count block)
			byte-1 (bit-shift-right length 8)
			byte-2 (bit-and length 255)]
		    (lazy-cat (lazy-cat [byte-1 byte-2 tag]
					block)
			      (f blocks-rest))))))))

(defn decode-block-seq 
  "Leniwie dekoduje bloki z postaci z encode-block-seq
   do sekwencji wektorów [blok tag]"
  [s]
  (-> s ((fn f [[byte-1 byte-2 tag & tail]]
	   (when-not (empty? tail)
	     (let [length (bit-or (bit-shift-left byte-1 8)
				  byte-2)
		   [block s-rest] (split-at length tail)]
	       (cons [block tag]
		     (lazy-seq (f s-rest)))))))))

(defn test-block-encode
  "Testuje kodowanie bloków bez kompresji."
  [fin fout]
  (->> fin file-byte-seq
       block-seq
       encode-block-seq
       (write-seq fout)))

(defn test-block-decode
  "Testuje dekodowanie bloków bez kompresji."
  [fin fout]
  (->> fin file-byte-seq
       decode-block-seq
       concat-blocks
       (write-seq fout)))

(defn compress-block
  "Kompresuje blok i zwraca [spakowany_blok tag]."
  [[block tag]]
  [(->> block
	(map char)
	(apply str)
	(re-seq #"(?s)(.)\1{0,254}")
	(map first)
	(map #(if (>= (count %) 4)
		(str
		 (char tag)
		 (char (count %))
		 (first %))
		%))
	(apply concat)
	(map int))	
   tag])

(defn decompress-block
  "Dekompresuje blok"
  [[block tag]]
  [(map int (string/replace-by
	     (re-pattern (str "(?s)\\Q" (char tag) "\\E.."))
	     (fn [[tag len c]] (apply str (repeat (int len) c)))
	     (->> block (map char) (apply str))))
   tag])

(defn compress 
  "Kompresuje plik fin do fout."
  [fin fout]
  (time (->> fin file-byte-seq
	     block-seq
	     (map compress-block)
	     encode-block-seq
	     (write-seq fout))))

(defn decompress
  "Dekompresuje plik fin do fout."
  [fin fout]
  (time (->> fin file-byte-seq
	     decode-block-seq
	     (map decompress-block)
	     concat-blocks
	     (write-seq fout))))

(def chooser (JFileChooser.))

(defn choose-file
  "Open chooser for opening or saving"
  [parent open?]
  (let [status (if open?
		 (.showOpenDialog chooser parent)
		 (.showSaveDialog chooser parent))]
    (if (= status JFileChooser/APPROVE_OPTION)
      (.getSelectedFile chooser))))

(defn compress-click [f]
  (fn [e this ids & _]
    (let [fin (choose-file (@ids :main-frame) true)
	  fout (choose-file (@ids :main-frame) false)]
      (when (and fin fout)
	(let [t (with-out-str (f fin fout))
	      fin-size (.length fin)
	      fout-size (.length fout)
	      ratio (double (* 100 (/ fout-size fin-size)))]
	  (-> @ids :size-in (.setText (str "wej: " fin-size)))
	  (-> @ids :size-out (.setText (str "wyj: " fout-size)))
	  (-> @ids :ratio (.setText (format "wsp: %.3g%%" ratio)))
	  (-> @ids :time (.setText t)))))))

(def main-frame
     [:frame {:title "Kompresja/dekompresja by S/W"
	      :size [400 150]
	      :visible true
	      :id :main-frame}
      [Box {:params [BoxLayout/PAGE_AXIS]}
       [:button {:text "Spakuj"
		 :onmcc (compress-click compress)}]
       [:button {:text "Rozpakuj"
		 :onmcc (compress-click decompress)}]
       [:label {:text "wej: 0"
		:id :size-in}]
       [:label {:text "wyj: 0"
		:id :size-out}]
       [:label {:text "wsp: 100%"
		:id :ratio}]
       [:label {:text "t"
		:id :time}]]])

(defn start [closing?]
  (-> main-frame
      parse-gui
      :root
      (.setDefaultCloseOperation
       (if closing?
	 JFrame/EXIT_ON_CLOSE
	 WindowConstants/HIDE_ON_CLOSE))))