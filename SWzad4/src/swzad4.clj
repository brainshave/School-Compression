(ns swzad4
  (use (sw gui)
       (clojure.contrib pprint)
       (clojure inspector))
  (import (java.io FileInputStream
		   FileOutputStream
		   BufferedInputStream
		   BufferedOutputStream)))
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

(defn concat-blocks ;;TODO zmienic na lazy-cat
  [blocks]
  (->> blocks (map first) (apply concat)))

(defn encode-block-seq [blocks]
  (-> blocks ((fn f [[[block tag] & blocks-rest]]
		(when-not (empty? block)
		  (let [length (count block)
			byte-1 (bit-shift-right length 8)
			byte-2 (bit-and length 255)]
		  (lazy-cat (lazy-cat [byte-1 byte-2 tag]
				      block)
			    (f blocks-rest))))))))

(defn decode-block-seq [s]
  (-> s ((fn f [[byte-1 byte-2 tag & tail]]
	   (when-not (empty? tail)
	     (let [length (bit-or (bit-shift-left byte-1 8)
				  byte-2)
		   [block s-rest] (split-at length tail)]
	       (cons [block tag]
		     (lazy-seq (f s-rest)))))))))

(defn test-block-encode [fin fout]
  (->> fin file-byte-seq
       block-seq
       encode-block-seq
       (write-seq fout)))

(defn test-block-decode [fin fout]
  (->> fin file-byte-seq
       decode-block-seq
       concat-blocks
       (write-seq fout)))