(ns ambcodes.main
  (:gen-class
   :init init)
  (:use (sw gui)
	ambcodes))

(defn -init []
  [[] (atom [])])

(defn -main [& a]
  (parse-gui main-frame (concat styles actions)))