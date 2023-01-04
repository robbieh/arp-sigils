(ns arp-sigils.core
  (:gen-class :main true)
  (:require [arp-sigils.draw :as draw]))

(defn -main [& args]
  (draw/start true)
  nil
  )
