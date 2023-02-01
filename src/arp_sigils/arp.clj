(ns arp-sigils.arp
  (:require [clojure.java.shell :as sh]
            [clojure.string :as string]))

(def macs (atom nil))
(defn run-arp []
  (-> (sh/sh "arp" "-a")
    :out
    (string/split #"\n")))

(defn match-macs [arplines]
  (->> (run-arp)
    (map #(first (re-seq #"..:..:..:..:..:.." %)) )
    (remove nil?)))

(defn get-macs []
  (reset! macs (match-macs (run-arp))))


