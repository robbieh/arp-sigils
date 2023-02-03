(ns arp-sigils.core
  (:gen-class :main true)
  (:require [arp-sigils.draw :as draw]
            [clojure.tools.cli :as cli]))

(def cli-options
  [["-f" "--fullscreen" "Turn on fullscreen mode"]
   ["-s" "--size [WxH]" "Set width and height of windowed mode"]
   ["-n" "--no-arp" "Do no use input from the `arp` command"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)
        {:keys [size fullscreen help]} options
        ]
    (when help
      (do (println summary) (System/exit 1)))
    (when (and (not fullscreen) (nil? size))
      (do (println "Please pass either --fullscreen or --size [WxH]") (System/exit 1)))
    (if-not fullscreen
      (let [[w h]  (mapv clojure.edn/read-string (clojure.string/split size #"[xX]"))]
        (if (and (integer? w) (integer? h))
          (draw/start false w h)
          (println "Failed to parse size arguments [" w h "]")))
      (draw/start true)
      ))
  nil)

