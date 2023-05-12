(defproject arp-sigils "0.1.0-SNAPSHOT"
  :description "arp-sigils"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [clojure2d "1.4.5"]
                 [org.clojure/tools.cli "1.0.214"]
                 [generateme/fastmath "2.2.1" :exclusions [com.github.haifengl/smile-mkl org.bytedeco/openblas]]
                 ]
  ;:main ^:skip-aot arp_sigils.core
  :main arp-sigils.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot [arp-sigils.core]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true" ]
                       }
             })
