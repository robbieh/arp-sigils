(defproject arp-sigils "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [instaparse "1.4.12"]
                 [clojure2d "1.4.4"]
                 [org.clojure/tools.cli "1.0.214"]
                 [generateme/fastmath "2.1.8" :exclusions [com.github.haifengl/smile-mkl org.bytedeco/openblas]]
                 ]
  :main ^:skip-aot arp-sigils.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
