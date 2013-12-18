(defproject parsero "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :profiles {:uberjar {:aot :all}
             :dev {:main bench
                   :dependencies [[criterium "0.4.2"]
                                  [org.clojure/data.json "0.2.3"]]}}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/algo.monads "0.1.4"]
                 [org.clojure/core.typed "0.2.19"]])
