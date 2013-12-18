(ns bench
  (:use criterium.core)
  (:require [clojure.data.json :as json]
            [examples.json :refer [parse-json]])
  (:gen-class))

(def ^:private data (json/write-str {:foo 2, :bar ["hello" "world", ["with", ["nested" "list"]]],
                                     :and {:nested 1 :map 2}}))

(defn bench-core-json
  []
  (json/read-str data))


(defn bench-parsero-json
  []
  (parse-json data))


(defn -main
  [& args]
  (println "****************************************** code.json *********************************")
  (with-progress-reporting (bench (bench-core-json) :verbose))
  (println "****************************************** parsero.json ******************************")
  (with-progress-reporting (bench (bench-parsero-json) :verbose)))
