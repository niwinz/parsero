(ns parsero.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [parsero.core :refer [parser any-char raw-parse parse-error?]]
            [parsero.combinators :refer [is-char many letter]]))


(defn pstring-from-result
  [p s]
  (let [r (raw-parse p s)]
    (if (parse-error? r)
      r
      (second r))))

(def letters-and-newlines
  (parser
    [_ (many letter)
     _ (is-char \newline)]
    nil))

(deftest any-char-test
  (testing "Advances one line when parsing a newline"
    (is (= (:line (pstring-from-result any-char "\nAbc")) 1)))
  (testing "Advances one line per parsed newline"
    (is (= (:line (pstring-from-result (many any-char) "\n\n\nAbc")) 3)))
  (testing "Advances one column when parsing a regular character"
    (is (= (:col (pstring-from-result any-char "Abc")) 1)))
  (testing "Resets column to 0 after parsing a newline"
    (is (= (:col (pstring-from-result letters-and-newlines "Abc\na23")) 0))))
