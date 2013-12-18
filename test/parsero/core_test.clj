(ns parsero.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [parsero.core :refer [parser any-char raw-parse parse-error?]]
            [parsero.combinators :refer [is-char many letter is-char]]))


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

(def abc
  (parser
    [a (is-char \a)
     b (is-char \b)
     c (is-char \c)]
    [a b c]))

(def abc-newline-three-chars
  (parser
    [_ abc
     _ (is-char \newline)
     _ any-char
     _ any-char
     _ any-char]
    nil))

(deftest any-char-test
  (testing "Advances one line when parsing a newline"
    (is (= 1 (:line (pstring-from-result any-char "\nAbc")))))
  (testing "Advances one line per parsed newline"
    (is (= 3 (:line (pstring-from-result (many any-char) "\n\n\nAbc")))))
  (testing "Advances one column when parsing a regular character"
    (is (= 1 (:col (pstring-from-result any-char "Abc")))))
  (testing "Resets column to 0 after parsing a newline"
    (is (= 0 (:col (pstring-from-result letters-and-newlines "Abc\na23"))))))

(deftest parse-error-test
  (testing "Gives the position of the error"
    (is (parse-error? (raw-parse any-char "")))
    (is (= 0 (:line (raw-parse any-char ""))))
    (is (= 0 (:col (raw-parse any-char ""))))
    (is (parse-error? (raw-parse abc "ab1")))
    (is (= 0 (:line (raw-parse abc "ab1"))))
    (is (= 2 (:col (raw-parse abc "ab1"))))
    (is (parse-error? (raw-parse abc-newline-three-chars "abc\nab")))
    (is (= 1 (:line (raw-parse abc-newline-three-chars "abc\nab"))))
    (is (= 2 (:col (raw-parse abc-newline-three-chars "abc\nab"))))))
