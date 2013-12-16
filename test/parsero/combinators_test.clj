(ns parsero.combinators-test
  (:require [clojure.test :refer :all]
            [parsero.core :refer [parse parse-error?]]
            [parsero.combinators :refer :all]))

(deftest is-char-test
  (testing "The parser that parses any character"
    (is (= '(\A "bc") (parse (is-char \A) "Abc")))
    (is (parse-error? (parse (is-char \a) "Abc")))))

(deftest many-test
  (testing "A combinator that applies a parser repeatedly until it fails"
    (is (= '((\A \b \c) "") (parse (many any-char) "Abc")))))

(deftest many1-test
  (testing "A combinator that applies a parser repeatedly at least once before it fails"
    (is (= '((\A) " ") (parse (many1 letter) "A ")))
    (is (parse-error? (parse (many1 digit) "Abc")))))

(deftest sep-by-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (= '((\A) " ") (parse (sep-by letter digit) "A ")))))

(deftest sep-by1-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (parse-error? (parse (sep-by1 letter (is-char \,)) "A"))))
    (is (= '((\A \b \c) "") (parse (sep-by1 letter (is-char \,)) "A,b,c"))))

(run-tests)
