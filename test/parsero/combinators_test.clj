(ns parsero.combinators-test
  (:require [clojure.test :refer [deftest testing is]]
            [parsero.core :refer [parse-error?]]
            [parsero.combinators :refer [any-char is-char letter digit many many1 sep-by sep-by1 surrounded-by]]))

(def comma (is-char \,))

(deftest is-char-test
  (testing "The parser that parses any character"
    (is (= '(\A "bc") ((is-char \A) "Abc")))
    (is (parse-error? ((is-char \a) "Abc")))))

(deftest many-test
  (testing "A combinator that applies a parser repeatedly until it fails"
    (is (= '((\A \b \c) "") ((many any-char) "Abc")))))

(deftest many1-test
  (testing "A combinator that applies a parser repeatedly at least once before it fails"
    (is (= '((\A) " ") ((many1 letter) "A ")))
    (is (parse-error? ((many1 digit) "Abc")))))

(deftest sep-by-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (= '((\A) " ") ((sep-by letter comma) "A ")))
    (is (= '((\A \b) "") ((sep-by letter comma) "A,b")))))

(deftest sep-by1-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (parse-error? ((sep-by1 letter comma) "A"))))
    (is (= '((\A \b \c) "") ((sep-by1 letter comma) "A,b,c"))))

(deftest surrounded-by-test
  (testing "A combinator that consumes a prefix and suffix ignoring them"
    (is (parse-error? ((surrounded-by comma letter comma) ",A")))
    (is (= '(\A "") ((surrounded-by comma letter comma) ",A,")))))
