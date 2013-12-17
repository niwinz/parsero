(ns parsero.combinators-test
  (:require [clojure.test :refer [deftest testing is]]
            [parsero.core :refer [parse-error? any-char parsed-value-and-remainder]]
            [parsero.combinators :refer [is-char letter digit many many1 sep-by sep-by1 surrounded-by skip-many skip-many1]]))

(def comma (is-char \,))

(deftest is-char-test
  (testing "The parser that parses any character"
    (is (= '(\A "bc") (parsed-value-and-remainder (is-char \A) "Abc")))
    (is (parse-error? (parsed-value-and-remainder (is-char \a) "Abc")))))

(deftest many-test
  (testing "A combinator that applies a parser repeatedly until it fails"
    (is (= '((\A \b \c) "") (parsed-value-and-remainder (many any-char) "Abc")))))

(deftest many1-test
  (testing "A combinator that applies a parser repeatedly at least once before it fails"
    (is (= '((\A) " ") (parsed-value-and-remainder (many1 letter) "A ")))
    (is (parse-error? (parsed-value-and-remainder (many1 digit) "Abc")))))

(deftest skip-many-test
  (testing "A combinator that applies a parser repeatedly until it fails discarding its results"
    (is (= '(nil " c") (parsed-value-and-remainder (skip-many letter) "Ab c")))))

(deftest skip-many1-test
  (testing "A combinator that applies a parser repeatedly at least once until it fails discarding its results"
    (is (= '(nil " c") (parsed-value-and-remainder (skip-many1 letter) "Ab c")))
    (is (parse-error? (parsed-value-and-remainder (skip-many1 digit) "Ab c")))))

(deftest sep-by-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (= '((\A) " ") (parsed-value-and-remainder (sep-by letter comma) "A ")))
    (is (= '((\A \b) "") (parsed-value-and-remainder (sep-by letter comma) "A,b")))))

(deftest sep-by1-test
  (testing "A combinator that takes yields the result of a parser while discarding the separator at least once until it fails"
    (is (parse-error? (parsed-value-and-remainder (sep-by1 letter comma) "A"))))
    (is (= '((\A \b \c) "") (parsed-value-and-remainder (sep-by1 letter comma) "A,b,c"))))

(deftest surrounded-by-test
  (testing "A combinator that consumes a prefix and suffix ignoring them"
    (is (parse-error? (parsed-value-and-remainder (surrounded-by comma letter comma) ",A")))
    (is (= '(\A "") (parsed-value-and-remainder (surrounded-by comma letter comma) ",A,")))))
