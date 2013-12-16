(ns examples.json
  (:require [clojure.algo.monads :refer [domonad]])
  (:require [parsero.core :refer [parser-m m-plus-parser]])
  (:require [parsero.combinators :refer [char-satisfies is-char any-char many sep-by number string]]))

; TODO ignore whitespace

(def parse-json-quotation-mark (is-char \"))

(def hex-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \A \b \B \c \C \d \D \e \E \f \F})
(def parse-hex-char (char-satisfies hex-chars))

(def parse-json-string-char
  (char-satisfies #(and (not= % \") (not= % \\))))

(def escape-chars {\" "\""
                   \\ "\\"
                   \/ \/
                   \b \backspace
                   \f \formfeed
                   \n \newline
                   \r \return
                   \t \tab})
(def parse-json-escaped-char
  (domonad parser-m
    [_ (is-char \\)
     c any-char
     :when (escape-chars c)]
    (escape-chars c)))

(def parse-json-unicode-code-point
  (domonad parser-m
    [u (is-char \u)
     a parse-hex-char
     b parse-hex-char
     c parse-hex-char
     d parse-hex-char]
    (char (Integer/parseInt (str a b c d) 16))))

(def parse-json-char
  (m-plus-parser
    parse-json-string-char
    parse-json-escaped-char
    parse-json-unicode-code-point))

(def parse-json-string-without-delimiters
  (domonad parser-m
    [c parse-json-char
     cs (many parse-json-char)]
    (apply str (cons c cs))))

(def parse-json-string
  (domonad parser-m
    [_ (is-char \")
     s parse-json-string-without-delimiters
     _ (is-char \")]
    s))

; TODO: JSON standard compliant
(def parse-json-number number)

(def parse-json-object-key
  (domonad parser-m
    [s parse-json-string]
    s))

(def parse-json-true
  (domonad parser-m
    [_ (string "true")]
    true))

(def parse-json-false
  (domonad parser-m
    [_ (string "false")]
    false))

(def parse-json-null
  (domonad parser-m
    [_ (string "null")]
    nil))


(declare parse-json-value)
(def parse-json-key-value
  (domonad parser-m
    [k parse-json-object-key
     _ (is-char \:)
     v parse-json-value]
    {k v}))

(def parse-json-object
  (domonad parser-m
    [_ (is-char \{)
     kvs (sep-by parse-json-key-value (is-char \,))
     _ (is-char \})]
    (reduce merge (cons {} kvs))))

(def parse-json-array
  (domonad parser-m
    [_ (is-char \[)
     vs (sep-by parse-json-value (is-char \,))
     _ (is-char \])]
     (into [] vs)))

(def parse-json-value
  (m-plus-parser
    parse-json-string
    parse-json-number
    parse-json-true
    parse-json-false
    parse-json-null
    parse-json-object
    parse-json-array))
