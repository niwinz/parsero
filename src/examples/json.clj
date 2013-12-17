(ns examples.json
  (:require [parsero.core :refer [parse parser one-of gives any-char]])
  (:require [parsero.combinators :refer [char-satisfies is-char unsigned-number number many sep-by string skip-many surrounded-by]]))


(def json-quotation-mark (is-char \"))

(def hex-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \A \b \B \c \C \d \D \e \E \f \F})
(def hex-char (char-satisfies hex-chars))

(def json-string-char
  (char-satisfies #(and (not= % \") (not= % \\))))

(def escape-chars {\" "\""
                   \\ "\\"
                   \/ \/
                   \b \backspace
                   \f \formfeed
                   \n \newline
                   \r \return
                   \t \tab})
(def json-escaped-char
  (parser
    [_ (is-char \\)
     c any-char
     :when (escape-chars c)]
    (escape-chars c)))

(def json-unicode-code-point
  (parser
    [u (is-char \u)
     a hex-char
     b hex-char
     c hex-char
     d hex-char]
    (char (Integer/parseInt (str a b c d) 16))))

(def json-char
  (one-of
    json-string-char
    json-escaped-char
    json-unicode-code-point))

(def json-string-without-delimiters
  (parser
    [c json-char
     cs (many json-char)]
    (apply str (cons c cs))))

(def json-string
  (parser
    [_ (is-char \")
     s json-string-without-delimiters
     _ (is-char \")]
    s))

(def json-number-sign
  (one-of
    (parser
      [_ (is-char \-)]
      -)
    (gives identity)))

(def json-number-integral
  (one-of
    (parser
      [_ (is-char \0)]
      0)
    number))

(def json-number-fraction
  (one-of
    (parser
      [_ (is-char \.)
       n unsigned-number]
      (fn [r]
        (let [op (if (>= r 0) + -)]
          (op r (/ n (Math/pow 10 (count (str n))))))))
    (gives identity)))

(def json-number-exponent
  (one-of
    (parser
      [_ (char-satisfies #{\e \E})
       n number]
      (fn [r]
        (* r (Math/pow 10 n))))
    (gives identity)))

(def json-number
  (parser
    [s json-number-sign
     i json-number-integral
     f json-number-fraction
     e json-number-exponent]
    (-> (s i) f e)))

(def json-whitespace-chars #{\space \tab \newline \return})
(def json-whitespace (char-satisfies json-whitespace-chars))
(def skip-whitespace (skip-many json-whitespace))
(defn trim
  [p]
  (surrounded-by skip-whitespace p skip-whitespace))

(def json-object-key
  (parser
    [s json-string]
    s))

(def json-true
  (parser
    [_ (string "true")]
    true))

(def json-false
  (parser
    [_ (string "false")]
    false))

(def json-null
  (parser
    [_ (string "null")]
    nil))

(declare json-value)
(def json-key-value
  (parser
    [k json-object-key
     _ (trim (is-char \:))
     v json-value]
    {k v}))

(def json-object
  (parser
    [_ (trim (is-char \{))
     kvs (sep-by json-key-value (trim (is-char \,)))
     _ (trim (is-char \}))]
    (reduce merge (cons {} kvs))))

(def json-array
  (parser
    [_ (trim (is-char \[))
     vs (sep-by json-value (trim (is-char \,)))
     _ (trim (is-char \]))]
     (into [] vs)))

(def json-value
  (one-of
    json-string
    json-number
    json-true
    json-false
    json-null
    json-object
    json-array))

(def parse-json (partial parse json-value))
