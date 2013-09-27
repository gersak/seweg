(ns seweg.asn1.primitive
  (:require frak))

(def primitive-types
  #{"BOOLEAN" 
    "NULL"
    "INTEGER"
    "REAL"
    "BITSTRING"
    "OCTET STRING"
    "OBJECT IDENTIFIER"
    "EXTERNAL"
    "ENUMERATED"
    "CHARACTER STRING"
    "UTCTime"
    "GeneralizedTime"
    "CHOICE"
    "SEQUENCE"
    "SET"
    "SEQUENCE OF"
    "SET OF"})

(defn primitive-type? [text]
  (re-find (frak/pattern primitive-types) text))


