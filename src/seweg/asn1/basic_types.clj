(ns seweg.asn1.basic-types)

(defprotocol ASN1Type
  (tag [this])
  (toClojure [this])
  (toBER [this value]))

(defrecord BOOLEAN [val]
  ASN1Type
  (tag [_] 0x01)
  (toClojure [_] (when (#{"TRUE" "FALSE"} (clojure.string/trim text)

(defn BOOLEAN [^String text]
  {:pre [(#{"TRUE" "FALSE"} (clojure.string/trim text))]}
  (if (= "TRUE" (clojure.string/trim text)) true false))

(defn NULL [_] 0xff)
