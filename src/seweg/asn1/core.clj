(ns seweg.asn1.core)

(def asn-special-signs 
  #{"." ".." "..." "," ";" "(" ")" "{" "}" "[" "]" "-" "<" "|" "::=" "“::=”" "“|”"})

(def asn-reader-signs 
  #{#"\.\." #"\.\.\." #"," #";" #"'" #"\(" #"\)" #"\{" #"\}" #"\[" #"\]" #"<" #">" #"(“\|”|\|)"  #"(“::=”|::=)"})

(def ans-definition-sign "::=")

(def ^:private asn-keywords
  #{"BOOLEAN"
    "INTEGER"
    "BIT"
    "STRING"
    "OCTET"
    "NULL"
    "SEQUENCE"
    "OF"
    "SET"
    "IMPLICIT"
    "CHOICE"
    "ANY"
    "EXTERNAL"
    "OBJECT"
    "IDENTIFIER"
    "OPTIONAL"
    "DEFAULT"
    "COMPONENTS"
    "UNIVERSAL"
    "APPLICATION"
    "PRIVATE"
    "TRUE"
    "FALSE"
    "BEGIN"
    "END"
    "DEFINITIONS"
    "EXPLICIT"
    "ENUMERATED"
    "EXPORTS"
    "IMPORTS"
    "REAL"
    "INCLUDES"
    "MIN"
    "MAX"
    "SIZE"
    "FROM"
    "WITH"
    "COMPONENT"
    "BY"
    "PLUS-INFINITY"
    "MINUS-INFINITY"
    "TAGS"})

(def asn-meaning?
  (memoize 
    (fn [^String word] 
      (cond 
        (= word (re-find #"^[a-z][\w\d-]+" word)) :valuereference
        (= word (re-find #"^[A-Z][a-z0-9-]+" word)) :typereference
        (= word (re-find #"^[A-Z][A-Za-z0-9-]+.[A-Z][a-z0-9-]+" word)) :external-typereferece
        (= word (re-find #"^[A-Z][A-Za-z0-9-]+.[a-z][\w\d-]+" word)) :external-valuereference
        (asn-special-signs word) :asn-special-sign
        (some (partial = word) asn-keywords) :known-keyword
        (= word (re-find #"[A-Z\-]+" word)) :unknow-keyword
        :else :asn-unknown))))

(defn ^:private remove-comments [^String text]
  (clojure.string/replace text #"--([^\n\r-]|-[^\n\r-])*(--|-?[\n\r])" ""))


(defn split-asn-elements [^String text]
  (assert (not= \" (first text)) "ASN module definition cannot start with \"")
  (letfn [(strech-special-signs [s]
            (reduce #(clojure.string/replace %1 %2 (fn [r] (str " " (if (string? r)  r (first r)) " "))) s asn-reader-signs))]
    (let [text (remove-comments text)
          marked-positions (keep-indexed #(if (= %2 \") %1) text)
          marked-positions (partition 2 (conj (into [0] (reduce #(into %1 [%2 %2]) [] marked-positions)) (count text)))]
      (vec (remove empty? (flatten (map-indexed #(if (odd? %1)
                                                   (subs text (first %2) (inc (second %2)))
                                                   (->
                                                     (if (= 0 %1)
                                                       (subs text (first %2) (second %2))
                                                       (subs text (inc (first %2)) (second %2))) 
                                                     strech-special-signs
                                                     (clojure.string/split  #"\s+")))
                                                marked-positions))))))) 
