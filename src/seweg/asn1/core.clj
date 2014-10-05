(ns seweg.asn1.core)

(def asn-special-signs 
  #{"." ".." "..." "," ";" "(" ")" "{" "}" "[" "]" "-" "<" "|" "::=" "\"::=\"" "\"|\""})

(def asn-reader-signs 
  #{#"\.\." #"\.\.\." #"," #";" #"'" #"\(" #"\)" #"\{" #"\}" #"\[" #"\]" #"<" #">" #"(\"\|\"|\|)"  #"(\"::=\"|::=)"})

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
    "TAGS"
    "VALUE"
    "NOTATION"})

(defprotocol ReferenceMemory
  (get-reference [this])
  (set-reference [this value]))

(def ^:dynamic *type-reference* (ref {}))
(def ^:dynamic *value-reference* (ref {}))
(def ^:dynamic *macro-reference* (ref {}))


(def ^:dynamic *local-type-reference* nil)
(def ^:dynamic *local-value-reference* nil)
(def ^:dynamic *local-macro-reference* nil)


(defprotocol ReferenceData
  (referencing? [this]))

(defrecord ASNTypeReference [reference]
  Object (toString [this] (str reference))
  ReferenceData (referencing? [_] ::TYPE))
(defrecord ASNValueReference [reference]
 Object (toString [this] (str reference))
 ReferenceData (referencing? [_] ::VALUE))
(defrecord ASNMacroReference [reference]
  Object (toString [this] reference)
  ReferenceData (referencing? [_] ::MACRO))
(defrecord ASNModuleReference [reference assigned-identifier]
  Object (toString [this] (str reference (when assigned-identifier "{" (apply str (interpose "," assigned-identifier)) "}")))
  ReferenceData (referencing? [_] ::MODULE))
(defrecord ASNExternalValueReference [modulereference reference]
  Object (toString [this] (str modulereference "." reference))
  ReferenceData (referencing? [_] ::VALUE))
(defrecord ASNExternalTypeReference [modulereference reference]
  Object (toString [this] (str modulereference "." reference))
  ReferenceData (referencing? [_] ::TYPE))
(defrecord ASNExternalMacroReference [modulereference reference]
  Object (toString [this] (str modulereference "." reference))
  ReferenceData (referencing? [_] ::MACRO))
(defrecord ASNProductionReference [reference]
  Object (toString [this] reference)
  ReferenceData (referencing? [_] ::MACRO))
(defrecord ASNLocalTypeReference [reference]
  Object (toString [this] (str reference))
  ReferenceData (referencing? [_] ::TYPE))
(defrecord ASNLocalValueReference [reference]
  Object (toString [this] (str reference))
  ReferenceData (referencing? [_] ::VALUE))

(defn asn-reference? [t]
  (satisfies? ReferenceData t))

(defn- asn-predicate [p]
  (fn [v] (clojure.pprint/write-out (apply str p (str v)))))

(defn- use-asn-pprint
  "Installs a function as a new method of multimethod associated with dispatch-value. "
  [multifn dispatch-val func]
  (. multifn addMethod dispatch-val func))


(def asn-pprint-dispatch clojure.pprint/simple-dispatch)
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNTypeReference (asn-predicate "Type: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNValueReference (asn-predicate "Value: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNModuleReference (asn-predicate "Module: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNMacroReference (asn-predicate "MACRO: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNExternalValueReference (asn-predicate "External Value: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNExternalTypeReference (asn-predicate "External Type: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNExternalMacroReference (asn-predicate "External MACRO: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNProductionReference (asn-predicate "ProductionReference: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNLocalTypeReference (asn-predicate "Local Type: "))
(use-asn-pprint asn-pprint-dispatch seweg.asn1.core.ASNLocalValueReference (asn-predicate "Local Value: "))


(defmacro print-asn [& body] 
  `(clojure.pprint/with-pprint-dispatch asn-pprint-dispatch
     (clojure.pprint/pprint ~@body)))

(def asn-meaning?
  (memoize 
    (fn [^String word] 
      (when (seq word) 
        (cond 
          (some (partial = word) asn-keywords) :known-keyword
          (= word (re-find #"^[a-z][\w\d-]+" word)) :valuereference
          (= word (re-find #"^[A-Z][\w\d-]+" word)) :typereference
          (= word (re-find #"^[A-Z][A-Z0-9-]+" word)) :macroreference
          (= word (re-find #"^[A-Z][A-Za-z0-9-]+.[A-Z][a-z0-9-]+" word)) :external-typereferece
          (= word (re-find #"^[A-Z][A-Za-z0-9-]+.[a-z][\w\d-]+" word)) :external-valuereference
          (asn-special-signs word) :asn-special-sign
          (= word (re-find #"[A-Z\-]+" word)) :unknow-keyword
          :else :asn-unknown)))))

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
