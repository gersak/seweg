(ns seweg.asn1.definitions.extractors
  (:require frak))

(def ^:private test-object-type-file (slurp "mibs/cisco/CISCO-CDP-MIB.my")) 
(def ^:private test-smi-extractor (slurp "mibs/ietf/SNMPv2-SMI")) 
(def ^:private test-type-extractor (slurp "mibs/ietf/SNMPv2-PDU")) 

(defmacro make-extractor [macro]
  (let [fn-name (symbol (str "extract-" (clojure.string/replace macro #"\s+" "_")))]
    `(defn- ~fn-name [^String text#]
       (let [pattern# (str "(?s)[A-Za-z0-9-]+\\s+" ~macro ".*?::=\\s*\\{.*?\\}")]
         (re-seq (re-pattern pattern#) text#)))))

(make-extractor "OBJECT-IDENTITY")
(make-extractor "OBJECT-TYPE")
(make-extractor "NOTIFICATION-TYPE")
(make-extractor "OBJECT-GROUP")
(make-extractor "NOTIFICATION-GROUP")
(make-extractor "MODULE-COMPLIANCE")
(make-extractor "AGENT-CAPABILITIES")
(make-extractor "TRAP-TYPE")

(defn extract-OBJECT_IDENTIFIER [^String text]
  (let [pattern #"(?s)[a-z][a-zA-Z][\w\d-]+\s+OBJECT\s+IDENTIFIER\s+::=\s*\{.*?\}"]
    (re-seq pattern text)))

(defn extract-TYPE-definitions [^String text]
  (let [pattern #"(?s)[A-Z][a-z][\w\d-]+\s+::="]
    (map #(re-find #"[A-Z][a-z][\w\d-]+" %) (re-seq pattern text))))

(defn type-test [type-name]
  ^{:type (keyword type-name)} {})


(defn remove-comments [^String text]
  (clojure.string/replace text #"--([^\n\r-]|-[^\n\r-])*(--|-?[\n\r])" ""))

(def asn-keywords (ref #{"DEFINITIONS"
                         "EXPLICIT"
                         "IMPLICIT"
                         "TAGS"
                         "BEGIN"
                         "END"
                         "EXPORTS"
                         "IMPORTS"
                         "FROM"
                         "MACRO"
                         "INTEGER"
                         "REAL"
                         "BOOLEAN"
                         "NULL"
                         "BIT"
                         "OCTET"
                         "STRING"
                         "ENUMERATED"
                         "SEQUENCE"
                         "SET"
                         "OF"
                         "CHOICE"
                         "UNIVERSAL"
                         "APPLICATION"
                         "PRIVATE"
                         "ANY"
                         "DEFINED"
                         "BY"
                         "OBJECT"
                         "IDENTIFIER"
                         "INCLUDES"
                         "MIN"
                         "MAX"
                         "SIZE"
                         "WITH"
                         "COMPONENT"
                         "COMPONENTS"
                         "PRESENT"
                         "ABSENT"
                         "OPTIONAL"
                         "DEFAULT"
                         "TRUE"
                         "FALSE"
                         "PLUS-INFINITY"
                         "MINUS-INFINITY"
                         "MODULE-IDENTITY"
                         "OBJECT-IDENTITY"
                         "OBJECT-TYPE"
                         "NOTIFICATION-TYPE"
                         "TRAP-TYPE"
                         "TEXTUAL-CONVENTION"
                         "OBJECT-GROUP"
                         "NOTIFICATION-GROUP"
                         "MODULE-COMPLIANCE"
                         "AGENT-CAPABILITIES"
                         "LAST-UPDATED"
                         "ORGANIZATION"
                         "CONTACT-INFO"
                         "DESCRIPTION"
                         "REVISION"
                         "STATUS"
                         "REFERENCE"
                         "SYNTAX"
                         "BITS"
                         "UNITS"
                         "ACCESS"
                         "MAX-ACCESS"
                         "MIN-ACCESS"
                         "INDEX"
                         "AUGMENTS"
                         "IMPLIED"
                         "DEFVAL"
                         "OBJECTS"
                         "ENTERPRISE"
                         "VARIABLES"
                         "DISPLAY-HINT"
                         "NOTIFICATIONS"
                         "MODULE"
                         "MANDATORY-GROUPS"
                         "GROUP"
                         "WRITE-SYNTAX"
                         "PRODUCT-RELEASE"
                         "SUPPORTS"
                         "VARIATION"
                         "CREATION-REQUIRES"}))

(def asn-value-pattern )




(def asn-special-signs 
  #{"." ".." "..." "," ";" "(" ")" "{" "}" "[" "]" "-" "<" "|" "::=" "“::=”"})

(def asn-reader-signs 
  #{#"\.\." #"\.\.\." #"," #";" #"\(" #"\)" #"\{" #"\}" #"\[" #"\]" #"<" #"\|" #"::=" })

(def asn-meaning?
  (memoize (fn [^String word] 
             (cond 
               (asn-special-signs word) :asn-special-sign
               (some (partial = word) @asn-keywords) :asn-known-keyword
               (= word (re-find #"[A-Z\-]+" word)) :asn-unknow-keyword
               (= word (re-find #"^[a-z][\w\d-]+" word)) :asn-value
               (= word (re-find #"^[A-Z][a-z0-9-]+" word)) :asn-type
               :else :asn-unknown))))


;(def asn-special-signs-pattern (frak/pattern asn-special-signs))

(defn asn-patterns
  {:identifier #"[a-z][a-zA-Z][\w\d-]+"})

(defn split-asn-elements [^String text]
  (assert (not= \" (first text)) "ASN module definition cannot start with \"")
  (letfn [(strech-special-signs [s]
            (reduce #(clojure.string/replace %1 %2 (fn [r] (str " " r " "))) s asn-reader-signs))]
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

(defrecord ASNDefinition [definition-name definition-type defition-elements])

(defn extract-macro-definitions [asn-elements]
  (loop [leftover asn-elements
         definitions []
         other-elements []]
    (if-let [next-word (fnext leftover)]
      (if (= "MACRO" next-word)
        (let [macro-definition-elements (take (inc (.indexOf leftover "END")) leftover)]
          (recur (drop (count macro-definition-elements) leftover)
                 (conj definitions (ASNDefinition. (first leftover) :MACRO macro-definition-elements))
                 other-elements))
        (recur (rest leftover)
               definitions 
               (conj other-elements (first leftover))))
      {:macros definitions
       :other-elements  other-elements})))

(defn extract-module-name [^clojure.lang.PersistentVector asn-elements]
  (let [def-position (.indexOf asn-elements "DEFINITIONS")]
    (if (not= -1 def-position)
      (get asn-elements (dec def-position)))))

(defn extract-imports-section [^clojure.lang.PersistentVector asn-elements]
  (let [imp-position (.indexOf asn-elements "IMPORTS")]
    (if (not= -1 imp-position)
      (take-while (partial not= ";") (drop imp-position asn-elements)))))

(defn extract-exports-section [^clojure.lang.PersistentVector asn-elements]
  (let [imp-position (.indexOf asn-elements "EXPORTS")]
    (if (not= -1 imp-position)
      (take-while (partial not= ";") (drop imp-position asn-elements)))))

(defn mark-type-definition-positions [^clojure.lang.PersistentVector asn-elements]
  (let [definitions (keep-indexed #(if (= "::=" %2) %1) asn-elements)]
    (map dec (filter #(= :asn-type (asn-meaning? (asn-elements (dec %1)))) definitions))))

(defn get-elements [asn-elements & idexes]
  (map #(get asn-elements %) idexes))

(defn extract-type [asn-elements position]
  (letfn [(extract-tag [[rest-of-def-seq result]] 
            (if (= "[" (first rest-of-def-seq))
              (let [tag-def (take-while #(not= "]" %) (rest rest-of-def-seq))]
                (assert (<= 2 (count tag-def)) (str "TAG " (apply str (interpose " " tag-def)) " not defined properly!"))
                (if (re-find #"\w+" (first tag-def))
                  [(drop (+ 2 (count tag-def)) rest-of-def-seq)
                   (merge result {:tag-domain (keyword (first tag-def))
                                  :tag-number (java.lang.Integer. (second tag-def))})] 
                  [(drop 3 rest-of-def-seq) 
                   (merge result {:tag-domain :UNIVERSAL
                                  :tag-number (java.lang.Integer. (first tag-def))})]))
              [[rest-of-def-seq
               (merge result {:tag-domain :UNIVERSAL
                              :tag-number nil})]]))
          (implicit? [[rest-of-def-seq result]] 
            (if (= "IMPLICIT" (first rest-of-def-seq))
              [(rest rest-of-def-seq) (assoc result :IMPLICIT? true)] 
              (if (= "EXPLICIT" (first rest-of-def-seq))
                [(rest rest-of-def-seq) (assoc result :EXPLICIT? true)] 
                [[rest-of-def-seq result]])))]
    (let [[pre-def-seq def-seq] (split-at position asn-elements)
          asn-type-name (first def-seq)
          [leftover-seq result] (-> [(drop 2 def-seq) {:asn-type asn-type-name}] 
                                    extract-tag 
                                    implicit?)]
      result)))
  
(defn test-extracted-elements [^clojure.lang.PersistentVector asn-elements]
  (let [positions (mark-type-definition-positions asn-elements)]
    (when (seq positions) 
      (map (partial extract-type asn-elements) positions))))
