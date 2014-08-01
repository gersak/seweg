(ns seweg.asn1.definitions.type)

(declare Type)

(defn- get-delimited-def [def-seq]
  (when (#{"{" "[" "("} (first def-seq)) 
    (let [matching-brackets {"[" "]"
                             "(" ")"
                             "{" "}"}
          first-s (first def-seq)
          end-s (get matching-brackets first-s)]
      (loop [c 1
             leftover (rest def-seq)
             seq-result [first-s]]
        (let [f (first leftover)]
         (if (or (zero? c) (empty? leftover)) seq-result
          (recur (if (= f end-s) (dec c)
                   (if (= f first-s) (inc c) c)) 
                 (rest leftover)
                 (conj seq-result (first leftover)))))))))

(defn satisfies-words? [def-seq & words]
  (= (take (count words) def-seq) words))

(defn BooleanType
  "BooleanType ::= BOOLEAN"
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "BOOLEAN") 
    [(rest def-seq) (assoc def-result :type :BOOLEAN :tag 1 :basic-type? true :class :UNIVERSAL)]
    [def-seq def-result]))

(defn IntegerType
  "IntegerType ::=
  INTEGER |
  INTEGER{NamedNumberList}
  NamedNumberList ::=
      NamedNumber |
      NamedNumberList,NamedNumber
  NamedNumber ::=
      identifier(SignedNumber) |
      identifier(DefinedValue)
  SignedNumber ::= number | -number
  " 
  [[def-seq def-result]] 
  (if (satisfies-words? def-seq "INTEGER") 
    (let [def-seq (rest def-seq)
          def-result (assoc def-result 
                            :type :INTEGER
                            :class :UNIVERSAL
                            :tag 2
                            :basic-type? true)]
      (if (= (first def-seq) "{") 
        (let [elements (remove #{"," "(" ")" "{" "}"} (get-delimited-def def-seq))
              defs (map #(list (-> % second read-string) (first %)) (partition 2 elements))]
          [(rest (drop-while (partial not= "}") def-seq))
           (assoc def-result 
                  :named-number-list (reduce #(apply assoc %1 %2) {} defs))])
        [def-seq def-result]))
    [def-seq def-result]))

(defn EnumeratedType
  "EnumeratedType ::= ENUMERATED {Enumeration}

  Enumeration ::=
  NamedNumber |
  NamedNumber, Enumeration" 
  [[def-seq def-result]] 
  (if (satisfies-words? def-seq "ENUMERATED")
    (let [def-seq (rest def-seq)
          def-result (assoc def-result 
                            :type :ENUMERATED
                            :class :UNIVERSAL
                            :tag 10
                            :basic-type? true)]
      (if (= (first def-seq) "{") 
        (let [explicit? (some #{"(" ")"} def-seq)
              delimited-seq (get-delimited-def def-seq)
              elements (remove #{"," "(" ")" "{" "}"} delimited-seq)
              defs (if explicit? 
                     (map #(list (-> % second read-string) (first %)) (partition 2 elements))
                     (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 (interleave (range (count elements)) elements))))]
          [(drop (count delimited-seq) def-seq)
           (assoc def-result 
                  :named-number-list (reduce #(apply assoc %1 %2) {} defs))])
        [def-seq def-result]))
    [def-seq def-result]))

(defn RealType
  "RealType ::= REAL"
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "REAL")
    [(rest def-seq) (assoc def-result 
                           {:type :REAL
                            :class :UNIVERSAL
                            :tag 9
                            :basic-type? true})]
    [def-seq def-result]))

(defn BitStringType
  "BitStringType ::=
          BIT STRING |
          BIT STRING{NamedBitList}
  NamedBitList ::=
          NamedBit |
          NamedBitList,NamedBit
  NamedBit ::=
          identifier(number) |
          identifier(DefinedValue)
  "
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "BIT" "STRING")
    (let [def-seq (drop 2 def-seq)
          def-result (assoc def-result 
                            :type :BIT_STRING
                            :class :UNIVERSAL
                            :tag 3
                            :basic-type? true)]
      (if (= (first def-seq) "{") 
        (let [delimited-seq (get-delimited-def def-seq)]
          (if (some #{"(" ")"} delimited-seq)
            (let [elements (remove #{"," "(" ")" "{" "}"} delimited-seq)
                  defs (map #(list (-> % second read-string) (first %)) (partition 2 elements))]
              [(drop (count delimited-seq) def-seq)
               (assoc def-result 
                      :named-bit-list (reduce #(apply assoc %1 %2) {} defs))])
            [(drop (count delimited-seq) def-seq) def-result]))
        [def-seq def-result]))
    [def-seq def-result]))


(defn OctetStringType
  "OctetStringType ::= OCTET STRING"
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "OCTET" "STRING")
    (let [def-seq (drop 2 def-seq)
          def-result (assoc def-result 
                            :type :OCTET_STRING
                            :class :UNIVERSAL
                            :tag 4
                            :basic-type? true)]
      [def-seq def-result])
    [def-seq def-result]))

(defn NullType
  "NullType ::= NULL"
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "NULL")
    (let [def-seq (rest def-seq) 
          def-result (assoc def-result 
                            :type :OCTET_STRING
                            :class :UNIVERSAL
                            :tag 4
                            :basic-type? true)]
      [def-seq def-result])
    [def-seq def-result]))

(defn SequenceType
  "SequenceType ::=
      SEQUENCE{ElementTypeList} |
      SEQUENCE{ }
  ElementTypeList ::=
      ElementType |
      ElementTypeList,ElementType
  ElementType ::=
      NamedType |
      NamedType OPTIONAL |
      NamedType DEFAULT Value"
  [[def-seq def-result]]
  (if (and (satisfies-words? def-seq "SEQUENCE") (not= "OF" (second def-seq))) 
    (let [def-seq (rest def-seq)
          def-result (assoc def-result 
                            :type :SEQUENCE
                            :class :UNIVERSAL
                            :tag 16
                            :basic-type? false)]
      (if (= (first def-seq) "{")
        (let [delimited-seq (get-delimited-def def-seq)
              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
          [(drop (count delimited-seq) def-seq)
           (assoc def-result 
                  :element-type-list elements-defs)])
        [def-seq def-result]))
    [def-seq def-result]))

(defn SequenceOfType
  "SEQUENCE OF Type | SEQUENCE" 
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "SEQUENCE" "OF")
    (let [def-seq (drop 2 def-seq)
          def-result (assoc def-result
                            :type :SEQUENCE_OF
                            :class :UNIVERSAL
                            :tag 16
                            :basic-type? false)]
      [(drop 1 def-seq) (assoc def-result
                               :sequence-type (read-string (first def-seq)))])
    [def-seq def-result]))

(defn SetType 
  "SetType ::=
      SET{ElementTypeList} |
      SET{ }"
  [[def-seq def-result]]
  (if (and (satisfies-words? def-seq "SET") (not= "OF" (second def-seq))) 
    (let [def-seq (rest def-seq)
          def-result (assoc def-result 
                            :type :SET
                            :class :UNIVERSAL
                            :tag 17
                            :basic-type? false)]
      (if (= (first def-seq) "{")
        (let [delimited-seq (get-delimited-def def-seq)
              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
          [(drop (count delimited-seq) def-seq)
           (assoc def-result 
                  :element-type-list elements-defs)])
        [def-seq def-result]))
    [def-seq def-result]))

(defn SetOfType
  "SET OF Type | SET" 
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "SET" "OF")
    (let [def-seq (drop 2 def-seq)
          def-result (assoc def-result
                            :type :SET_OF
                            :class :UNIVERSAL
                            :tag 17
                            :basic-type? false)]
      [(drop 1 def-seq) (assoc def-result :set-type (read-string (first def-seq)))])
    [def-seq def-result]))


(defn ChoiceType
  "ChoiceType ::= CHOICE{AlternativeTypeList}

  AlternativeTypeList ::=
  NamedType |
  AlternativeTypeList,NamedType
  "
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "CHOICE")
    (let [def-seq (drop 1 def-seq)
          def-result (assoc def-result
                            :type :CHOICE
                            :class :UNIVERSAL
                            :basic-type? false)]
      (if (= (first def-seq) "{")
        (let [delimited-seq (get-delimited-def def-seq)
              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
          [(drop (count delimited-seq) def-seq)
           (assoc def-result 
                  :choices elements-defs)])
        [def-seq def-result]))
    [def-seq def-result]))

(defn TaggedType 
  "
  TaggedType ::=
          Tag Type |
          Tag IMPLICIT Type |
          Tag EXPLICIT Type

  Tag ::= [Class ClassNumber]
  
  ClassNumber ::=
          number |
          DefinedValue

  Class ::=
          UNIVERSAL |
          APPLICATION |
          PRIVATE
  "
  [[def-seq def-result]]
  (if (= (first def-seq) "[")
    (let [delimited-seq (get-delimited-def def-seq)
          def-seq (drop (count delimited-seq) def-seq)
          [def-seq tag-type] (if-let [tag-type (#{"IMPLICIT" "EXPLICIT"} (first def-seq))]
                               [(rest def-seq) tag-type]
                               [def-seq "IMPLICIT"])
          [ClassNumber Class] (reverse delimited-seq) 
          tag-part (reduce conj {} 
                           (remove (comp nil? val) {:tag ClassNumber :class (if Class (keyword Class) :CONTEXT-SPECIFIC)}))
          [def-seq def-result] (Type [def-seq def-result])]
      [def-seq (merge def-result tag-part)])
    [def-seq def-result]))

(defn ObjectIdentifierType
  "ObjectIdentifierType ::= OBJECT IDENTIFIER"
  [[def-seq def-result]]
  (if (satisfies-words? def-seq "OBJECT" "IDENTIFIER")
    (let [def-seq (drop 2 def-seq)
          def-result (assoc def-result
                            :type :OBJECT_IDENTIFIER
                            :class :UNIVERSAL
                            :tag 6
                            :basic-type? true)]
      [(drop 2 def-seq) def-seq])
    [def-seq def-result]))

(defn CharacterStringType
  "CharacterStringType ::=
              NumericString |
              PrintableString |
              TeletexString |
              VideotexString |
              VisibleString |
              IA5String |
              GraphicString |
              GeneralString "
  [[def-seq def-result]]
  (let [string-types ["NumericString" "PrintableString" "TeletexString" "VideotexString"
                      "VisibleString" "IA5String" "GraphicString" "GeneralString"]
        tag-map {"NumericString" 18
                 "PrintableString" 19
                 "TeletexString" 20
                 "VideotexString" 21
                 "VisibleString" 26
                 "IA5String" 22
                 "GraphicString" 25
                 "GeneralString" 27}]
    (if-let [tag (some #{(first def-seq)} string-types)] 
      [(rest def-seq) (assoc def-result :type (keyword tag) :tag (get tag-map tag) :class :UNIVERSAL)]
      [def-seq def-result])))


(defn ValueSet
  "Subtype ValueSet ::=
              SingleValue |
              ContainedSubtype |
              ValueRange |
              PermittedAlphabet | SizeConstraint | InnerTypeConstraints"
  [[def-seq def-result]])

(defn Subtype
  "Subtype ::=
            ParentType SubtypeSpec |
            SET SizeConstraint OF Type |
            SEQUENCE SizeConstraint OF Type

  ParentType ::= Type
  
  SubtypeSpec ::=
            (SubtypeValueSet SubtypeValueSetList)

  SubtypeList ::=
            “ | ”
            SubtypeValueSet
            SubtypeValueSetList |
            empty"
  [[def-seq def-result]]
  (when-let [constrain-def (get-delimited-def def-seq)]
   constrain-def))

  
(defn BuiltinType
  "BuiltinType ::=

          BooleanType |
          IntegerType |
          BitStringType |
          OctetStringType |
          NullType |
          SequenceType |
          SequenceOfType |
          SetType |
          SetOfType |
          ChoiceType |
          SelectionType |
          TaggedType |
          AnyType |
          ObjectIdentifierType |
          CharacterStringType |
          UsefulType |
          EnumeratedType |
          RealType"
  [def-seq def-result])

(defn Type 
  "Type ::= BuiltinType | DefinedType | Subtype"
  [def-seq def-result])
