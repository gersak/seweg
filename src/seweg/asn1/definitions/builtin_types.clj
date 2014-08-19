(in-ns 'seweg.asn1.definitions.X208)

(with-monad asn-seq-track 
  (defn bind-value [value]
    (fn [_]
      (m-result value)))

  (defn plain-type [asn-type & words]
    (m-bind (apply asn-keywords words) (bind-value asn-type)))

  (def BooleanType 
    (domonad [_ (asn-keyword "BOOLEAN")]
             {:type :BOOLEAN :tag 1}))

  (def NamedNumber
    (domonad [id identifier 
              _ (asn-keyword "(")
              v (choice number DefinedValue)
              _ (asn-keyword ")")]
             {v id}))
  
  (def NamedNumberList
    (while-m-object 
      (choice 
        (domonad [nn NamedNumber
                  _ asn-comma]
                 nn)
        NamedNumber)))
  
  (def IntegerType
    (choice
      (domonad [_ (asn-keywords "INTEGER" "{")
                nl NamedNumberList
                _ (asn-keyword "}")]
               {:type :INTEGER
                :tag 2
                :class :UNIVERSAL
                :NamedNumberList (reduce into {} nl)})
      (domonad [_ (asn-keyword "INTEGER")]
               {:type :INTEGER :tag 2 :class :UNIVERSAL})))

  (def NamedBit
    (domonad [id identifier 
              _ (asn-keyword "(")
              v (choice number DefinedValue)
              _ (asn-keyword ")")]
             {v id}))

  (def NamedBitList
    (while-m-object 
      (choice 
        (domonad [nn NamedBit
                  _ asn-comma]
                 nn)
        NamedBit)))

  (def BitStringType
    (choice 
      (domonad [_ (asn-keywords "BIT" "STRING" "{")
                nbl NamedBitList
                _ (asn-keyword "}")]
               {:type :BIT_STRING
                :class :UNIVERSAL
                :tag 3
                :NamedBitList (reduce into {} nbl)})
      (plain-type {:type :BIT_STRING
                   :class :UNIVERSAL
                   :tag 3} "BIT" "STRING")))

  (def OctetStringType
    (plain-type {:type :OCTET_STRING :tag 4 :class :UNIVERSAL} "OCTET" "STRING"))

  (def NullType
    (plain-type {:type :NULL :tag 5} "NULL"))

  (def ObjectIdentifierType
    (plain-type {:type :OBJECT_IDENTIFIER :tag 6} "OBJECT" "IDENTIFIER"))

  (def RealType
    (plain-type {:type :REAL :tag 9} "REAL"))

  (def CharacterStringType typereference)

  (def UsefulType typereference)

  (def Enumeration
    NamedNumberList)

  (def EnumeratedType
    (domonad [_ (asn-keyword "ENUMERATED")
              en Enumeration]
             {:type :ENUMERATED
              :class :UNIVERSAL
              :tag 10
              :Enumeration en}))

  (def NamedType
    (choice
      (domonad [id identifier
                t Type]
               {id t})
      ;; TODO: Selection Type not implemented jet 
      Type))


  (def ElementType
    (choice
      (domonad [elt NamedType
                _ (asn-keyword "OPTIONAL")]
               elt)
      (domonad [elt NamedType
                _ (asn-keyword "DEFAULT")
                v Value]
               elt)
      (domonad [_ (asn-keywords "COMPONENTS" "OF")
                elt NamedType]
               elt)
      NamedType))

  (def ElementTypeList
   (while-m-object
     (choice
       (domonad [el ElementType
                 _ asn-comma]
                el)
       ElementType)))

  

  (def ClassNumber
    (choice number DefinedValue))

  (def TagClass
    (choice 
      (plain-type :UNIVERSAL "UNIVERSAL")
      (plain-type :APPLICATION "APPLICATION")
      (plain-type :PRIVATE "PRIVATE")
      (m-result nil)))

  (def Tag
    (choice
      (domonad [_ (asn-keyword "[")
                c TagClass
                cn ClassNumber
                _ (asn-keyword "]")]
               {:tag cn :class c})))

  (def TaggedType
    (choice
      (domonad [tag Tag
                _ (asn-keyword "IMPLICIT")
                t Type]
               (merge t  tag {:tag-type :IMPLICIT}))
      (domonad [tag Tag
                _ (asn-keyword "EXPLICIT")
                t Type]
               (merge t tag {:tag-type :EXPLICIT}))
      (domonad [tag Tag
                t Type]
               (merge t tag))))


  (def SequenceType
    (choice
      (plain-type {:type :SEQUENCE :class :UNIVERSAL :tag 16} "SEQUENCE" "{" "}")
      (domonad [_ (asn-keywords "SEQUENCE" "{")
                elms ElementTypeList
                _ (asn-keyword "}")]
               {:type :SEQUENCE :class :UNIVERSAL :tag 16 :ElementTypeList elms})))

  (def SequenceOfType
    (choice
      (domonad [_ (asn-keywords "SEQUENCE" "OF")
                t Type]
               {:type :SEQUENCE_OF :class :UNIVERSALL :tag 16 :ofType t})
      (plain-type {:type :SEQUENCE_OF :class :UNIVERSALL :tag 16} "SEQUENCE")))

  (def SetType
    (choice
      (domonad [_ (asn-keywords "SET" "{")
                elms ElementTypeList
                _ (asn-keyword "}")]
               {:type :SET :class :UNIVERSAL :tag 17 :ElementTypeList elms})
      (plain-type {:type :SET :class :UNIVERSAL :tag 17} "SET" "{" "}")))

  (def SetOfType
    (choice
      (domonad [_ (asn-keywords "SET" "OF")
                t Type]
               {:type :SET_OF :class :UNIVERSALL :tag 17 :ofType t})
      (plain-type {:type :SET_OF :class :UNIVERSALL :tag 17} "SET")))

  (def AlternativeTypeList
    ElementTypeList)

  (def Choice
    (domonad [_ (asn-keywords  "CHOICE" "{")
              alt AlternativeTypeList
              _ (asn-keyword "}")]
             {:type :CHOICE :AlternativeTypeList (reduce merge alt)}))

  (def NumericString
    (plain-type {:tag 18 :type :NumericString :class :UNIVERSAL} "NumericString"))

  (def PrintableString
    (plain-type {:tag 19 :type :PrintableString :class :UNIVERSAL} "NumericString"))

  (def VisibleString 
    (plain-type {:tag 26 :type :VisibleString :class :UNIVERSAL} "VisibleString"))

  (def IA5String 
    (plain-type {:tag 22 :type :IA5String :class :UNIVERSAL} "IA5String"))


  (def BuiltinType
    (choice
      BooleanType
      IntegerType
      BitStringType
      OctetStringType
      NullType
      ObjectIdentifierType
      EnumeratedType
      CharacterStringType
      RealType
      TaggedType
      SequenceType
      SequenceOfType
      SetType
      SetOfType
      Choice
      IA5String
      NumericString
      PrintableString
      VisibleString)))

(def module-test-seq (-> "mibs/ietf/IP-FORWARD-MIB" slurp split-asn-elements))


(def Choice-test
  (split-asn-elements
    "    CHOICE {
    ipAddress-value
    IpAddress,

    counter-value
    Counter32,

    timeticks-value
    TimeTicks,

    arbitrary-value
    Opaque,

    big-counter-value
    Counter64,

    unsigned-integer-value  -- includes Gauge32
    Unsigned32
    }
    "))


(def Tag-test-seq
  (split-asn-elements
    "IpAddress ::=
        [APPLICATION 0]
            IMPLICIT OCTET STRING (SIZE (4))"))

(def seq-test-seq 
  (split-asn-elements
    "SEQUENCE {
    inetCidrRouteDestType     InetAddressType,
    inetCidrRouteDest         InetAddress,
    inetCidrRoutePfxLen       InetAddressPrefixLength,
    inetCidrRoutePolicy       OBJECT IDENTIFIER,
    inetCidrRouteNextHopType  InetAddressType,
    inetCidrRouteNextHop      InetAddress,
    inetCidrRouteIfIndex      InterfaceIndexOrZero,
    inetCidrRouteType         INTEGER,
    inetCidrRouteProto        IANAipRouteProtocol,
    inetCidrRouteAge          Gauge32,
    inetCidrRouteNextHopAS    InetAutonomousSystemNumber,
    inetCidrRouteMetric1      Integer32,
    inetCidrRouteMetric2      Integer32,
    inetCidrRouteMetric3      Integer32,



    inetCidrRouteMetric4      Integer32,
    inetCidrRouteMetric5      Integer32,
    inetCidrRouteStatus       RowStatus
    }"))




;(defn satisfies-words? [asn-seq & words]
;  (= (take (count words) asn-seq) words))
;
;(defn BooleanType
;  "BooleanType ::= BOOLEAN"
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "BOOLEAN") 
;    [(rest asn-seq) (assoc result :type :BOOLEAN :tag 1 :basic-type? true :class :UNIVERSAL)]
;    [result asn-seq]))
;
;(defn IntegerType
;  "IntegerType ::=
;  INTEGER |
;  INTEGER{NamedNumberList}
;  NamedNumberList ::=
;      NamedNumber |
;      NamedNumberList,NamedNumber
;  NamedNumber ::=
;      identifier(SignedNumber) |
;      identifier(DefinedValue)
;  SignedNumber ::= number | -number
;  " 
;  [[result asn-seq]] 
;  (if (satisfies-words? asn-seq "INTEGER") 
;    (let [asn-seq (rest asn-seq)
;          result (assoc result 
;                            :type :INTEGER
;                            :class :UNIVERSAL
;                            :tag 2
;                            :basic-type? true)]
;      (if (= (first asn-seq) "{") 
;        (let [elements (remove #{"," "(" ")" "{" "}"} (get-delimited-def asn-seq))
;              defs (map #(list (-> % second read-string) (first %)) (partition 2 elements))]
;          [(rest (drop-while (partial not= "}") asn-seq))
;           (assoc result 
;                  :named-number-list (reduce #(apply assoc %1 %2) {} defs))])
;        [result asn-seq]))
;    [result asn-seq]))
;
;(defn EnumeratedType
;  "EnumeratedType ::= ENUMERATED {Enumeration}
;
;  Enumeration ::=
;  NamedNumber |
;  NamedNumber, Enumeration" 
;  [[result asn-seq]] 
;  (if (satisfies-words? asn-seq "ENUMERATED")
;    (let [asn-seq (rest asn-seq)
;          result (assoc result 
;                            :type :ENUMERATED
;                            :class :UNIVERSAL
;                            :tag 10
;                            :basic-type? true)]
;      (if (= (first asn-seq) "{") 
;        (let [explicit? (some #{"(" ")"} asn-seq)
;              delimited-seq (get-delimited-def asn-seq)
;              elements (remove #{"," "(" ")" "{" "}"} delimited-seq)
;              defs (if explicit? 
;                     (map #(list (-> % second read-string) (first %)) (partition 2 elements))
;                     (reduce #(assoc %1 (first %2) (second %2)) {} (partition 2 (interleave (range (count elements)) elements))))]
;          [(drop (count delimited-seq) asn-seq)
;           (assoc result 
;                  :named-number-list (reduce #(apply assoc %1 %2) {} defs))])
;        [result asn-seq]))
;    [result asn-seq]))
;
;(defn RealType
;  "RealType ::= REAL"
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "REAL")
;    [(rest asn-seq) (assoc result 
;                           {:type :REAL
;                            :class :UNIVERSAL
;                            :tag 9
;                            :basic-type? true})]
;    [result asn-seq]))
;
;(defn BitStringType
;  "BitStringType ::=
;          BIT STRING |
;          BIT STRING{NamedBitList}
;  NamedBitList ::=
;          NamedBit |
;          NamedBitList,NamedBit
;  NamedBit ::=
;          identifier(number) |
;          identifier(DefinedValue)
;  "
;  [[result asn-seq]]
;  (if-let [{ros :_rest-of-seq} (unifyk '("BIT" "STRING" & :_rest-of-seq) asn-seq)]
;    (if-let [delimited-seq (get-delimited-def ros)]
;      [{:type :BIT_STRING 
;        :class :UNIVERSAL
;        :tag 3
;        :basic-type? true
;        :NamedBitList (reduce conj {} 
;                              (for [x (partition 2 (remove #{"{" "}" "," "(" ")"} delimited-seq)) :let [identifier (keyword (first x))
;                                                                                                        value (read-string (second x))]]
;                                (if (number? value)
;                                  [value identifier]
;                                  [(first (DefinedValue [nil (rest x)])) identifier])))}
;       (drop (count delimited-seq) ros)]
;      [{:type :BIT_STRING 
;        :class :UNIVERSAL
;        :tag 3
;        :basic-type? true} ros])
;    [result asn-seq]))
;
;
;(def bit-string-test (split-asn-elements "BIT STRING { a(2) , b(fucker) }"))
;
;
;(defn OctetStringType
;  "OctetStringType ::= OCTET STRING"
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "OCTET" "STRING")
;    (let [asn-seq (drop 2 asn-seq)
;          result (assoc result 
;                        :type :OCTET_STRING
;                        :class :UNIVERSAL
;                        :tag 4
;                        :basic-type? true)]
;      [result asn-seq])
;    [result asn-seq]))
;
;(defn NullType
;  "NullType ::= NULL"
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "NULL")
;    (let [asn-seq (rest asn-seq) 
;          result (assoc result 
;                            :type :OCTET_STRING
;                            :class :UNIVERSAL
;                            :tag 4
;                            :basic-type? true)]
;      [result asn-seq])
;    [result asn-seq]))
;
;(defn SequenceType
;  "SequenceType ::=
;      SEQUENCE{ElementTypeList} |
;      SEQUENCE{ }
;  ElementTypeList ::=
;      ElementType |
;      ElementTypeList,ElementType
;  ElementType ::=
;      NamedType |
;      NamedType OPTIONAL |
;      NamedType DEFAULT Value"
;  [[result asn-seq]]
;  (if (and (satisfies-words? asn-seq "SEQUENCE") (not= "OF" (second asn-seq))) 
;    (let [asn-seq (rest asn-seq)
;          result (assoc result 
;                            :type :SEQUENCE
;                            :class :UNIVERSAL
;                            :tag 16
;                            :basic-type? false)]
;      (if (= (first asn-seq) "{")
;        (let [delimited-seq (get-delimited-def asn-seq)
;              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
;          [(drop (count delimited-seq) asn-seq)
;           (assoc result 
;                  :element-type-list elements-defs)])
;        [result asn-seq]))
;    [result asn-seq]))
;
;(defn SequenceOfType
;  "SEQUENCE OF Type | SEQUENCE" 
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "SEQUENCE" "OF")
;    (let [asn-seq (drop 2 asn-seq)
;          result (assoc result
;                            :type :SEQUENCE_OF
;                            :class :UNIVERSAL
;                            :tag 16
;                            :basic-type? false)]
;      [(drop 1 asn-seq) (assoc result
;                               :sequence-type (read-string (first asn-seq)))])
;    [result asn-seq]))
;
;(defn SetType 
;  "SetType ::=
;      SET{ElementTypeList} |
;      SET{ }"
;  [[result asn-seq]]
;  (if (and (satisfies-words? asn-seq "SET") (not= "OF" (second asn-seq))) 
;    (let [asn-seq (rest asn-seq)
;          result (assoc result 
;                            :type :SET
;                            :class :UNIVERSAL
;                            :tag 17
;                            :basic-type? false)]
;      (if (= (first asn-seq) "{")
;        (let [delimited-seq (get-delimited-def asn-seq)
;              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
;          [(drop (count delimited-seq) asn-seq)
;           (assoc result 
;                  :element-type-list elements-defs)])
;        [result asn-seq]))
;    [result asn-seq]))
;
;(defn SetOfType
;  "SET OF Type | SET" 
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "SET" "OF")
;    (let [asn-seq (drop 2 asn-seq)
;          result (assoc result
;                            :type :SET_OF
;                            :class :UNIVERSAL
;                            :tag 17
;                            :basic-type? false)]
;      [(drop 1 asn-seq) (assoc result :set-type (read-string (first asn-seq)))])
;    [result asn-seq]))
;
;
;(defn ChoiceType
;  "ChoiceType ::= CHOICE{AlternativeTypeList}
;
;  AlternativeTypeList ::=
;  NamedType |
;  AlternativeTypeList,NamedType
;  "
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "CHOICE")
;    (let [asn-seq (drop 1 asn-seq)
;          result (assoc result
;                            :type :CHOICE
;                            :class :UNIVERSAL
;                            :basic-type? false)]
;      (if (= (first asn-seq) "{")
;        (let [delimited-seq (get-delimited-def asn-seq)
;              elements-defs (remove #{'(",")} (partition-by (partial not= ",") (butlast (rest delimited-seq))))]
;          [(drop (count delimited-seq) asn-seq)
;           (assoc result 
;                  :choices elements-defs)])
;        [result asn-seq]))
;    [result asn-seq]))
;
;(defn TaggedType 
;  "
;  TaggedType ::=
;          Tag Type |
;          Tag IMPLICIT Type |
;          Tag EXPLICIT Type
;
;  Tag ::= [Class ClassNumber]
;  
;  ClassNumber ::=
;          number |
;          DefinedValue
;
;  Class ::=
;          UNIVERSAL |
;          APPLICATION |
;          PRIVATE
;  "
;  [[result asn-seq]]
;  (if (= (first asn-seq) "[")
;    (let [delimited-seq (get-delimited-def asn-seq)
;          asn-seq (drop (count delimited-seq) asn-seq)
;          [asn-seq tag-type] (if-let [tag-type (#{"IMPLICIT" "EXPLICIT"} (first asn-seq))]
;                               [(rest asn-seq) tag-type]
;                               [asn-seq "IMPLICIT"])
;          [ClassNumber Class] (reverse delimited-seq) 
;          tag-part (reduce conj {} 
;                           (remove (comp nil? val) {:tag ClassNumber :class (if Class (keyword Class) :CONTEXT-SPECIFIC)}))
;          [result asn-seq] (Type [asn-seq result])]
;      [asn-seq (merge result tag-part)])
;    [result asn-seq]))
;
;(defn ObjectIdentifierType
;  "ObjectIdentifierType ::= OBJECT IDENTIFIER"
;  [[result asn-seq]]
;  (if (satisfies-words? asn-seq "OBJECT" "IDENTIFIER")
;    (let [asn-seq (drop 2 asn-seq)
;          result (assoc result
;                            :type :OBJECT_IDENTIFIER
;                            :class :UNIVERSAL
;                            :tag 6
;                            :basic-type? true)]
;      [(drop 2 asn-seq) asn-seq])
;    [result asn-seq]))
;
;(defn CharacterStringType
;  "CharacterStringType ::=
;              NumericString |
;              PrintableString |
;              TeletexString |
;              VideotexString |
;              VisibleString |
;              IA5String |
;              GraphicString |
;              GeneralString "
;  [[result asn-seq]]
;  (let [string-types ["NumericString" "PrintableString" "TeletexString" "VideotexString"
;                      "VisibleString" "IA5String" "GraphicString" "GeneralString"]
;        tag-map {"NumericString" 18
;                 "PrintableString" 19
;                 "TeletexString" 20
;                 "VideotexString" 21
;                 "VisibleString" 26
;                 "IA5String" 22
;                 "GraphicString" 25
;                 "GeneralString" 27}]
;    (if-let [tag (some #{(first asn-seq)} string-types)] 
;      [(rest asn-seq) (assoc result :type (keyword tag) :tag (get tag-map tag) :class :UNIVERSAL)]
;      [result asn-seq])))


;(defn BuiltinType
;  "BuiltinType ::=
;
;          BooleanType |
;          IntegerType |
;          BitStringType |
;          OctetStringType |
;          NullType |
;          SequenceType |
;          SequenceOfType |
;          SetType |
;          SetOfType |
;          ChoiceType |
;          SelectionType |
;          TaggedType |
;          AnyType |
;          ObjectIdentifierType |
;          CharacterStringType |
;          UsefulType |
;          EnumeratedType |
;          RealType"
;  [[result asn-seq]])
