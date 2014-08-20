(in-ns 'seweg.asn1.definitions.X208)

(with-monad asn-seq-track 
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
      (bind-value-for {:type :BIT_STRING
                   :class :UNIVERSAL
                   :tag 3} "BIT" "STRING")))

  (def OctetStringType
    (bind-value-for {:type :OCTET_STRING :tag 4 :class :UNIVERSAL} "OCTET" "STRING"))

  (def NullType
    (bind-value-for {:type :NULL :tag 5} "NULL"))

  (def ObjectIdentifierType
    (bind-value-for {:type :OBJECT_IDENTIFIER :tag 6} "OBJECT" "IDENTIFIER"))

  (def RealType
    (bind-value-for {:type :REAL :tag 9} "REAL"))

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
      (bind-value-for :UNIVERSAL "UNIVERSAL")
      (bind-value-for :APPLICATION "APPLICATION")
      (bind-value-for :PRIVATE "PRIVATE")
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
      (bind-value-for {:type :SEQUENCE :class :UNIVERSAL :tag 16} "SEQUENCE" "{" "}")
      (domonad [_ (asn-keywords "SEQUENCE" "{")
                elms ElementTypeList
                _ (asn-keyword "}")]
               {:type :SEQUENCE :class :UNIVERSAL :tag 16 :ElementTypeList elms})))

  (def SequenceOfType
    (choice
      (domonad [_ (asn-keywords "SEQUENCE" "OF")
                t Type]
               {:type :SEQUENCE_OF :class :UNIVERSALL :tag 16 :ofType t})
      (bind-value-for {:type :SEQUENCE_OF :class :UNIVERSALL :tag 16} "SEQUENCE")))

  (def SetType
    (choice
      (domonad [_ (asn-keywords "SET" "{")
                elms ElementTypeList
                _ (asn-keyword "}")]
               {:type :SET :class :UNIVERSAL :tag 17 :ElementTypeList elms})
      (bind-value-for {:type :SET :class :UNIVERSAL :tag 17} "SET" "{" "}")))

  (def SetOfType
    (choice
      (domonad [_ (asn-keywords "SET" "OF")
                t Type]
               {:type :SET_OF :class :UNIVERSALL :tag 17 :ofType t})
      (bind-value-for {:type :SET_OF :class :UNIVERSALL :tag 17} "SET")))

  (def AlternativeTypeList
    ElementTypeList)

  (def Choice
    (domonad [_ (asn-keywords  "CHOICE" "{")
              alt AlternativeTypeList
              _ (asn-keyword "}")]
             {:type :CHOICE :AlternativeTypeList (reduce merge alt)}))

  (def NumericString
    (bind-value-for {:tag 18 :type :NumericString :class :UNIVERSAL} "NumericString"))

  (def PrintableString
    (bind-value-for {:tag 19 :type :PrintableString :class :UNIVERSAL} "NumericString"))

  (def VisibleString 
    (bind-value-for {:tag 26 :type :VisibleString :class :UNIVERSAL} "VisibleString"))

  (def IA5String 
    (bind-value-for {:tag 22 :type :IA5String :class :UNIVERSAL} "IA5String"))


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

(with-monad asn-seq-track
  (def Subtype
    (m-result nil)) 

  (def DefinedType
    typereference)

  (def Type
    (choice
      ;Subtype
      BuiltinType 
      DefinedType)))

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

