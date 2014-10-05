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
               [id t])
      ;; TODO: Selection Type not implemented jet 
      #'Type))


  (def ElementType
    (choice
      (domonad [elt NamedType
                _ (asn-keyword "OPTIONAL")]
               (if (vector? elt)
                 (let [[id t] elt]
                   {id (with-meta t {:OPTIONAL? true})})
                 (with-meta elt {:OPTIONAL? true})))

      (domonad [elt NamedType
                _ (asn-keyword "DEFAULT")
                v Value]
               (if (vector? elt)
                 (let [[id t] elt]
                   {id (with-meta t {:DEFAULT v})})
                 (with-meta elt {:DEFAULT v})))

      (domonad [_ (asn-keywords "COMPONENTS" "OF")
                elt NamedType]
               elt)

      (domonad [result NamedType]
               (if (vector? result)
                 (apply hash-map result)
                 result))))

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
               (if c
                 {:tag cn :class c}
                 {:tag cn}))))

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
             {:type :CHOICE :AlternativeTypeList (reduce conj alt)}))

  (def NumericString
    (bind-value-for {:tag 18 :type :NumericString :class :UNIVERSAL} "NumericString"))

  (def PrintableString
    (bind-value-for {:tag 19 :type :PrintableString :class :UNIVERSAL} "PrintableString"))

  (def VisibleString 
    (bind-value-for {:tag 26 :type :VisibleString :class :UNIVERSAL} "VisibleString"))

  (def IA5String 
    (bind-value-for {:tag 22 :type :IA5String :class :UNIVERSAL} "IA5String"))


  (def BuiltinTypeFunctions
    [#'BooleanType
     #'IntegerType
     #'BitStringType
     #'OctetStringType
     #'NullType
     #'ObjectIdentifierType
     #'EnumeratedType
     #'RealType
     #'TaggedType
     #'SequenceType
     #'SequenceOfType
     #'SetType
     #'SetOfType
     #'Choice
     #'IA5String
     #'NumericString
     #'PrintableString
     #'VisibleString
     #'CharacterStringType])

  (defn resolveBuiltinType [data]
    (some #(when-let [r (% data)]
             (println (apply str (repeat 20 \-)) " Resolved by: " %)
             r) BuiltinTypeFunctions))

  (def BuiltinType
    (apply choice BuiltinTypeFunctions)))


;; SUBTYPE

(declare SubtypeSpec)

(with-monad asn-seq-track
  ;(def ParentType Type)
  

  (def SingleValue 
    (domonad [svc (while-m-object
                    (choice
                      (domonad [v Value
                                _ (asn-keyword "|")]
                               v)
                      Value))]
             {:SINGLE_VALUE (set svc)}))

  (def ContainedSubtype
    (domonad [_ (asn-keyword "INCLUDES")
              t Type]
             {:INCLUDES t}))


  (def LowerEndPoint 
    (choice
      Value
      (bind-value-for "MIN" "MIN")))

  (def UpperEndPoint
    (choice Value
            (bind-value-for "MAX" "MAX")))

  (def ValueRange
    (domonad [l LowerEndPoint
              _ (asn-keyword "..")
              h UpperEndPoint]
             {:RANGE [l h]}))

  (def SizeConstraint
    (domonad [_ (asn-keyword "SIZE")
              spc SubtypeSpec]
             {:SIZE spc}))

  (def NumberSetConstraint
    (domonad 
      [r (while-m-object
           (choice
             (domonad [n number
                       _ (asn-keyword "|")]
                      n)
             number))]
      {:SET (set r)}))

  (def PermittedAlphabet
    (domonad [_ (asn-keyword "FROM")
              spec SubtypeSpec]
             {:ALPHABET_FROM spec}))


  (def SubtypeValueSet
    (choice
      ContainedSubtype
      ValueRange
      PermittedAlphabet
      SizeConstraint
      SingleValue))

  (def SubtypeValueSetList
    (while-m-object
      (choice
        (domonad [v SubtypeValueSet
                  _ (asn-keyword "|")]
                 v)
        SubtypeValueSet)))

  (def SubtypeSpec 
    (domonad [_ (asn-keyword "(")
              ss SubtypeValueSetList 
              _ (asn-keyword ")")]
             (reduce merge ss)))
  
  (def DefinedType
    typereference)

  (def ParentType
    (choice BuiltinType DefinedType))

  (def Subtype
    (choice
      (domonad [_ (asn-keyword "SET")
                size-constraint SizeConstraint
                _ (asn-keyword "OF")
                t Type]
               {:type :SET_OF :class 
                :UNIVERSALL :tag 17
                :SIZE size-constraint
                :ofType t})
      (domonad [_ (asn-keyword "SEQUENCE")
                size-constraint SizeConstraint
                _ (asn-keyword "OF")
                t Type]
               {:type :SEQUENCE_OF 
                :class :UNIVERSALL 
                :SIZE size-constraint
                :tag 16 :ofType t})
      (domonad [pt ParentType
                spec SubtypeSpec]
               (reduce merge pt spec))))
  

  (def Type
    (choice
      Subtype
      BuiltinType 
      DefinedType)))

(def module-test-seq (-> "mibs/ietf/IP-FORWARD-MIB" slurp split-asn-elements))


(comment 
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


  (def subtype-test-seq
    "SEQUENCE {
    name PrintableString (SIZE (1..40)),
    street PrintableString (SIZE (1..50)) OPTIONAL,
    postcode NumericString (SIZE (10)),
    town PrintableString (SIZE (1..30)),
    country PrintableString (SIZE (1..20))
    DEFAULT default-country }")

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


  (def hard-core-test
    (split-asn-elements
      "[APPLICATION 0] IMPLICIT SET
      { Name,
      title [0] VisibleString,
      number EmployeeNumber,
      dateOfHire [1] Date,
      nameOfSpouse [2] Name,
      children [3] IMPLICIT
      SEQUENCE OF
      ChildInformation
      DEFAULT { }}"))

  (def hard-coretest2
    (split-asn-elements
      "[APPLICATION 1] IMPLICIT SEQUENCE
      {givenName VisibleString,
      initial VisibleString,
      familyName VisibleString}
      EmployeeNumber ::= [APPLICATION 2] IMPLICIT")))
