(in-ns 'seweg.asn1.definitions.X208)


(with-monad asn-seq-track

  (def Externalmacroreference 
    (domonad [mr (get-one)
              _ (asn-keyword ".")
              macreference macroreference]
             (ASNExternalMacroReference. mr macreference)))

  (def productionreference
    (domonad [p identifier
              :when (= :typereference (asn-meaning? p))]
             (ASNProductionReference. p)))


  (def localtypereference
    (domonad [p identifier
              :when (= :typereference (asn-meaning? p))]
             (ASNLocalTypeReference. p)))

  (def localvaluereference
    (domonad [p identifier
              :when (= :valuereference (asn-meaning? (name p))) ]
             (ASNLocalValueReference. p)))

  (def MacroType
    (choice localtypereference Type))

  (def MacroValue
    (choice Value localvaluereference))

  (def LocalTypeAssignement
    (domonad [ltr localtypereference 
              _ (asn-keyword "::=")
              mt MacroType]
             [ltr mt]))
             ;{ltr mt}))

  (def LocalValueAssignement
    (domonad [lvr LocalValueAssignement
              mt MacroType
              _ (asn-keyword "::=")
              mv MacroValue]
             [lvr mv]))
             ;{lvr mv}))

  (def MEmbeddedDefinition
    (choice
      LocalTypeAssignement
      LocalValueAssignement))

  (def MEmbeddedDefinitionsList
    (while-m-object MEmbeddedDefinition))

  (def MEmbeddedDefinitions
    (domonad [_ (asn-keyword "<")
              edl MEmbeddedDefinitionsList
              _ (asn-keyword ">")]
             edl))

  (def productionreference 
    (domonad [w (get-one)
              :when (#{:typereference} (asn-meaning? w))]
             (ASNProductionReference. w)))

  (def astring
    (domonad [a (get-one)
              :when (every? #{\"} [(first a) (last a)])]
             (.substring a 1 (dec (count a)))))

  (def mtype Type)

  (def resolveSymbolDef)

  (def MSymbolDef
    (choice
      astring
      (bind-value-for 'identifier "identifier")
      (bind-value-for 'string "string")
      (bind-value-for 'empty "empty")
      (bind-value-for 'number "number")
      (asn-when-not-def productionreference)
      (asn-when-not-def mtype)
      (bind-value-for 'type "type")
      (domonad [_ (asn-keywords "type" "(")
                lt localtypereference
                _ (asn-keyword ")")]
               lt)
      (domonad [_ (asn-keywords "value" "(")
                mt MacroType
                _ (asn-keyword ")")]
               mt)
      (domonad [_ (asn-keywords "value" "(")
                lr localtypereference
                mt MacroType
                _ (asn-keyword ")")]
               [lr mt])
      (domonad [_ (asn-keywords "value" "(" "VALUE")
                ofType MacroType
                _ (asn-keywords ")")]
               ofType)))

  (def MSymbolElement
    (choice MSymbolDef MEmbeddedDefinitions))

  (def MSymbolList
    (while-m-object MSymbolElement))

  (def MacroAlternative MSymbolList)

  (def MacroAlternativeList 
    (while-m-object
      (choice
        (domonad [ma MacroAlternative
                  _ (asn-keyword "|")]
                 ma)
        MacroAlternative)))

  (def Production
    (domonad [reference productionreference
              _ (asn-keyword "::=")
              mal MacroAlternativeList]
             {reference mal}))

  (def ProductionList
    (while-m-object Production))

  (def SupportingProductions
    (choice
      ProductionList
      (m-result 'empty)))

  (def ValueProduction
    (domonad [_ (asn-keywords "VALUE" "NOTATION" "::=")
              mal MacroAlternativeList]
            mal))

  (def TypeProduction
    (domonad [_ (asn-keywords "TYPE" "NOTATION" "::=")
              mal MacroAlternativeList]
             mal))

  (def MacroBody
    (domonad [tp TypeProduction
              vp ValueProduction
              sp SupportingProductions]
             {:TypeProduction tp
              :ValueProduction vp
              :SupportingProductions (reduce merge sp)}))

  (def MacroSubstance
    (choice
      (domonad [_ (asn-keyword "BEGIN")
                body MacroBody
                _ (asn-keyword "END")]
               body)
      macroreference
      Externalmacroreference))

  (def MacroDefinition
    (domonad [m macroreference
              _ (asn-keywords "MACRO" "::=")
              substance MacroSubstance]
             {m substance})))

(def unifyr (make-occurs-unify-fn record?))

(defn MacroAlternativeResolver 
  "Input is MacroDefinitionAlternative that represents
  sequence that defines this MacroAlternative"
  [macro-alternative-definition]
  )


(def test-macro
  (split-asn-elements
    "MODULE-IDENTITY MACRO ::=
    BEGIN
    TYPE NOTATION ::=\"LAST-UPDATED\" value(Update ExtUTCTime)
    \"ORGANIZATION\" Text
    \"CONTACT-INFO\" Text
    \"DESCRIPTION\" Text
    RevisionPart

    VALUE NOTATION ::=
    value(VALUE OBJECT IDENTIFIER)

    RevisionPart ::=
    Revisions
    | empty
    Revisions ::=
    Revision
    | Revisions Revision
    Revision ::=
    \"REVISION\" value(Update ExtUTCTime)
    \"DESCRIPTION\" Text

    -- a character string as defined in section 3.1.1
    Text ::= value(IA5String)
    END"))

(def test-macro2
  (split-asn-elements
    "OBJECT-TYPE MACRO ::=
    BEGIN
    TYPE NOTATION ::=
                  \"SYNTAX\" Syntax
                  UnitsPart
                  \"MAX-ACCESS\" Access
                  \"STATUS\" Status
                  \"DESCRIPTION\" Text
                  ReferPart
                  IndexPart
                  DefValPart

    VALUE NOTATION ::=
                  value(VALUE ObjectName)

    Syntax ::=   -- Must be one of the following:
                       -- a base type (or its refinement),
                       -- a textual convention (or its refinement), or
                       -- a BITS pseudo-type
                   type
                | \"BITS\" \"{\" NamedBits \"}\"

    NamedBits ::= NamedBit
                | NamedBits \",\" NamedBit

    NamedBit ::=  identifier \"(\" number \")\" -- number is nonnegative

    UnitsPart ::=
                  \"UNITS\" Text
                | empty

    Access ::=
                  \"not-accessible\"
                | \"accessible-for-notify\"
                | \"read-only\"
                | \"read-write\"
                | \"read-create\"

    Status ::=
                  \"current\"
                | \"deprecated\"
                | \"obsolete\"

    ReferPart ::=
                  \"REFERENCE\" Text
                | empty

    IndexPart ::=
                  \"INDEX\"    \"{\" IndexTypes \"}\"
                | \"AUGMENTS\" \"{\" Entry      \"}\"
                | empty
    IndexTypes ::=
                  IndexType
                | IndexTypes \",\" IndexType
    IndexType ::=
                  \"IMPLIED\" Index
                | Index
    Index ::=
                    -- use the SYNTAX value of the
                    -- correspondent OBJECT-TYPE invocation
                  value(ObjectName)
    Entry ::=
                    -- use the INDEX value of the
                    -- correspondent OBJECT-TYPE invocation
                  value(ObjectName)

    DefValPart ::= \"DEFVAL\" \"{\" Defvalue \"}\"
                | empty

    Defvalue ::=  -- must be valid for the type specified in
                  -- SYNTAX clause of same OBJECT-TYPE macro
                  value(ObjectSyntax)
                | \"{\" BitsValue \"}\"

    BitsValue ::= BitNames
                | empty

    BitNames ::=  BitName
                | BitNames \",\" BitName

    BitName ::= identifier

    -- a character string as defined in section 3.1.1
    Text ::= value(IA5String)
END

"
    )
  )
