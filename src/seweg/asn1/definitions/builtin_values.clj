(in-ns 'seweg.asn1.definitions.X208)

(with-monad asn-seq-track
 (def BooleanValue
   (choice 
     (bind-value-for true "TRUE")
     (bind-value-for false "FALSE")))

 (def IntegerValue
   (choice
     number
     (domonad 
       [id identifier
        :when nil]
       id)))
 
 (def bstring
   (domonad [_ (asn-keyword "'")
             bsv (get-one)
             _ (asn-keywords "'" "B")]
            (.toByteArray 
              (biginteger 
                (reduce bit-set 0 
                        (keep-indexed (fn [i n] (if (= \1 n) i)) bsv))))))

 (def hstring
   (domonad [_ (asn-keyword "'")
             hdv (get-one)
             :when (when (seq hdv) (re-find #"[a-fA-f0-9]" hdv))
             _ (asn-keywords "'" "H")]
            (. javax.xml.bind.DatatypeConverter parseHexBinary hdv)))

 (def IdentifierList 
   (while-m-object
     (choice
       (domonad [id identifier
                 _ asn-comma]
                id)
       identifier)))

 (def BitStringValue
   (choice
     bstring
     hstring
     (domonad [_ (asn-keyword "{")
               il IdentifierList
               _ (asn-keyword "}")]
              il)))

 (def OctetStringValue
   (choice
     bstring
     hstring))

 (def NullValue 
   (bind-value-for nil "NULL"))

 (def NumericRealValue
   (domonad [_ (asn-keyword "{")
             m (get-one)
             _ asn-comma
             b number
             :when (#{2 10} b) ;; Currently not supproting base 2
             _ asn-comma
             e number
             _ (asn-keyword "}")]
            (let [m (java.lang.Integer/parseInt m b)]
              (* m (java.lang.Math/pow b e)))))

 (def SpecialRealValue
   (choice
     (bind-value-for Double/POSITIVE_INFINITY "PLUS-INFINITY")
     (bind-value-for Double/NEGATIVE_INFINITY "MINUS-INFINITY")))

 (def RealValue
   (choice
     SpecialRealValue
     NumericRealValue))

 (def NamedValue
   (choice
     (domonad [id identifier
               v Value]
              {id v})
     Value))

 (def ElementValueList
   (while-m-object
     (choice 
       (domonad [nv NamedValue
                 _  asn-comma]
                nv)
       NamedValue)))
 
 (def SequenceValue
   (domonad [_ (asn-keyword "{") 
             el ElementValueList
             _ (asn-keyword "}")]
            el))

 (def ValueList
   (while-m-object
     (choice 
       (domonad [v Value
                 _ asn-comma]
                v)
       Value)))

 (def SequenceOfValue
   (domonad [_(asn-keyword "{")
             vl ValueList
             _ (asn-keyword "}")]
            vl)))

(with-monad asn-seq-track
  (def SetValue 
    (choice 
      (domonad [_ (asn-keyword "{")
                el ElementValueList
                _ (asn-keyword "}")]
               el)
      (domonad [_ (asn-keywords "{" "}")]
               #{})))
  
  (def SetOfValue
    (domonad [_ (asn-keyword "{")
              vl ValueList
              _ (asn-keyword "}")]
             vl))
  
  (def ChoiceValue NamedValue)

  (def TaggedValue Value)

  (def ObjIdComponent
    (choice NameAndNumberForm NumberForm NameForm))

  (def ObjIdComponentList
    (while-m-object ObjIdComponent))

  (def ObjectIdentifierValue
    (choice
      (domonad [_ (asn-keyword "{")
                dv DefinedValue
                component-list ObjIdComponentList
                _ (asn-keyword "{")]
              (do
                ;(println "referenced " dv)
                (if (vector? dv)
                  (reduce into dv component-list)
                  (reduce into [dv] component-list))))
      (domonad [_ (asn-keyword "{")
                component-list ObjIdComponentList
                _ (asn-keyword "}")]
               (do
                 ;(println "flat" component-list)
                 (vec (flatten component-list))))))

  (def cstring
    (domonad [cs (get-one)
              :when (and (= (first cs) \") (= (last cs) \"))]
             (apply str (butlast (rest cs)))))
  
  (def CharacterStringValue
    cstring)
  
  (def EnumeratedValue identifier)

  (def ValueFunctions 
    [#'BooleanValue
     #'IntegerValue
     #'RealValue  
     #'BitStringValue
     #'OctetStringValue
     #'NullValue
     #'SequenceOfValue
     #'SequenceValue
     #'SetOfValue
     #'SetValue
     ;#'ChoiceValue
     ;#'TaggedValue
     #'ObjectIdentifierValue
     #'CharacterStringValue
     #'EnumeratedValue])

  (defn resolvableValue? [data]
    (some #(when-let [r (% data)]
             (println (apply str (repeat 10 \-)) "Resolved by" %)
             r) ValueFunctions))

  (def BuiltinValue
    (apply choice ValueFunctions))
  
  (def Value
    (choice BuiltinValue DefinedValue)))
