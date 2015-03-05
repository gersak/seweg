(ns seweg.asn1.definitions.X208
  (:require [clojure.core.unify :refer :all]
            [clojure.algo.monads :refer :all]
            [seweg.asn1.core :refer [asn-meaning?
                                     split-asn-elements
                                     print-asn]])
  (:import [seweg.asn1.core
            ASNValueReference
            ASNTypeReference
            ASNMacroReference
            ASNModuleReference
            ASNExternalValueReference
            ASNExternalTypeReference
            ASNExternalMacroReference
            ASNProductionReference
            ASNLocalTypeReference
            ASNLocalValueReference]))

(declare Type Value BuiltinType BuiltinValue DefinedValue ObjectIdentifierValue)

(def unifyk (make-unify-fn keyword?))

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

(def oid-val-mapping (ref {[1] :iso}))

(def identifier-mapping (ref {:iso [1]}))

(def test-module (-> "mibs/ietf/SNMPv2-SMI" slurp split-asn-elements))
;(def test-module (-> "mibs/ietf/RTP-MIB" slurp split-asn-elements))


(defmonad asn-seq-track
  [m-result  (fn  [value]
               (fn  [state]
                 [value state]))

   m-bind  (fn  [computation func]
             (fn  [state]
               (when-let  [[value new-state]  (computation state)]
                 ((func value) new-state))))

   m-zero  (fn  [new-state] nil)

   m-plus  (fn  [left right]
             (fn  [state]
               (if-let  [result  (left state)]
                 result
                 (right state))))])



;; PARSERER FUNCTIONS

(with-monad asn-seq-track
  ; PREPARATION AND STATE CHANGES
  ;(defn prepare-for-resolving [^String data]
  ;  (split-asn-elements data))

  ;(defn get-one []
  ;  (domonad [input (fetch-state)
  ;            _ (set-state (rest input))]
  ;           (first input)))

  (defn prepare-for-resolving [^String data]
    {:ros_ (split-asn-elements data)})

  (defn get-one []
    (domonad [input (fetch-state)
              _ (set-state (update-in input [:ros_] rest))]
             (first (:ros_ input))))

  (defn when-m-seq [values]
    (fn [s]
      (if (= (seq values) (take (count values) (:ros_ s)))
        [true (update-in s [:ros_] #(drop (count values) %))]
        nil)))

  (defn while-m-object [object]
    (fn [s]
      (loop [r [] s s]
        (if-let [[nr new-state] (object s)]
          (if nr
            (recur (conj r nr) new-state)
            (if (seq r) [r s]))
          (if (seq r) [r s])))))

  (defn asn-keyword [word]
    (domonad [w (get-one)
              :when (= word w)]
             true))

  (defn asn-when-not-def [obj]
   (domonad [cs (fetch-state)
             _ (get-one)
             w (get-one)
             :when (not= "::=" w)
             _ (set-state cs)
             x obj]
            x))

  (defn asn-keywords [& words]
    (when-m-seq words))

  (def identifier
    (domonad [id (get-one)
              :when (when (seq id) ((partial re-find #"[\w\d-]+") id))]
             id))

  (def valuereference
    (domonad [w identifier
              :when (= :valuereference (asn-meaning? w))]
              ;vr (fetch-val :value-reference)
              ;_ (if-let [r (get vr w)]
              ;    (if (realized? r)
              ;      @r
              ;      r)
              ;    (set-val-in [:value-reference w] (promise)))]
             (ASNValueReference. w)))

  (def typereference
    (domonad [w identifier
              :when (= :typereference (asn-meaning? w))]
             (ASNTypeReference. w)))

  (def macroreference
    (domonad [w identifier
              :when (= :typereference (asn-meaning? w))]
             (ASNMacroReference. w)))

  (def number
    (domonad [n (get-one)
              :when (when (seq n) (= (re-find #"[\d-\+]+" n) n))]
             (read-string n)))

  (def Externalvaluereference
    (domonad [m identifier
              _ (asn-keyword ".")
              v identifier]
             (ASNExternalValueReference. m v)))

  (defn bind-value [value]
    (fn [_]
      (m-result value)))

  (defn bind-value-for [asn-type & words]
    (m-bind (apply asn-keywords words) (bind-value asn-type)))


  (defn choice [& definitions]
    (reduce m-plus definitions))

  (def asn-comma (asn-keyword ",")))

;; ASN X208 definitions

(with-monad asn-seq-track
  (def DefinedValue
    (choice
      #'Externalvaluereference
      (domonad [vr valuereference
                known-references (fetch-val :value-reference)]
               (do
                 (if-let [r (get known-references vr)]
                   (if (realized? r)
                     (:value @r)
                     r)
                   vr)))))
  ;(def DefinedValue
  ;  (choice
  ;    #'Externalvaluereference
  ;    #'valuereference))

  (def NameForm identifier)

  (def NumberForm (choice #'number #'DefinedValue))

  (def NameAndNumberForm
    (domonad [id identifier
              _ (asn-keyword "(")
              n number
              _ (asn-keyword ")")]
             n))
             ;;{n id}))

  (def AssignedIdentifier
    (choice
      #'ObjectIdentifierValue
      (m-result nil)))

  (def TagDefault
    (choice
      (domonad [_ (asn-keywords "EXPLICIT" "TAGS")]
               {:TagDefault :EXPLICIT})
      (domonad [_ (asn-keywords "IMPLICIT" "TAGS")]
               {:TagDefault :IMPLICIT})
      (m-result nil)))

  (def ModuleIdentifier
    (domonad [mr (get-one)
              aid AssignedIdentifier]
             (ASNModuleReference. mr aid)))

  (def Symbol
    (choice #'typereference #'valuereference #'macroreference))

  (def SymbolList
    (while-m-object
      (choice
        (domonad [s Symbol
                  _ asn-comma]
                 s)
        Symbol)))

  (def SymbolsExported
    (choice
      SymbolList
      (m-result nil)))

  (def Exports
    (choice
      (domonad
        [_ (asn-keyword "EXPORTS")
         symbols SymbolsExported
         _ (asn-keyword ";")]
        symbols)
      (m-result nil)))

  (def SymbolsFromModule
    (domonad [s SymbolList
              _ (asn-keyword "FROM")
              module identifier]
             {(ASNModuleReference. module nil) s}))

  (def SymbolsFromModuleList
    (domonad [imports (while-m-object SymbolsFromModule)]
             imports))

  (def SymbolsImported
    (choice #'SymbolsFromModuleList (m-result nil)))

  (def Imports
    (choice
      (domonad [_ (asn-keyword "IMPORTS")
                symbols SymbolsImported
                _ (asn-keyword ";")]
               symbols)
      (m-result nil))))



(defn update-val-in [ks f]
  (fn [s]
    (let [ks (if-not (vector? ks) (vector ks) ks)
          old-val (get-in s ks)
          new-s (update-in s ks f)]
      [old-val new-s])))

(defn set-val-in [ks value]
  (do
    ;(println ks value)
    (update-val-in ks (fn [_] value))))



(load "builtin_values")
(load "builtin_types")
(load "macros")

(with-monad asn-seq-track
  (def Typeassignment
    (domonad [tr typereference
              _ (asn-keyword "::=")
              t Type]
              ;type-references (fetch-val :type-reference)
              ;_ (set-val-in [:type-reference (:reference tr)]
              ;              (promise))]
              (do
                ;(println tr t)
                {tr t})))

  (def Valueassignment
    (domonad [vr valuereference
              tr Type
              _ (asn-keyword "::=")
              st (fetch-val :type-reference)
              sv (fetch-val :value-reference)
              ;; r is monad that resolves
              ;; value of following sequence
              r (get st (:type tr))]
              ;_ (set-val-in [:value-reference vr]
              ;              @(future
              ;                 (if (get sv vr)
              ;                   (deliver (get sv vr) (merge {:value r} tr))
              ;                   (deliver (promise) (merge {:value r} tr)))))]
             {vr (merge {:value r} tr)}))

  (def Assignment
    (choice #'Typeassignment #'Valueassignment #'MacroDefinition))

  (def AssignmentList
    (while-m-object Assignment))

  (def ModuleBody
    (choice
      (domonad
        [exports Exports
         imports Imports
         as-list AssignmentList]
        {:exports exports
         :imports imports
         :AssignmentList (reduce merge as-list)})
      (m-result nil)))

  (def ModuleDefinition
    (domonad [mr ModuleIdentifier
              _ (asn-keyword "DEFINITIONS")
              tag TagDefault
              _ (asn-keywords "::=" "BEGIN")
              mb ModuleBody]
              ;_ (asn-keyword "END")]
             {:ModuleIdentifier mr
              :TagDefault tag
              :ModuleBody mb})))

(def builtin-references
  {:OBJECT_IDENTIFIER ObjectIdentifierValue
   :BOOLEAN BooleanValue
   :INTEGER IntegerValue
   :BIT_STRING BitStringValue
   :NULL NullValue
   :ENUMERATED EnumeratedValue
   :SEQUENCE SequenceValue
   :SEQUENCE_OF SequenceOfValue
   :SET SetValue
   :SET_OF SetOfValue
   ;:NumericString NumericString
   :IA5String astring
   :OCTET_STRING OctetStringValue})
