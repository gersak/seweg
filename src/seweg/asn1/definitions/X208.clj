(ns seweg.asn1.definitions.X208
  (:use dreamcatcher.core)
  (:require [clojure.core.unify :refer :all]
            [clj-tuple :as tuple :refer [tuple]]
            [clojure.algo.monads :refer :all]
            [seweg.asn1.core :refer [asn-meaning?
                                     split-asn-elements]])
  (:import [seweg.asn1.core ASNValueReference ASNTypeReference ASNMacroReference ASNModuleReference ASNExternalValueReference ASNExternalTypeReference]))

(declare Type Value BuiltinType BuiltinValue DefinedValue )

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



(def asn-x208-def (-> "resources/X208.asn1" slurp split-asn-elements))



(def oid-val-mapping (ref {[1] :iso}))

(def identifier-mapping (ref {:iso [1]}))

(def test-module (-> "mibs/ietf/RTP-MIB" slurp split-asn-elements))

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
  (defn get-one []
    (domonad [input (fetch-state)
              _ (set-state (rest input))]
             (first input)))
  
  (defn asn-keyword [word]
    (domonad [w (get-one)
              :when (= word w)]
             true))

  (defn match-expression [exp m-val]
    (fn [s] [(re-find exp m-val) s]))

  (defn m-asn-meaning? [x]
    (fn [s]
      [(asn-meaning? x) s]))

  (defn m-satisfies? [p x]
    (fn [s]
      (when (p x) [x s])))


  (def identifier
    (domonad [id (get-one)
              rid (m-satisfies? (partial re-find #"[\w\d-]+") id)]
             (keyword rid)))

  (def valuereference
    (domonad [w identifier
              m (m-asn-meaning? (name w))
              _ (m-satisfies? #{:valuereference} m)]
             (ASNValueReference. w)))
             ;{:valuereference w}))

  (def typereference 
    (domonad [w identifier
              m (m-asn-meaning? (name w))
              _ (m-satisfies? #{:typereference} m)]
             (ASNTypeReference. w)))
             ;{:typereference w}))

  (def macroreference 
    (domonad [w identifier
              m (m-asn-meaning? (name w))
              _ (m-satisfies? #{:macroreference} m)]
             (ASNMacroReference. w)))
             ;{:macroreference w}))

  (def number
    (domonad [n (get-one)
              :when (re-find #"\d+" n)]
             (read-string n)))

  (def Externalvaluereference
    (domonad [m identifier 
              _ (asn-keyword ".")
              v identifier]
             (ASNExternalValueReference. m v)))

  

  (defn while-m-object [object]
    (fn [s]
      (loop [r [] s s]
        (if-let [[nr new-state] (object s)]
          (if nr
            (recur (conj r nr) new-state)
            (if (seq r) [r s]))
          (if (seq r) [r s])))))

  (defn when-m-seq [values]
    (fn [s]
      (if (= (seq values) (take (count values) s))
        [true (drop (count values) s)]
        nil)))

  (defn asn-keywords [& words]
    (when-m-seq words))

  (defn choice [& definitions]
    (reduce m-plus definitions))

  (def asn-comma (asn-keyword ",")))

;; ASN X208 definitions

(with-monad asn-seq-track
  (def DefinedValue 
    (choice
      Externalvaluereference
      valuereference))

  (def NameForm identifier)

  (def NumberForm (choice number DefinedValue))

  (def NameAndNumberForm
    (domonad [id identifier 
              _ (asn-keyword "(")
              n number 
              _ (asn-keyword ")")]
             {n id}))

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
               [dv component-list])
      (domonad [_ (asn-keyword "{")
                component-list ObjIdComponentList
                _ (asn-keyword "}")]
               component-list)))

  (def AssignedIdentifier
    (choice 
      ObjectIdentifierValue 
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
    (choice typereference valuereference macroreference))
  
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
    (choice SymbolsFromModuleList (m-result nil)))
  
  (def Imports
    (choice
      (domonad [_ (asn-keyword "IMPORTS")
                symbols SymbolsImported
                _ (asn-keyword ";")]
               symbols)
      (m-result nil)))

  (def Subtype
   (m-result nil)) 
  
  (def DefinedType
    typereference)



  (def Type
    (choice
      ;Subtype
      BuiltinType 
      DefinedType))


  (load "builtin_types")
  (load "builtin_values")

  (def Typeassignment
    (domonad [tr typereference
              _ (asn-keyword "::=")
              t Type]
             {tr t}))

  (def Valueassignment (m-result nil))

  (def Assignment
    (choice Typeassignment Valueassignment))
  
  (def AssignmentList
    (m-result nil))

  (def ModuleBody
    (choice 
      (domonad 
        [exports Exports
         imports Imports
         as-list AssignmentList]
        {:exports exports
         :imports imports
         :AssignmentList as-list})
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

(defn test-monad []
  (domonad maybe-m
           [a 1
            b nil 
            c 3]
           (list a b c)))


(with-monad maybe-m (defn test-monad2 []
                      (let [a nil 
                            b 2
                            c 3]
                        (reduce m-plus [a c b]))))
