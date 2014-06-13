(ns seweg.asn1.definitions.basic-types)


(def primitives #{"INTEGER" 
                  "BOOLEAN"
                  "NULL"
                  "ENUMERATED"
                  "REAL"
                  "BIT STRING"
                  "OCTET STRING"
                  "OBJECT IDENTIFIER"
                  "RELATIVE-OID"})

(def string-type #{"NumericString"
                   "PrintableString"
                   "VisibleString"
                   "ISO646String"
                   "IA5String"
                   "VideotexString"
                   "GraphicString"
                   "GeneralString"
                   "UniversalString"
                   "BMPString"
                   "UTF8String" })

(def constructed #{"SEQUENCE"
                   "SEQUENCE OF"
                   "SET"
                   "SET OF"
                   "CHOICE"
                   "SELECTION" })

(def builtin-types (reduce #(assoc %1 %2 (gensym)) {} (conj primitives constructed)))

(defn builtin-type? [^String type-name]
  (cond
    (contains? primitives type-name) :primitive
    (contains? constructed type-name) :constructed
    :else nil))
