(ns seweg.asn1.universal)

(def ans1-universal-types
  {:Boolean 0x01
   :Integer 0x02
   :BitString 0x03
   :OctetString 0x04
   :Null 0x05
   :ObjectIdentifier 0x06
   :ObjectDescriptor 0x07
   :External 0x08
   :Real 0x09
   :Enumerated 0x0A
   :Sequence 0x10
   :Set 0x11
   :PrintableString 0x13
   :T61String 0x14
   :IA5String 0x16
   :UTCTime 0x17})

(def single-line-comment "--")

;; Identifiers begin with lowercase letters, and types with uppercase letters
;; Structured types are: SEQUENCE and SEQUENCE OF, SET and SET OF
;; SEQUENCE, an ordered collection of one or more types. 
;; SEQUENCE OF, an ordered collection of zero or more occurrences of a given type
;; SET, an unordered collection of one or more types
;; SET OF, an unordered collection of zero or more occurrences of a given type

(defn set-class [input class-type]
 (case class-type
   :universal (-> input (bit-clear 7) (bit-clear 6))
   :application (-> input (bit-clear 7) (bit-set 6))
   :context-specific (-> input (bit-set 7) (bit-clear 6))
   :private (-> input (bit-set 7) (bit-set 6))))

(defn constructed? [input]
  (bit-test input 5))

(defn primitive? [input]
  (not (constructed? input)))

(def length-octet 0x80)

(def end-of-content 0x0000)
