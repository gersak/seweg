(ns seweg.asn1.definitions.strings)

(defn- char-range [^java.lang.Character a ^java.lang.Character b]
  (set (map char (range (int a) (int b)))))

(defn- char-map [^clojure.lang.PersistentVector v]
  (set (map char v)))

(def constrains 
  {:NumericString (reduce conj #{} [(char-range \0 \9) [\space]]) 
   :PrintableString (reduce conj #{} [(char-range \A \Z) (char-range \a \z) [\' \( \) \+ \, \- \. \/ \: \= \?]])
   :VisibleString #{(char 6) \space}
   :ISO646String #{(char 6) \space} 
   :IA5String  (char-range (char 0) (char 255))
   :TeletexString (char-map [6 87 102 103 106 107 126 144 150 153 156 164 165 168])
   :T61String (char-map [6 87 102 103 106 107 126 144 150 153 156 164 165 168])
   :VideotexString (char-map [1 13 72 73 87 89 102 108 126 128 128 144 150 153 164 165])
   :GraphicString nil
   :GeneralString nil
   :UniversalString nil
   :BMPString nil
   :UTF8String nil})
