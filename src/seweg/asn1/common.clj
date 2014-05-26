(ns seweg.asn1.common)

(defn remove-comments 
  "Function removes comments from
  ASN1 input text string"
  [^String text]
  (when (seq text) (clojure.string/replace text #"(--.*--|--.*(?=\n))" "")))

(def sample-comment (slurp "./mibs/cisco/CISCO-SMI-V1SMI.my"))
