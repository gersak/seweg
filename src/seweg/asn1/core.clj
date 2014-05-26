(ns seweg.asn1.core
  (:use seweg.asn1.basic-types)
  (:require frak))

(defn remove-comments 
  "Function removes comments from
  ASN1 input text string"
  [^String text]
  (when (seq text) (clojure.string/replace text #"(--.*--|--.*(?=\n))" "")))

;; New part 06.05.2014
(defrecord ObjectIdentifier [^clojure.lang.PersistentVector oid]
  Object
  (toString [_] (apply str (interpose "." oid))))

(defn resolve-type 
  "For given string word returns ASN type.

  Either:

  :Type
  :Value
  :Keyword
  "

  [^String word]
  {:pre [(= word (first (re-find #"^[\w\d]+(-{0,1}[\w\d]*)+" word)))]}
  (cond
    (= word (re-find #"[A-Z\-]+" word)) :ASN-Keyword
    (= word (re-find #"^[a-z][A-Z][\w\d-]+" word)) :ASN-Value
    (= word (re-find #"^[A-Z][a-z0-9-]+" word)) :ASN-Type
    :else :ASN-Unknown))
  

(def sample-comment (slurp "./mibs/cisco/CISCO-SMI-V1SMI.my"))

(def ^:private find-positions
  "Function returns sequence of positions
  inside of text that match pattern string.

  text is sequence of strings"
  (fn [text pattern]
             (let [text (vec text)
                   found-lines (set (filter #(seq (re-find pattern %)) text))]
               (vec
                 (remove nil?
                         (for [x (range (count text))]
                           (when (found-lines (text x)) x)))))))

(defn- extract-sections
  "Extracts section of text that is
  matched by start reg-ex and end
  reg-ex.
  Return result is vector with extracted
  section as first result and rest of text
  as second result"
  [^String text ^java.util.regex.Pattern start ^java.util.regex.Pattern end]
  (let [text (when text (clojure.string/split-lines text))
        text (vec (filter seq text)) ;; Get rid of empty lines
        start-positions (find-positions text start)
        delimiter-positions (find-positions text end)]
    ;;(-> delimiter-positions count println)
    (when (and (seq start-positions) (seq delimiter-positions))
      (let [sections (loop [beginings start-positions
                            delimiters delimiter-positions
                            sections []]
                       (if-not (seq beginings) sections
                         (let [new-delimiters (drop-while #(> (first beginings) %) delimiters)]
                           (if (= (first beginings) (first new-delimiters))
                             (recur (rest beginings) (rest new-delimiters) (conj sections [(text (first beginings))]))
                             (recur (rest beginings) (rest new-delimiters) (conj sections (subvec text (first beginings) (inc (first new-delimiters)))))))))]
        (doall (map #(apply str (interpose "\n" %)) sections))))))

(defn extract-section [^String text ^java.util.regex.Pattern start ^java.util.regex.Pattern end]
  (first (extract-sections text start end)))

(defn- get-module-imports [body]
  (let [import-body (apply str (take-while #(not= % \;) (re-find #"(?s)(?<=IMPORTS).*" (remove-comments body))))
        froms (map clojure.string/trim (re-seq #"(?<=FROM).*(?=\n*)" import-body))
        froms-pattern (frak/pattern froms)
        clean-imports (map #(clojure.string/replace % #"\s+FROM\s+" "") (clojure.string/split import-body froms-pattern))
        clean-imports (map #(-> % clojure.string/trim-newline clojure.string/trim) clean-imports)
        import-names (map #(map clojure.string/trim (clojure.string/split % #",")) clean-imports)] 
    (apply hash-map (interleave froms import-names))))


(def sample-imports (slurp "./mibs/cisco/CISCO-TC-V1SMI.my"))
(def sample-object (slurp "./mibs/ietf/IF-MIB"))
