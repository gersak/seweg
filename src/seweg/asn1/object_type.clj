(in-ns 'seweg.asn1.modules)


(defn- parse-object-info
  "Function parses object info from single
  OBJECT-TYPE definition"
  [text]
  (when-let [k (keyword (re-find (re-pattern var-name-pattern) text))]
    (let [syntax (clojure.string/trim (re-find #"(?<=SYNTAX).*" text))
          access (clojure.string/trim (re-find #"(?<=ACCESS).*" text))
          status (clojure.string/trim (re-find #"(?<=STATUS).*" text))
          description (second (re-find #"(?s)(?<=DESCRIPTION).*\"(.*?)\"" text))]
      (hash-map 
        k  
        {:syntax (when (seq syntax) (clojure.string/trim syntax))
         :access (when (seq access) (clojure.string/trim access))
         :status (when (seq status) (clojure.string/trim status))
         :description (when (seq description)
                        (clojure.string/replace description  #"\n\s+" "\n"))}))))

(def object-type-name-pattern (re-pattern (str var-name-pattern "\\s+OBJECT-TYPE")))

(defn- get-object-types-data
  "Function imports OBJECT-TYPE data from
  text input of module"
  [text]
  (let [text (remove-comments text)
        object-sections (type-definitions text object-type-name-pattern "^\\s+::=\\s+\\{.*\\}")
        oids-keys (map #(keyword (re-find (re-pattern var-name-pattern) %)) object-sections)
        object-oids (map #(re-find #"(?<=\{).*(?=\})"
                                   (re-find #"(?<=::=).*" %)) object-sections)
        object-oids (map #(when (seq %) 
                            (clojure.string/split (clojure.string/trim %) #"\s+")) object-oids)
        object-oids (map #(map read-string %) object-oids)
        object-oids (map #(map (fn [x] (if (number? x) x (keyword x))) %) object-oids)
        object-oid-mappings (reduce conj (map #(apply hash-map %) (partition 2 (interleave oids-keys object-oids))))]
    (hash-map 
      :oids object-oid-mappings
      :info (reduce conj (map parse-object-info object-sections)))))
