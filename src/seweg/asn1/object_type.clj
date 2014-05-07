(in-ns 'seweg.asn1.modules)


(def object-type-name-pattern "^\\s*\\w+[-_]*\\d*\\w*\\s+OBJECT-TYPE")
(def object-key-pattern #"\s*([a-z]\w+[-_]*\d*\w*)")


(defn- get-object-type-key [section]
  (keyword (second (re-find oid-key-pattern section))))

(defn- parse-object-info
  "Function parses object info from single
  OBJECT-TYPE definition"
  [text]
  (when-let [k (get-object-type-key text)] 
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
                        (clojure.string/replace description  #"\n\s+" "\n"))
         :type :object-type}))))


(defn- get-object-types-data
  "Function imports OBJECT-TYPE data from
  text input of module"
  [text]
  (try
    (when-let [object-sections (type-definitions (remove-comments text) object-type-name-pattern "^\\s+::=.*\\{.*\\}")]
      (let [oids-keys (map get-object-type-key object-sections)
            object-oids (map #(re-find #"(?<=\{).*(?=\})"
                                       (re-find #"(?<=::=).*" %)) object-sections)
            object-oids (map #(when (seq %) 
                                (clojure.string/split (clojure.string/trim %) #"\s+")) object-oids)
            object-oids (map #(map read-string %) object-oids)
            object-oids (map #(map (fn [x] (if (number? x) x (keyword x))) %) object-oids)]
        (hash-map 
          :oids (normalize-oid-mappings oids-keys object-oids) 
          :info (normalize-oid-info (map parse-object-info object-sections)))))
    (catch Exception e (.printStackTrace e))))
