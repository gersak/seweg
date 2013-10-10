(in-ns 'seweg.asn1.modules)


(def oid-section-pattern "^\\s*(\\w+[-_]*\\d*\\w*)\\s+OBJECT\\s+IDENTIFIER")
(def oid-key-pattern #"\s*(\w+[-_]*\d*\w*)")

(defn- get-object-idetifier-key [section]
  (keyword (second (re-find oid-key-pattern section))))

(defn- get-pure-oids-data [text]
  (try
    (let [text (remove-comments text)
          oid-definitions (type-definitions text oid-section-pattern "\\{.*\\}")
          oids-keys (map get-object-idetifier-key oid-definitions) 
          oids-values (map #(re-find #"(?<=\{).*(?=\})" %) oid-definitions)
          oids-values (map #(when (seq %) (clojure.string/split (clojure.string/trim %) #"\s+")) oids-values)
          oids-values (map #(map read-string %) oids-values)
          oids-values (map #(map (fn [x] (if (number? x) x (keyword x))) %) oids-values)]
      {:oids (normalize-oid-mappings oids-keys oids-values)})
    (catch Exception e (do
                         (.printStackTrace e)
                         (println "ERROR in get-pure-oids-data")))))
