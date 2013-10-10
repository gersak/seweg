(in-ns 'seweg.asn1.modules)

(def test-module "./mibs/mibs/ietf/IP-FORWARD-MIB")

(def module-identity-key-pattern #"\s*(\w+[-_]*\d*\w*)")

(defn- get-module-key [section]
  (keyword (second (re-find module-identity-key-pattern section))))

(defn- parse-module-identity
  "Function parses object info from single
  OBJECT-TYPE definition"
  [text]
  (when-let [k (keyword (second (re-find module-identity-key-pattern text)))]
    (let [organization (re-find #"(?<=ORGANIZATION).*" text)
          last-updated (re-find #"(?<=LAST-UPDATED).*" text)
          description (re-find #"(?s)\s*DESCRIPTION.*\".*\"" text)]
      (hash-map 
        k  
        {:organization (when (seq organization) (clojure.string/trim organization))
         :last-updated (when (seq last-updated) (clojure.string/trim last-updated))
         :description description}))))

(def module-identity-section-pattern "^\\s*\\w+[-_]*\\d*\\w*\\s+MODULE-IDENTITY")

(defn- get-module-identity-data
  [text]
  (let [text (remove-comments text)
        module-sections (type-definitions text module-identity-section-pattern "\\s*::=\\s+\\{.*\\}")
        module-keys (map get-module-key module-sections)
        module-oids (map #(re-find #"(?<=\{).*(?=\})"
                                   (re-find #"(?<=::=).*" %)) module-sections)
        module-oids (map #(when (seq %) 
                            (clojure.string/split (clojure.string/trim %) #"\s+")) module-oids)
        module-oids (map #(map read-string %) module-oids)
        module-oids (map #(map (fn [x] (if (number? x) x (keyword x))) %) module-oids)]
    (hash-map 
      :oids (normalize-oid-mappings module-keys module-oids)
      :info (normalize-oid-info (map parse-module-identity module-sections)))))
