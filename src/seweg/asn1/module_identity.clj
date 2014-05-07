(in-ns 'seweg.asn1.modules)

(def test-module "./mibs/ietf/IP-FORWARD-MIB")

(def module-identity-key-pattern #"\w+[-_a-zA-Z0-9]*(?= +MODULE-IDENTITY)")

(defn- get-module-key [section]
  (re-find module-identity-key-pattern section))
  ;(keyword (second (re-find module-identity-key-pattern section))))

(defn- parse-module-identity
  "Function parses object info from single
  OBJECT-TYPE definition"
  [text]
  (when-let [k (get-module-key text)]
    (let [organization (re-find #"(?s)(?<=ORGANIZATION)\s+\".*?\"" text)
          last-updated (re-find #"(?<=LAST-UPDATED)\s+\".*?\"" text)
          contact-info (re-find #"(?s)(?<=CONTACT-INFO)\s+\".*?\"" text)
          description (re-seq #"(?s)(?<=DESCRIPTION)\s+\".*?\"" text)]
      (hash-map 
        k  
        {:organization (when (seq organization) (clojure.string/trim organization))
         :last-updated (when (seq last-updated) (clojure.string/trim last-updated))
         :description description
         :contact-info (when contact-info (clojure.string/trim contact-info))
         :type :module-identity}))))

(def module-identity-section-pattern "^\\s*\\w+[-_]*\\d*\\w*\\s+MODULE-IDENTITY")

(defn- get-module-identity-data
  [text]
  (when-let [module-sections (type-definitions (remove-comments text) module-identity-section-pattern "\\s*::=\\s+\\{.*\\}")]
    (let [module-keys (map get-module-key module-sections)
          module-oids (map #(re-find #"(?<=\{).*(?=\})"
                                     (re-find #"(?<=::=).*" %)) module-sections)
          module-oids (map #(when (seq %) 
                              (clojure.string/split (clojure.string/trim %) #"\s+")) module-oids)
          module-oids (map #(map read-string %) module-oids)
          module-oids (map #(map (fn [x] (if (number? x) x (keyword x))) %) module-oids)]
      (hash-map 
        :oids (normalize-oid-mappings module-keys module-oids)
        :info (normalize-oid-info (map parse-module-identity module-sections))))))
