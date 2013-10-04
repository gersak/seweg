(in-ns 'seweg.asn1.modules)

(def test-module "./mibs/mibs/ietf/IP-FORWARD-MIB")

(defn- parse-module-identity
  "Function parses object info from single
  OBJECT-TYPE definition"
  [text]
  (when-let [k (keyword (re-find (re-pattern var-name-pattern) text))]
    (let [organization (clojure.string/trim (re-find #"(?<=ORGANIZATION).*" text))
          last-updated (clojure.string/trim (re-find #"(?<=LAST-UPDATED).*" text))
          description (re-find #"(?s)\s*DESCRIPTION.*\".*\"" text)]
      (hash-map 
        k  
        {:organization (when (seq organization) (clojure.string/trim organization))
         :last-updated (when (seq last-updated) (clojure.string/trim last-updated))
         :description description}))))

(def module-identity-name-pattern (re-pattern (str var-name-pattern "\\s+MODULE-IDENTITY")))

(defn- get-module-identity-data
  [text]
  (let [text (remove-comments text)
        module-sections (type-definitions text module-identity-name-pattern "^\\s+::=\\s+\\{.*\\}")
        module-keys (map #(keyword (re-find (re-pattern var-name-pattern) %)) module-sections)
        module-oids (map #(re-find #"(?<=\{).*(?=\})"
                                   (re-find #"(?<=::=).*" %)) module-sections)
        module-oids (map #(when (seq %) 
                            (clojure.string/split (clojure.string/trim %) #"\s+")) module-oids)
        module-oids (map #(map read-string %) module-oids)
        module-oids (map #(map (fn [x] (if (number? x) x (keyword x))) %) module-oids)
        module-oids-mapping (reduce conj (map #(apply hash-map %) (partition 2 (interleave module-keys module-oids))))]
    (hash-map 
      :oids module-oids-mapping
      :info (reduce conj (map parse-module-identity module-sections)))))
