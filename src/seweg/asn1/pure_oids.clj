(in-ns 'seweg.asn1.modules)


(def oid-name-pattern (str var-name-pattern "\\s*OBJECT\\s*IDENTIFIER"))

(defn- get-pure-oids-data [text]
  (let [text (remove-comments text)
        oid-definitions (type-definitions text oid-name-pattern "::=")
        oids-keys (map #(keyword (re-find (re-pattern var-name-pattern) %)) oid-definitions)
        oids-values (map #(re-find #"(?<=\{).*(?=\})" %) oid-definitions)
        oids-values (map #(when (seq %) (clojure.string/split (clojure.string/trim %) #"\s")) oids-values)
        oids-values (map #(map read-string %) oids-values)
        oids-values (map #(map (fn [x] (if (number? x) x (keyword x))) %) oids-values)]
    {:oids (dissoc (reduce conj (map #(apply hash-map %) (partition 2 (interleave oids-keys oids-values)))) nil)}))
