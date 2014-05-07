(in-ns 'seweg.asn1.modules)

;;(defn list-oid 
;;  "Function returns sorted sequence of OID
;;  matching OID description"
;;  [& {:keys [match]}]
;;  (let [oids (sort
;;               (filter #(re-find (re-pattern (str "(?i).*" match ".*")) %) (map name (keys @oids))))]
;;    (doseq [x oids] (println x))))
