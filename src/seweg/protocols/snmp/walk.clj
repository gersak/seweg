(ns seweg.protocols.snmp.walk
  (:use seweg.protocols.snmp
        seweg.protocols.snmp.oid-repository
        lamina.core))

(load "/seweg/coders/snmp")

(defn snmp-bulk-walk
  "Function tries to walk OID tree as far as response
  contains input oid value. If it fails or times out
  function will return nil. Input OIDs accepts vector
  of OID values. Either as keywords or vectors.

  Default timeout is 2s. "
  ([host community oids] (snmp-bulk-walk host community oids 2000))
  ([host community oids timeout]
   (let [bulk-fn (open-line host community :pdu-type :get-bulk-request)
         oids (if (coll? oids) (map normalize-oid oids)
                (list (normalize-oid oids)))
         result (atom [])
         filter-oid-fn (fn [oid] 
                         (fn [x] (is-child-of-oid? (first (keys x)) oid)))
         transmition-fn (fn [oid]
                          (let [c (get-snmp-channel)]
                            (try
                              (loop [last-oid (normalize-oid oid)]
                                (enqueue @c (bulk-fn [last-oid]))
                                (let [r (read-channel @c) 
                                      _ (wait-for-result r timeout)
                                      vb (get-variable-bindings @r)
                                      last-oid (-> vb last keys first)]
                                  (if (not (is-child-of-oid? last-oid oid)) 
                                    (do 
                                      (swap! result into (filter (filter-oid-fn oid) vb)))
                                    (do
                                      (swap! result into vb)
                                      (recur (-> @result last first key))))))
                              (catch Exception e nil)
                              (finally (close @c)))))]
       (doseq [x oids] 
             (transmition-fn x))
       @result)))


;; NOT FINISHED
(defn snmp-walk 
  ([host community oids] (snmp-walk host community oids 2000))
  ([host community oids timeout]
   (let [result (atom [])
         oids (if (coll? oids) 
                (map normalize-oid oids)
                [oids])]
     (doseq [x oids]
       (let [x (normalize-oid x)]
         (loop [q (snmp-get-next nil host community x)]
           (when q
             (let [lo (-> q ffirst key)]
               (if-not (is-child-of-oid? lo x)
                 @result
                 (do
                   (swap! result into q)
                   (recur (snmp-get-next nil host community lo)))))))))
     @result)))


(defn tabelize-fix-length
  "Function returns single map that has keyword
  as fix-length input vector and a vector of values
  as map value. Basicly it filters OID from data"
  [data fix-oid]
  (let [oid (normalize-oid fix-oid)
        fd (filter #(= (take (count oid) (-> % keys first)) oid) data)]
    fd))

(defn tabelize-index
  [data fix-oid]
  (let [oid (normalize-oid fix-oid)
        fd (filter #(= (take (count oid) (-> % keys first)) oid) data)
        d (map #(vec (drop (count oid) (-> % keys first))) fd)]
    d))


(defn make-table
  "Function creates vector of maps that have
  keys of input paramater keywords. It filters data
  based on header-oids and in that order associates
  keys to found values.

  Data is supposed to be variable bindings data."
  [data header-oids keywords]
  (assert (= (count header-oids) (count keywords)) "Count heder-oids and keywords has to be equal")
  (let [indexes (sort (reduce into #{} (map #(tabelize-index data %) header-oids)))
        mapped-data (if-not (map? data) (reduce into {} data))
        mapping (apply hash-map (interleave header-oids keywords))
        get-values (fn [index] 
                     (apply merge 
                            (map #(hash-map 
                                    (get mapping %) 
                                    (get mapped-data (into (normalize-oid %) index))) header-oids)))]
    (map get-values indexes)))
