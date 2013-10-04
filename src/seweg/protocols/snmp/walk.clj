(ns seweg.protocols.snmp.walk
  (:use seweg.protocols.snmp
        seweg.protocols.snmp.oid-repository
        lamina.core))

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
                (list oids))
         c (get-snmp-channel)
         filter-oid-fn (fn [oid] 
                         (fn [x] (is-child-of-oid? (first (keys x)) oid)))
         transmition-fn (fn [oid]
                          (loop [last-oid (normalize-oid oid)
                                 result []]
                            (enqueue @c (bulk-fn [last-oid]))
                            (let [r (read-channel @c) 
                                  _ (wait-for-result r timeout)
                                  vb (get-variable-bindings @r)
                                  last-oid (-> vb last keys first)]
                              (if (not (is-child-of-oid? last-oid oid)) (doall (filter (filter-oid-fn oid) (into result vb)))
                                (recur last-oid (into result vb))))))]
     (try
       (flatten (for [x oids] (transmition-fn x)))
       (catch Exception e nil)
       (finally (close @c))))))

;; NOT FINISHED
(defn snmp-walk [host community oid]
  (let [get-fn (open-line host community :pdu-type :get-request)
        c (get-snmp-channel)]
    (try
      (enqueue @c (get-fn [oid]))
      (let [r (read-channel @c)
            _ (wait-for-result r 2000)
            vb (get-variable-bindings @r)]
        vb)
      (catch Exception e nil)
      (finally (close @c)))))


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
