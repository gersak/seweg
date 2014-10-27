(ns seweg.client.aleph
  (:require 
    [lamina.core :refer [enqueue
                         read-channel
                         wait-for-result 
                         close 
                         receive-all 
                         idle-result]]
    [seweg.protocols.snmp :as s :refer [compose-snmp-packet
                                        decompose-snmp-response
                                        open-line
                                        get-variable-bindings
                                        snmp-template
                                        get-new-rid
                                        make-table
                                        ]]
    [seweg.protocols.snmp.oid-repository :refer [normalize-oid is-child-of-oid?]]
    [seweg.coders.snmp :refer [SNMP]]
    [aleph.udp :refer [udp-socket]]
    [gloss.core :refer [compile-frame]]
    [clojure.core.async :as a])
  (:import 
    [java.net
     DatagramPacket
     DatagramSocket
     InetAddress]
    [seweg.coders.snmp.SNMP]
    [ber BERUnit]))


(defn get-snmp-channel 
  ([] (udp-socket {:frame (compile-frame SNMP)
                   :buf-size 3000}))
  ([port] (udp-socket {:frame (compile-frame SNMP)
                       :port port 
                       :buf-size 3000})))



(def ^:dynamic *timeout* 2000)


(defn shout 
  "Function \"shouts\" oids to collection of hosts. It openes one
  port through which it sends UDP packets to different targets and
  waits for their response.

  Sort of multicast traffic."
  [hosts & {:keys [community port version oids pdu-type send-interval timeout shout-port] 
            :or {send-interval 5
                 timeout *timeout*}
            :as receiver-options}]
  (let [c (get-snmp-channel)
        template-fn (snmp-template receiver-options)
        packets (map #(merge {:host %} (template-fn (get-new-rid))) hosts)
        result (atom nil)]
    (try
      (receive-all @c #(swap! result 
                              (fn [x] (do
                                        (conj x (hash-map :host (:host %)
                                                          :bindings (get-variable-bindings %)))))))
      (doseq [x packets] (do (Thread/sleep send-interval) (enqueue @c x)))
      @(idle-result timeout @c)
      (catch Exception e nil)
      (finally (close @c)))
    @result))


;; Usefull functions
(defn poke [host community & {:keys [timeout oids]
                              :or {timeout *timeout* 
                                   oids [[1 3 6 1 2 1 1 1 0]]}}]
  (let [get-fn (open-line host community :pdu-type :get-request)
        c (get-snmp-channel)]
    (try 
      (enqueue @c (get-fn oids))
      (let [r (read-channel @c) 
            _ (wait-for-result r timeout)
            vb (get-variable-bindings @r)]
        vb)
      (catch Exception e nil)
      (finally (close @c)))))


(defn snmp-get [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-request :version version)
        c (get-snmp-channel)]
    (try
      (enqueue @c (get-fn oids))
      (let [r (read-channel @c)
            _ (wait-for-result r *timeout*)
            vb (get-variable-bindings @r)]
        vb)
      (catch Exception e nil)
      (finally (close @c)))))

(defn snmp-get-next [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-next-request :version version)
        c (get-snmp-channel)]
    (try
      (enqueue @c (get-fn oids))
      (let [r (read-channel @c)
            _ (wait-for-result r *timeout*)
            vb (get-variable-bindings @r)]
        vb)
      (catch Exception e nil)
      (finally (close @c)))))

(defn snmp-get-first 
  "Returns first valid found value of oids input
  argumetns."
  ([version host community & oids]
   (let [oids (vec (map normalize-oid oids))
         get-fn (open-line host community :pdu-type :get-next-request :version version)
         c (get-snmp-channel)
         transmition-fn (fn [oids]
                          (enqueue @c (get-fn oids))
                          (let [r (read-channel @c)
                                _ (wait-for-result r *timeout*)
                                vb (get-variable-bindings @r)]
                            vb))
         valid-oids (fn [results oids]
                      (let [get-key #(apply key %)]
                        (remove nil?
                                (set (for [r results ok oids :let [rk (get-key r)]]
                                       (let [c (dec (min (count rk) (count ok)))]
                                         (if (= (take c rk) (take c ok)) r)))))))
         ;; Checks if value of returned result is valid...
         checkfn (fn [x] 
                   (let [v (apply val x)]
                     (cond 
                       (coll? v) (seq v)
                       (string? v) (boolean (seq v))
                       :else (boolean v))))]
     (try
       (let [vb-initial (transmition-fn oids)]
         (loop [vb (filter checkfn vb-initial)
                not-found (remove checkfn vb-initial)]
           (when (some (comp not nil?) vb)
             (if (empty? not-found) (sort-by #(apply key %) (valid-oids vb oids))
               (let [new-vb (transmition-fn (map #(apply key %) not-found))
                     found-vb (filter checkfn new-vb)
                     empty-vb (valid-oids (remove checkfn new-vb) oids)]
                 (recur (into vb found-vb) empty-vb))))))
       (catch Exception e nil)
       (finally (close @c))))))


;;(def oids [[1 3 6 1 2 1 1 2] [1 3 6 1 2 1 47 1 1 1 1 11] [1 3 6 1 4 1 2636 3 1 3]])




;;(def checkfn (fn [x] 
;;               (let [v (apply val x)
;;                     k (apply key x)
;;                     c (-> k count dec)
;;                     oids (map #(apply key %) oids)]
;;                 (when (some #(= (subvec k 0 c) (subvec % 0 (min c (-> % count dec)))) oids)
;;                   (cond 
;;                     (coll? v) (do (println 1 (seq v)) (boolean (seq v)))
;;                     (string? v) (do (println 3) (boolean (seq v)))
;;                     :else (do (println 3) (boolean v)))))))


;; (snmp-get-first nil "mzg-dr-11" "spzROh" [1 3 6 1 2 1 1 2] [1 3 6 1 2 1 47 1 1 1 1 11] [1 3 6 1 4 1 2636 3 1 3])

(defn snmp-bulk-get [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-bulk-request :version version)
        c (get-snmp-channel)]
    (try
      (enqueue @c (get-fn oids))
      (let [r (read-channel @c)
            _ (wait-for-result r *timeout*)
            vb (get-variable-bindings @r)]
        vb)
      (catch Exception e nil)
      (finally (close @c)))))




 


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




(comment
  "Example usage"
  (def rst18 (open-line "RST18" "spzROh"))
  (def example-channel (get-snmp-channel))
  (def filtered-channel (filter-line "RST18" @example-channel))
  (receive-all filtered-channel show-variable-bindings)
  (enqueue @example-channel (rst18 [:system])))
