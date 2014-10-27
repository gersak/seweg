(ns seweg.client.simple
  (:require 
    [clojure.core.async :refer [go <!! go-loop take! put! alts!! chan]]
    [seweg.protocols.snmp :as s :refer [compose-snmp-packet
                                        decompose-snmp-response
                                        open-line
                                        get-variable-bindings
                                        snmp-template
                                        get-new-rid
                                        make-table]]
    [seweg.protocols.snmp.oid-repository :refer [normalize-oid is-child-of-oid? find-oid]]
    [seweg.protocols.snmp.values-repository :refer [get-known-value]]
    [seweg.coders.snmp :refer [snmp-encode snmp-decode]])
  (:import 
    [java.net
     DatagramPacket
     DatagramSocket
     InetAddress]
    [seweg.coders.snmp.SNMP]
    [ber BERUnit]))


(def ^{:dynamic true} *timeout* 2000)

(def ^{:dynamic true} *receive-packet-size* 2000)

(defn- make-udp-socket 
  ([] (doto (DatagramSocket.) (.setSoTimeout *timeout*))) 
  ([port] (doto (DatagramSocket.) (.setSoTimeout *timeout*) (.setPort port))) 
  ([port timeout] (doto (DatagramSocket.) (.setSoTimeout timeout) (.setPort port))))

(defn- generate-udp-packet 
  ([^bytes byte-seq ^String host] (generate-udp-packet byte-seq host 161))
  ([^bytes byte-seq ^String host port]
   (DatagramPacket. byte-seq (count byte-seq) (InetAddress/getByName host) port)))


(defn- generate-blank-packet
  ([] (generate-blank-packet *receive-packet-size*))
  ([size] (DatagramPacket. (byte-array size) (int size))))

(defn- send-sync [#^DatagramSocket client ^DatagramPacket packet]
  (let [rp (generate-blank-packet)]
    (.send client packet)
    (.receive client rp)
    {:host (.getHostAddress (.getAddress rp)) 
     :port (.getPort rp) 
     :message (snmp-decode (.getData rp))}))

(defn- proccess-udp [{:keys [message host port timeout] 
                      :as prepared-packet 
                      :or {port 161 timeout *timeout*}}]
  (when-let [client (make-udp-socket)]
    (try
      (let [byte-seq (snmp-encode message)
            packet (generate-udp-packet byte-seq host)]
        (send-sync client packet))
      (catch Exception e (.printStackTrace e))
      (finally (.close client)))))


(defn poke [host community & {:keys [timeout oids]
                              :or {timeout *timeout* 
                                   oids [[1 3 6 1 2 1 1 1 0]]}}]
  (let [l (open-line host community :pdu-type :get-request)]
    (-> (l oids) proccess-udp get-variable-bindings)))

(defn snmp-get [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-request :version version)]
    (-> (get-fn oids) proccess-udp get-variable-bindings)))

(defn snmp-get-next [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-next-request :version version)]
    (-> (get-fn oids) proccess-udp get-variable-bindings)))


(defn snmp-get-first 
  "Returns first valid found value of oids input
  argumetns."
  ([version host community & oids]
   (when-let [client (make-udp-socket)]
     (let [oids (vec (map normalize-oid oids))
           get-fn (open-line host community :pdu-type :get-next-request :version version)
           transmition-fn (fn [oids]
                            (let [ld (get-fn oids)
                                  p (generate-udp-packet (snmp-encode (:message ld)) host)]
                              (get-variable-bindings (send-sync client p))))
           valid-oids (fn [results oids]
                        (let [get-key #(apply key %)]
                          (remove nil?
                                  (set (for [r results ok oids :let [rk (get-key r)]]
                                         (let [c (dec (min (count rk) (count ok)))]
                                           (if (= (take c rk) (take c ok)) r)))))))
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
               (if (empty? not-found)
                 (sort-by #(apply key %) (valid-oids vb oids))
                 (let [new-vb (transmition-fn (map #(apply key %) not-found))
                       found-vb (filter checkfn new-vb)
                       empty-vb (valid-oids (remove checkfn new-vb) oids)]
                   (recur (into vb found-vb) empty-vb))))))
         (catch Exception e nil)
         (finally (.close client)))))))


(defn snmp-bulk-get [version host community & oids]
  (let [oids (vec oids)
        get-fn (open-line host community :pdu-type :get-bulk-request :version version)]
    (-> (get-fn oids) proccess-udp get-variable-bindings)))



;; Walking functions
(defn snmp-bulk-walk
  "Function tries to walk OID tree as far as response
  contains input oid value. If it fails or times out
  function will return nil. Input OIDs accepts vector
  of OID values. Either as keywords or vectors.

  Default timeout is 2s. "
  ([host community oids] (snmp-bulk-walk host community oids 2000))
  ([host community oids timeout]
   (when-let [c (make-udp-socket)]
     (try
       (let [oids (if (coll? oids) (map normalize-oid oids)
                    (list (normalize-oid oids)))
             bulk-fn (open-line host community :pdu-type :get-bulk-request)]
         (letfn [(valid-vb? [vb] 
                   (let [vb-oid (-> vb first key)] 
                     (some #(is-child-of-oid? vb-oid %) oids)))
                 (send-fn [oid]
                   (.send c (generate-udp-packet (snmp-encode 
                                                   (:message
                                                     (bulk-fn [oid]))) host)))
                 (receive-fn []
                   (let [p (generate-blank-packet)]
                     (.receive c p)
                     {:host (.getHostAddress (.getAddress p)) 
                      :port (.getPort p) 
                      :message (snmp-decode (.getData p))}))]
           (doseq [x oids] (send-fn x))
           (loop [r []]
             (let [p (receive-fn)
                   vb (get-variable-bindings p)
                   last-oid (-> vb last keys first)]
               (if (valid-vb? (last vb))
                 (do
                   (send-fn last-oid)
                   (recur (into r (filter valid-vb? vb))))
                 (into r (filter valid-vb? vb)))))))
       (catch Exception e nil)
       ;(catch Exception e (.printStackTrace e))
       (finally (.close c))))))


(defn snmp-walk 
  ([host community oids] (snmp-walk host community oids 1000))
  ([host community oids timeout]
   (when-let [c (make-udp-socket)]
     (try
       (let [oids (if (coll? oids) (map normalize-oid oids)
                    (list (normalize-oid oids)))
             next-fn (open-line host community :pdu-type :get-bulk-request)]
         (letfn [(valid-vb? [vb] 
                   (let [vb-oid (-> vb first key)] 
                     (some #(is-child-of-oid? vb-oid %) oids)))
                 (send-fn [oid]
                   (.send c (generate-udp-packet (snmp-encode 
                                                   (:message
                                                     (next-fn [oid]))) host)))
                 (receive-fn []
                   (let [p (generate-blank-packet)]
                     (.receive c p)
                     {:host (.getHostAddress (.getAddress p)) 
                      :port (.getPort p) 
                      :message (snmp-decode (.getData p))}))]
           (doseq [x oids] (send-fn x))
           (loop [r []]
             (let [p (receive-fn)
                   vb (get-variable-bindings p)
                   last-oid (-> vb last keys first)]
               (if (valid-vb? (last vb))
                 (do
                   (send-fn last-oid)
                   (recur (into r (filter valid-vb? vb))))
                 (into r (filter valid-vb? vb)))))))
       (catch Exception e nil)
       ;(catch Exception e (.printStackTrace e))
       (finally (.close c))))))
