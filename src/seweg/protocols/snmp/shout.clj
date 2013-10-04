(ns seweg.protocols.snmp.shout
  (:use [seweg.protocols.snmp :only (snmp-template get-snmp-channel get-variable-bindings get-new-rid open-line)]
        lamina.core))




(defn shout 
  "Function \"shouts\" oids to collection of hosts. It openes one
  port through which it sends UDP packets to different targets and
  waits for their response.

  Sort of multicast traffic."
  [hosts & {:keys [community port version oids pdu-type send-interval timeout shout-port] 
            :or {send-interval 5
                 timeout 2000}
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


(def oids  [[1 3 6 1 2 1 1 2 0]  [1 3 6 1 2 1 47 1 1 1 1 11 1]])

(def hosts (read-string (slurp "test-hosts.txt")))
