(ns seweg.protocols.snmp.oid-repository
  (:use [clojure.set :only (map-invert)]))

(def ^{:private true} repository 
  {[1] :iso
   [1 3] :identified-organization
   [1 3 6] :dod
   [1 3 6 1] :internet
   [1 3 6 1 2] :mgmt
   [1 3 6 1 2 1] :mib-2
   [1 3 6 1 2 1 4] :ip
   [1 3 6 1 2 1 4 1] :ipForwarding
   [1 3 6 1 2 1 4 2] :ipDefaultTTL
   [1 3 6 1 2 1 4 3] :ipInReceives
   [1 3 6 1 2 1 4 4] :ipInHdrErrors
   [1 3 6 1 2 1 4 5] :ipAddrErrors
   [1 3 6 1 2 1 4 6] :ipForwDatagrams
   [1 3 6 1 2 1 4 7] :ipInUnknownProtos
   [1 3 6 1 2 1 4 8] :ipInDiscards
   [1 3 6 1 2 1 4 9] :ipInDelivers
   [1 3 6 1 2 1 4 10] :ipOutRequests
   [1 3 6 1 2 1 4 11] :ipOutDiscards
   [1 3 6 1 2 1 4 12] :ipOutNoRoutes
   [1 3 6 1 2 1 4 13] :ipReasmTimeout
   [1 3 6 1 2 1 4 14] :ipReasmReqds
   [1 3 6 1 2 1 4 15] :ipReasmOks
   [1 3 6 1 2 1 4 16] :ipReasmFails
   [1 3 6 1 2 1 4 17] :ipFragOks
   [1 3 6 1 2 1 4 18] :ipFragFails
   [1 3 6 1 2 1 4 19] :ipFragCreates 
   [1 3 6 1 2 1 4 20] :ipAddrTable
   [1 3 6 1 2 1 4 20 1] :ipAddrEntry
   [1 3 6 1 2 1 4 20 1 1] :ipAdEntAddr
   [1 3 6 1 2 1 4 20 1 2] :ipAdEntIfIndex
   [1 3 6 1 2 1 4 20 1 3] :ipAdEntNetMask
   [1 3 6 1 2 1 4 20 1 4] :ipAdEntBcastAddr
   [1 3 6 1 2 1 4 20 1 5] :ipAdEntReasmMaxSize 
   [1 3 6 1 2 1 4 21] :ipRouteTable
   [1 3 6 1 2 1 4 21 1] :ipRouteEntry
   [1 3 6 1 2 1 4 21 1 1] :ipRouteDest
   [1 3 6 1 2 1 4 21 1 2] :ipRouteIfIndex
   [1 3 6 1 2 1 4 21 1 3] :ipRouteMetric1
   [1 3 6 1 2 1 4 21 1 4] :ipRouteMetric2
   [1 3 6 1 2 1 4 21 1 5] :ipRouteMetric3
   [1 3 6 1 2 1 4 21 1 6] :ipRouteMetric4
   [1 3 6 1 2 1 4 21 1 7] :ipRouteNextHop
   [1 3 6 1 2 1 4 21 1 8] :ipRouteType
   [1 3 6 1 2 1 4 21 1 9] :ipRouteProto
   [1 3 6 1 2 1 4 21 1 10] :ipRouteAge
   [1 3 6 1 2 1 4 21 1 11] :ipRouteMask
   [1 3 6 1 2 1 4 21 1 12] :ipRouteMetric5
   [1 3 6 1 2 1 4 21 1 13] :ipRouteInfo
   [1 3 6 1 2 1 4 22] :ipNetToMediaTable
   [1 3 6 1 2 1 4 22 1] :ipNetToMediaEntry
   [1 3 6 1 2 1 4 22 1 1] :ipNetToMediaIfIndex
   [1 3 6 1 2 1 4 22 1 2] :ipNetToMediaPhysAddress
   [1 3 6 1 2 1 4 22 1 3] :ipNetToMediaNetAddress
   [1 3 6 1 2 1 4 22 1 4] :ipNetToMediaType
   [1 3 6 1 2 1 17] :dot1dBridge 
   [1 3 6 1 2 1 17 4] :dot1dTp
   [1 3 6 1 2 1 17 4 3] :dot1dTpFdbTable
   [1 3 6 1 2 1 17 4 3 1] :dot1dTpFdbEntry
   [1 3 6 1 2 1 17 4 3 1 1] :dot1dTpFdbAddress
   [1 3 6 1 2 1 17 4 3 1 2] :dot1dTpFdbPort
   [1 3 6 1 2 1 17 4 3 1 3] :dot1dTpFdbStatus
   [1 3 6 1 2 1 47] :entityMIB
   [1 3 6 1 2 1 47 1] :entityMIBObjects
   [1 3 6 1 2 1 47 1 1] :entityPhysical
   [1 3 6 1 2 1 47 1 1 1] :entPhysicalTable
   [1 3 6 1 2 1 47 1 1 1 1] :entPhysicalEntry
   [1 3 6 1 2 1 47 1 1 1 1 1] :entPhysicalIndex
   [1 3 6 1 2 1 47 1 1 1 1 2] :entPhysicalDescr
   [1 3 6 1 2 1 47 1 1 1 1 3] :entPhysicalVendorType
   [1 3 6 1 2 1 47 1 1 1 1 4] :entPhyscialContainedIn
   [1 3 6 1 2 1 47 1 1 1 1 5] :entPhysicalClass
   [1 3 6 1 2 1 47 1 1 1 1 6] :entPhysicalParentRelPos
   [1 3 6 1 2 1 47 1 1 1 1 7] :entPhysicalName
   [1 3 6 1 2 1 47 1 1 1 1 8] :entPhysicalHardwareRev
   [1 3 6 1 2 1 47 1 1 1 1 9] :entPhysicalFirmwareRev
   [1 3 6 1 2 1 47 1 1 1 1 10] :entPhysicalSoftwareRev
   [1 3 6 1 2 1 47 1 1 1 1 11] :entPhysicalSerialNum
   [1 3 6 1 2 1 47 1 1 1 1 12] :entPhysicalMfgName
   [1 3 6 1 2 1 47 1 1 1 1 13] :entPhysicalModelName
   [1 3 6 1 2 1 47 1 1 1 1 14] :entPhysicalAlias
   [1 3 6 1 2 1 47 1 1 1 1 15] :entPhysicalAssetID
   [1 3 6 1 2 1 47 1 1 1 1 16] :entPhysicalsFRU
   [1 3 6 1 2 1 47 1 1 1 1 17] :entPhysicalMfgDate
   [1 3 6 1 2 1 47 1 1 1 1 18] :entPhysicalUris
   [1 3 6 1 2 1 47 2] :entityMIBTraps
   [1 3 6 1 2 1 47 2 0] :entityMIBTrapPrefix
   [1 3 6 1 2 1 47 2 0 1] :entConfigChange
   [1 3 6 1 2 1 1] :system
   [1 3 6 1 2 1 1 1] :sysDescr
   [1 3 6 1 2 1 1 2] :sysObjectID
   [1 3 6 1 2 1 1 3] :sysUpTime
   [1 3 6 1 2 1 1 4] :sysContact
   [1 3 6 1 2 1 1 5] :sysName
   [1 3 6 1 2 1 1 6] :sysLocation
   [1 3 6 1 2 1 1 7] :sysServices
   [1 3 6 1 2 1 1 8] :sysORLastChange
   [1 3 6 1 2 1 1 9] :sysORTable
   [1 3 6 1 2 1 2] :interface
   [1 3 6 1 2 1 2 1] :ifNumber
   [1 3 6 1 2 1 2 2] :ifTable
   [1 3 6 1 2 1 2 2 1] :ifEntry
   [1 3 6 1 2 1 2 2 1 1] :ifIndex
   [1 3 6 1 2 1 2 2 1 2] :ifDescr
   [1 3 6 1 2 1 2 2 1 3] :ifType
   [1 3 6 1 2 1 2 2 1 4] :ifMtu
   [1 3 6 1 2 1 2 2 1 5] :ifSpeed 
   [1 3 6 1 2 1 2 2 1 6] :ifPhysAddress
   [1 3 6 1 2 1 2 2 1 7] :ifAdminStatus
   [1 3 6 1 2 1 2 2 1 8] :ifOperStatus
   [1 3 6 1 2 1 2 2 1 9] :ifLastChange
   [1 3 6 1 2 1 2 2 1 10] :ifInOctets
   [1 3 6 1 2 1 2 2 1 11] :ifInUcastPkts
   [1 3 6 1 2 1 2 2 1 12] :ifInNUcastPkts
   [1 3 6 1 2 1 2 2 1 13] :ifInDiscards
   [1 3 6 1 2 1 2 2 1 14] :ifInErrors
   [1 3 6 1 2 1 2 2 1 15] :ifUnknownProtos
   [1 3 6 1 2 1 2 2 1 16] :ifOutOctets
   [1 3 6 1 2 1 2 2 1 17] :ifOutUcastPkts
   [1 3 6 1 2 1 2 2 1 18] :ifOutNUcastPkts
   [1 3 6 1 2 1 2 2 1 19] :ifOutDiscards
   [1 3 6 1 2 1 2 2 1 20] :ifOutErrors
   [1 3 6 1 2 1 2 2 1 21] :ifOutQLen
   [1 3 6 1 2 1 2 2 1 22] :ifSpecific
   [1 3 6 1 2 1 31 1 1] :ifXTable
   [1 3 6 1 2 1 31 1 1 1] :ifXEntry
   [1 3 6 1 2 1 31 1 1 1 1] :ifName
   [1 3 6 1 2 1 31 1 1 1 2] :ifInMulticastPkts
   [1 3 6 1 2 1 31 1 1 1 3] :ifInBroadcastPkts
   [1 3 6 1 2 1 31 1 1 1 4] :ifOutMulticastPkts
   [1 3 6 1 2 1 31 1 1 1 5] :ifOutBroadcastPkts
   [1 3 6 1 2 1 31 1 1 1 6] :ifHCInOctets
   [1 3 6 1 2 1 31 1 1 1 7] :ifHCInUcastPkts
   [1 3 6 1 2 1 31 1 1 1 8] :ifHCInMulticastPkts
   [1 3 6 1 2 1 31 1 1 1 9] :ifHCInBroadcastPkts
   [1 3 6 1 2 1 31 1 1 1 10] :ifHCOutOctets
   [1 3 6 1 2 1 31 1 1 1 11] :ifHCoutUcastPkts
   [1 3 6 1 2 1 31 1 1 1 12] :ifHCoutMulticastPkts
   [1 3 6 1 2 1 31 1 1 1 13] :ifHCOutBroadcastPkts
   [1 3 6 1 2 1 31 1 1 1 14] :ifLinkUpDownTrapEnable
   [1 3 6 1 2 1 31 1 1 1 15] :ifHighSpeed
   [1 3 6 1 2 1 31 1 1 1 16] :ifPromiscuousMode
   [1 3 6 1 2 1 31 1 1 1 17] :ifConnectorPresent
   [1 3 6 1 2 1 31 1 1 1 18] :ifAlias
   [1 3 6 1 2 1 31 1 1 1 19] :ifCounterDiscountinuityTime
   [1 3 6 1 4 1 25506] :HP
   [1 3 6 1 4 1 9] :Cisco})


(def repository-inv (map-invert repository))

(def known-oids (set (keys repository)))

(defn split-oid 
  "Function takes OID value as vector and returns 
  vector of known OID, as keyword and rest of oid value.
  Function also alows to specify known nuber of OID digits.
  
  Known OID keys are defined in repository."
  ([oid-key] (split-oid oid-key 0))
  ([oid-key n]
   (loop [ko (vec (take n oid-key))
          vo (drop n oid-key)]
     (if-not (contains? repository (conj ko (first vo)))
       [ko vo]
       (recur (conj ko (first vo)) (rest vo))))))



(defn find-oid 
  "Returns OID value. If input argument is a keyword, than
  OID vector is returned and vice versa."
  [oid-key]
  (cond
    (keyword? oid-key) (get repository-inv oid-key)
    (vector? oid-key) (get repository oid-key)
    :else (assert false "Inappropriate input value. Allowed types are keyword and vector")))

 (defn normalize-oid
  "Function allways returns input OID value as vector."
   [oid]
   (if-not (vector? oid) (find-oid oid) oid))

(defn is-child-of-oid? 
  "Test function that examines if OID is child of parent.
  Input values can be keywords or vectors and combination."
  [oid parent]
  (let  [o  (if  (vector? oid) oid (find-oid oid))
         p (if  (vector? parent) parent (find-oid parent))]
    (if (> (count p) (count o)) 
      false 
      (= (take (count p) o) p))))

(defn find-children 
  "Function returns children of OID if it is contained
  in repository. As second argument it is possible to 
  specify output result as sorted or not."
  ([oid-key] (find-children oid-key false))
  ([oid-key ^Boolean sort-children?] (let [o (if (vector? oid-key) oid-key (find-oid oid-key))
                                           r (remove #(= o %) (filter #(= (take (count o) %) o) (keys repository)))]
                                      (when r (if sort-children? (sort r) r)))))

(defn has-children? [oid]
  "Function checks if OID has children or not lazily."
  (boolean (some seq (find-children oid))))

(defn list-oid 
  "Function lists known OID children for easier development."
  [oid-key & opts]
  (if-let [c (find-children oid-key)]
    (doseq [x (map #(hash-map (vec %) (get repository %)) c)] (println x))))

(defn oid2str [oid-key]
  (apply str (interpose "." oid-key)))
