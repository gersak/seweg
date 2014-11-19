(ns asn1.basic-types
  (:require [seweg.asn1.definitions.constructors :refer :all]
            [seweg.asn1.definitions.extractors :refer [split-asn-elements]]
            [clojure.test :refer :all]))

;; CONSTRAINS


;; INTEGER
(def INTEGER_SINGLE_VALUE_CONSTRAINS
  (split-asn-elements 
    "{ monday(0), tuesday(1), wednesday(2), thursday(3), friday(4), saturday(5), sunday(6) }"))

(def INTEGER_SET_VALUE_CONSTRAINS
  (split-asn-elements 
    "(1|2|3|10|11|12)"))

(def INTEGER_RANGE_CONSTRAINS
  (split-asn-elements "(10..20) "))

(def INTEGER_NO_CONSTRAINS
  (split-asn-elements "Typo ::= INTEGER (10..20)"))

(deftest INTEGER_CONSTRAINS
  (is (= ['() {:constrain-type :value-constrains
               :value-constrains {6 "sunday", 5 "saturday", 4 "friday", 3 "thursday", 2 "wednesday", 1 "tuesday", 0 "monday"}
               }] 
         (INTEGER [INTEGER_SINGLE_VALUE_CONSTRAINS {}])))
  (is (= ['() {:constrain-type :number-set
               :number-set #{1 2 3 10 11 12}}]
         (INTEGER [INTEGER_SET_VALUE_CONSTRAINS {}])))
  (is (= ['() {:constrain-type :range
               :range [10 20]}]
         (INTEGER [INTEGER_RANGE_CONSTRAINS {}])))
  (is (= ['("Typo" "::=" "INTEGER" "(" "10" ".." "20" ")") {}]
         (INTEGER [INTEGER_NO_CONSTRAINS {}]))))





;; SEQUENCE 

(def SEQUENCE_TEST_SET
  "{
  ifName                  DisplayString,
  ifInMulticastPkts       Counter32,
  ifInBroadcastPkts       Counter32,
  ifOutMulticastPkts      Counter32,
  ifOutBroadcastPkts      Counter32,
  ifHCInOctets            Counter64,
  ifHCInUcastPkts         Counter64,
  ifHCInMulticastPkts     Counter64,


  ifHCInBroadcastPkts     Counter64,
  ifHCOutOctets           Counter64,
  ifHCOutUcastPkts        Counter64,
  ifHCOutMulticastPkts    Counter64,
  ifHCOutBroadcastPkts    Counter64,
  ifLinkUpDownTrapEnable  INTEGER,
  ifHighSpeed             Gauge32,
  ifPromiscuousMode       TruthValue,
  ifConnectorPresent      TruthValue,
  ifAlias                 DisplayString,
  ifCounterDiscontinuityTime TimeStamp }")
