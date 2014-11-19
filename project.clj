(defproject seweg "0.1.6"
  :description "SNMP library for Clojure"
  :url "http://example.com/FIXME"
  :aot :all
  ;:aot [seweg.protocols.netconf.TransportSSH
  ;      seweg.coders.snmp]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.7.0-alpha3"]
                 [com.taoensso/timbre "3.3.1"]
                 [frak "0.1.6"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [clojure.network "0.1.0-beta3"]
                 [aleph "0.3.3"]
                 [byte-streams "0.1.13"]
                 [org.clojure/core.unify "0.5.6"]
                 [org.clojure/algo.monads "0.1.5"]])
