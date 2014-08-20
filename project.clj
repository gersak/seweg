(defproject seweg "0.1.2"
  :description "SNMP library for Clojure"
  :url "http://example.com/FIXME"
  :aot [seweg.protocols.netconf.TransportSSH
        seweg.coders.snmp]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [kovacnica/dreamcatcher "1.0.3"]
                 [frak "0.1.6"]
                 [aleph "0.3.2"]
                 [lamina "0.5.2"]
                 [byte-streams "0.1.13"]
                 [org.clojure/core.unify "0.5.5"]
                 [clj-ssh "0.5.10"]
                 [org.clojure/algo.monads "0.1.5"]
                 [me.raynes/fs "1.4.6"]
                 [clj-tuple "0.1.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]])
