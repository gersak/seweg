(defproject seweg "0.1.2"
  :description "SNMP library for Clojure"
  :url "http://example.com/FIXME"
  :aot [seweg.protocols.netconf.TransportSSH
        seweg.coders.snmp]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["src/java"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [frak "0.1.3"]
                 [aleph "0.3.0"]
                 [lamina "0.5.0"]
                 [clj-ssh "0.5.6"]
                 [com.jcraft/jsch "0.1.50"]
                 [com.taoensso/timbre "2.6.2"]
                 [net.i2cat.netconf/netconf4j "0.0.4"]
                 [clj-ssh "0.5.6"]
                 [me.raynes/fs "1.4.5"]
                 [org.clojure/math.numeric-tower "0.0.2"]])
