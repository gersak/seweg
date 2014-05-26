(ns seweg.asn1.definitions.macros)

(def ^:private test-object-type-file (slurp "mibs/cisco/CISCO-CDP-MIB.my")) 

(defmacro isolate-macro [macro]
  (let [fn-name (symbol (str "isolate-" macro))]
    `(defn- ~fn-name [^String text# ^String object-name#]
       (let [pattern# (str "(?s)" object-name# "\\s+" ~macro ".*?::=\\s*\\{.*?\\}")]
         (re-find (re-pattern pattern#) text#)))))

(isolate-macro "OBJECT-IDENTITY")
(isolate-macro "OBJECT-TYPE")
(isolate-macro "NOTIFICATION-TYPE")
(isolate-macro "OBJECT-GROUP")
(isolate-macro "NOTIFICATION-GROUP")
(isolate-macro "MODULE-COMPLIANCE")
(isolate-macro "AGENT-CAPABILITIES")
(isolate-macro "TRAP-TYPE")

(defmacro make-extractor [macro]
  (let [fn-name (symbol (str "extract-" (clojure.string/replace macro #"\s+" "_")))]
    `(defn- ~fn-name [^String text#]
       (let [pattern# (str "(?s)[A-Za-z0-9-]+\\s+" ~macro ".*?::=\\s*\\{.*?\\}")]
         (re-seq (re-pattern pattern#) text#)))))

(make-extractor "OBJECT-IDENTITY")
(make-extractor "OBJECT-TYPE")
(make-extractor "NOTIFICATION-TYPE")
(make-extractor "OBJECT-GROUP")
(make-extractor "NOTIFICATION-GROUP")
(make-extractor "MODULE-COMPLIANCE")
(make-extractor "AGENT-CAPABILITIES")
(make-extractor "TRAP-TYPE")

(defn extract-OBJECT_IDENTIFIER [^String text]
  (let [pattern #"(?s)[a-z][a-zA-Z][\w\d-]+\s+OBJECT\s+IDENTIFIER\s+::=\s*\{.*?\}"]
    (re-seq pattern text)))
