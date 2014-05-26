(ns seweg.asn1.definitions.module
  (:require [seweg.asn1.common :as common :refer [remove-comments]]))

(def module-imports-sample (slurp "./mibs/ietf/IF-MIB"))

(defn get-module-name 
  "For input text function returns module name in
  string form if found"
  [text]
  (first (clojure.string/split (clojure.string/trim (re-find #"(?s)\s*.*?\s+DEFINITIONS\s*::=\s*BEGIN" text)) #"\s")))

(defn get-module-imports 
  "For given ASN module definition function
  returns object names required for definition of
  types, mapped to module name"
  [text]
  (let [isolated-import (re-find #"(?s)\s*IMPORTS.*?\;" text)
        clean-imports (clojure.string/replace isolated-import #"(IMPORTS|;)" "")
        separated-imports (map clojure.string/trim (re-seq #"(?s)\s*.*?FROM.*?\n" clean-imports))
        imports (for [x separated-imports :let [[symbols module] (clojure.string/split x #"FROM")]]
                  {:references (map clojure.string/trim (clojure.string/split symbols #","))
                   :module (clojure.string/trim module)})]
    imports))
