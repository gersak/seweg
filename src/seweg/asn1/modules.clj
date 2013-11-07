(ns seweg.asn1.modules
  (:require frak
            [me.raynes.fs :as fs]
            [taoensso.timbre :as timbre :refer (debug)]))


(defn- get-module-name [text]
  (re-find #"(?<=\s*).*(?=\s+DEFINITIONS)" text))

(defn- get-module-body [text]
  (re-find #"(?s)(?<=BEGIN).*(?=END)" text))

(defn- remove-comments 
  "Function removes comments from
  ASN1 input text string"
  [text]
  (when (seq text) (clojure.string/replace text #"--.*(?=\n)" "")))

(defn- get-module-imports [body]
  (let [import-body (apply str (take-while #(not= % \;) (re-find #"(?s)(?<=IMPORTS).*" body)))
        froms (map clojure.string/trim (re-seq #"(?<=FROM).*(?=\n*)" import-body))
        froms-pattern (frak/pattern froms)
        clean-imports (map #(clojure.string/replace % #"\s+FROM\s+" "") (clojure.string/split import-body froms-pattern))
        clean-imports (map #(-> % clojure.string/trim-newline clojure.string/trim) clean-imports)
        import-names (map #(map clojure.string/trim (clojure.string/split % #",")) clean-imports)] 
    (apply hash-map (interleave froms import-names))))


;; Definitions 
(def default-oid-file "./resources/default.oids")
(def default-info-file "./resources/default.info")
(defonce oids (ref (read-string (slurp default-oid-file))))
(defonce info (ref {}))

;; Initialize OIDS

(defn known-oid?  
  "Function returns true if last 
  OID is known to compiler"
  [oid]
  (contains? @oids oid))

(defn- get-known-oids 
  [new-oids]
  "Returns OIDS that are known"
  (filter #(if (every? number? %)
                     true
                     (known-oid? (first (drop-while number? (-> % val reverse))))) new-oids))

(def ^:private find-positions
  "Function returns sequence of positions
  inside of text that match pattern string.

  text is sequence of strings"
  ;(memoize (fn [text pattern]
  (fn [text pattern]
             (let [text (vec text)
                   found-lines (set (filter #(seq (re-find (re-pattern pattern) %)) text))]
               (vec
                 (remove nil?
                         (for [x (range (count text))]
                           (when (found-lines (text x)) x)))))))

(defn- type-definitions
  "Function returns parsed sections 
  of definitions in form of String.
  Definitions are sequence of strings
  delimited by start-string pattern 
  and end-string pattern"
  [text start-string end-string]
  (let [text (when text (clojure.string/split-lines text))
        text (vec (filter seq text))
        type-positions (find-positions text start-string)
        delimiter-positions (find-positions text end-string)]
    ;(println type-positions delimiter-positions)
    (when (and (seq type-positions) (seq delimiter-positions))
      (let [sections (loop [types type-positions
                            delimiters delimiter-positions
                            sections []]
                       ;;(-> sections last println) 
                       (if-not (seq types) sections
                         (let [new-delimiters (drop-while #(> (first types) %) delimiters)]
                           (if (= (first types) (first new-delimiters))
                             (recur (rest types) (rest new-delimiters) (conj sections [(text (first types))]))
                             (recur (rest types) (rest new-delimiters) (conj sections (subvec text (first types) (inc (first new-delimiters)))))))))]
        (doall (map #(apply str (interpose "\n" %)) sections))))))

(defn- normalize-oid-mappings [oid-keys oid-values]
  (when (seq oid-keys)
    (reduce conj (map #(apply hash-map %) (partition 2 (interleave oid-keys oid-values))))))


(defn- normalize-oid-info [infos]
  (when (seq infos)
    (reduce conj infos)))

(defn- add-new-oid 
  "Adds new oid to oids variable"
  [new-oid]
  (if (every? number? (val new-oid))
    (dosync (alter oids conj new-oid))
    (let [last-valid-oid (first (drop-while number? (-> new-oid val reverse)))
          tail (take-while number? (-> new-oid val reverse))]
      (dosync
        (alter oids assoc (key new-oid) (into (get @oids last-valid-oid) tail))))))

(defn- add-new-oids [new-oids]
  (let [new-oids (atom (reduce conj {} (remove #(contains? @oids (key %)) new-oids)))]
    (println (count @new-oids) " parsed new OIDS")
    (while (and (-> @new-oids get-known-oids seq boolean) (-> @new-oids seq boolean))
      (doseq [x (get-known-oids @new-oids)]
        (println "Adding : " x " OID")
        (add-new-oid x)
        (swap! new-oids dissoc (key x))))))

(defn- merge-all-files 
  "Merges all files into one big
  string."
  [dir]
  (let [files (fs/list-dir dir)]
    (when (seq files)
      (let [definitions (apply str 
                               (interpose "\n"
                                          (for [x files]
                                            (slurp (str dir x)))))]
        definitions))))


(def var-name-pattern "^\\w+[-_]*\\d*\\w*")

;; Other files
(load "pure_oids")
(load "object_type")
(load "module_identity")
(load "modules/doc")


(def test-dirs "./mibs/mibs/ietf/" "./mibs/mibs/cisco/")

(declare synchronize-seweg-repository)

(defn import-dir [& dirs]
  (when-let [definitions (flatten 
                           (for [d dirs :let [files (fs/list-dir d)]]
                             (for [x files] {:file x
                                             :content (slurp (str d  x))})))]
    (let [pure-oid-data (for [x definitions]
                          (do 
                            (debug "Extracting OBJECT IDENTIFIER from: " (:file x))
                            (-> x :content get-pure-oids-data)))
          object-type-data (for [x definitions]
                             (do
                               (debug "Extracting OBJECT-TYPE from: " (:file x))
                               (-> x :content get-object-types-data )))
          module-identity-data (for [x definitions]
                                 (do
                                   (debug "Extracting MODULE-IDENTITY from: " (:file x))
                                   (-> x :content get-module-identity-data)))]
      (let [all-oids (apply merge (map :oids (flatten [pure-oid-data object-type-data module-identity-data])))
            all-info (apply merge (map :info (flatten [pure-oid-data object-type-data module-identity-data])))]
        (add-new-oids all-oids)
        (dosync (alter info merge all-info))
        (debug
          (-> (map :oids pure-oid-data) flatten count) " pure OIDs extracted\n"
          (-> (map :oids pure-oid-data) flatten count) " OBJECT-TYPES extracted\n"
          (-> (map :oids pure-oid-data) flatten count) " MODULE-IDENTITY extracted\n")
        (synchronize-seweg-repository)
        (remove #(contains? @oids (key %)) all-oids)))))

(defn synchronize-seweg-repository []
  (intern 'seweg.protocols.snmp.oid-repository 'repository-inv @oids)
  (intern 'seweg.protocols.snmp.oid-repository 'repository (clojure.set/map-invert @oids)))

(defn- export-default-oids [oids]
  (spit default-oid-file oids))

(defn- export-default-info [info]
  (spit default-info-file info))
