(ns seweg.asn1.modules
  (:require frak
            [me.raynes.fs :as fs]))


(defn- get-module-name [text]
  (re-find #"(?<=\s*).*(?=\s+DEFINITIONS)" text))

(defn- get-module-body [text]
  (re-find #"(?s)(?<=BEGIN).*(?=END)" text))

(defn- remove-comments 
  "Function removes comments from
  ASN1 input text string"
  [text]
  (clojure.string/replace text #"--.*(?=\n)" ""))

(defn- get-module-imports [body]
  (let [import-body (apply str (take-while #(not= % \;) (re-find #"(?s)(?<=IMPORTS).*" body)))
        froms (map clojure.string/trim (re-seq #"(?<=FROM).*(?=\n*)" import-body))
        froms-pattern (frak/pattern froms)
        clean-imports (map #(clojure.string/replace % #"\s+FROM\s+" "") (clojure.string/split import-body froms-pattern))
        clean-imports (map #(-> % clojure.string/trim-newline clojure.string/trim) clean-imports)
        import-names (map #(map clojure.string/trim (clojure.string/split % #",")) clean-imports)] 
   (apply hash-map (interleave froms import-names))))


;; Definitions 
(defonce oids (ref {:iso [1]}))
(defonce objects-info (ref {}))



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
  (memoize (fn [text pattern]
             (let [text (vec text)
                   found-lines (set (filter #(seq (re-find (re-pattern pattern) %)) text))]
               (vec
                 (remove nil?
                         (for [x (range (count text))]
                           (when (found-lines (text x)) x)))))))) 

(defn- type-definitions
  "Function returns parsed sections 
  of definitions in form of String.
  Definitions are sequence of strings
  delimited by start-string pattern 
  and end-string pattern"
  [text start-string end-string]
  (let [text (clojure.string/split-lines text)
        text (vec (filter seq text))
        type-positions (find-positions text start-string)
        delimiter-positions (find-positions text end-string)
        sections (loop [types type-positions
                        delimiters delimiter-positions
                        sections []]
                   (if-not (seq types) sections
                     (let [new-delimiters (drop-while #(> (first types) %) delimiters)]
                       (if (= (first types) (first new-delimiters))
                         (recur (rest types) (rest new-delimiters) (conj sections [(text (first types))]))
                         (recur (rest types) (rest new-delimiters) (conj sections (subvec text (first types) (inc (first new-delimiters)))))))))]
    (doall (map #(apply str (interpose "\n" %)) sections))))

(defn- add-new-oid 
  "Adds new oid to oids variable"
  [new-oid]
  (if (every? number? (val new-oid))
    (dosync (alter oids conj new-oid))
    (let [last-valid-oid (first (drop-while number? (-> new-oid val reverse)))
          tail (take-while number? (-> new-oid val reverse))]
      (dosync
        (alter oids assoc (key new-oid) (into (get @oids last-valid-oid) tail))))))

(defn- add-new-oids [oids]
  (let [new-oids (atom oids)]
    (println (count oids) " parsed OIDS")
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


(def test-dirs "./mibs/mibs/ietf/" "./mibs/mibs/cisco/")

(defn- import-dir [& dirs]
  (when-let [definitions (apply str 
                                (interpose "\n" 
                                           (flatten 
                                             (for [d dirs :let [files (fs/list-dir d)]]
                                               (for [x files] (slurp (str d x)))))))]
    (let [pure-oid-agent (agent definitions)
          object-type-agent (agent definitions)
          module-identity-agent (agent definitions)]
      (send-off pure-oid-agent get-pure-oids-data)
      (send-off object-type-agent get-object-types-data)
      (send-off module-identity-agent get-module-identity-data)
      (await pure-oid-agent object-type-agent module-identity-agent)
      (let [all-oids (apply merge (map #(-> % deref :oids) [pure-oid-agent object-type-agent module-identity-agent]))]
        (add-new-oids all-oids)
        all-oids))))
