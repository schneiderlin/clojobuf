(ns clojobuf.extend
  (:require
   [clojure.core :refer [tap>]]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.walk :refer [prewalk]]
   [com.rpl.specter :refer [ALL LAST select]]
   [instaparse.core :refer [failure? get-failure]]
   [instaparse.failure :refer [augment-failure]]
   [rubberbuf.ast-preprocess :as preprocess]
   [rubberbuf.ast-util :refer [import?]]
   [rubberbuf.parse :refer [parse]]
   [rubberbuf.util :refer [raise]]))

(defn- update-meta [ast filename]
  (prewalk
   (fn [node] (if-let [m (meta node)]
                (->> (assoc m :instaparse.gll/file filename)
                     (with-meta node))
                node))
   ast))

(defn- parse- [pb-text filename]
  (let [ast (parse pb-text)]
    (if (failure? ast)
      (raise (str filename ": " (-> ast get-failure (augment-failure pb-text) print with-out-str)))
      (update-meta ast filename))))

(defn- protoc-
  [resource-paths auto-import registry]
  (let [asts (for [resource-path resource-paths]
               (do (tap> (str "Compiling " resource-path))
                   (let [pb-text (try
                                   (slurp (io/resource resource-path))
                                   (catch Exception _
                                     (raise (str "Resource not found: " resource-path))))]
                     (parse- pb-text resource-path))))
        registry (conj registry (zipmap resource-paths asts))
        imports #(select [ALL ALL import? LAST] asts) ; wrap in lambda to defer execution
        new-files (if (true? auto-import)
                    (set/difference (set (imports)) (set (keys registry)))
                    [])]
    (if (empty? new-files) registry
        (protoc- new-files auto-import registry)))) ; recur

(defn protoc 
  [resource-paths & {:keys [auto-import normalize] :or {auto-import true normalize true}}]
  (let [rast (protoc- resource-paths auto-import {})]
    (if (true? normalize)
      (do
        (tap> "Normalizing registry of AST")
        (preprocess/normalize rast))
      rast)))