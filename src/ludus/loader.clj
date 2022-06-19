(ns ludus.loader
  (:require [babashka.fs :as fs]))

(defn cwd [] (fs/cwd))

(defn load-import
  ([file]
   (let [path (-> file (fs/canonicalize) (fs/file))]
     (try (slurp path)
          (catch java.io.FileNotFoundException _
            (throw (ex-info (str "File " path " not found") {:path path ::error true}))))))
  ([file from]
   (load-import
    (fs/path
     (if (= from :cwd) (fs/cwd) (fs/parent (fs/canonicalize from)))
     (fs/path file)))))
