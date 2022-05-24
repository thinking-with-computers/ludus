(ns ludus.loader
	(:require [babashka.fs :as fs]))

(defn cwd [] (fs/cwd))

(defn load-import
	([file] (-> file (fs/canonicalize) (fs/file) (slurp)))
	([file from]
		(load-import 
		(fs/path
			(fs/parent (fs/canonicalize from))
			(fs/path file)))))
