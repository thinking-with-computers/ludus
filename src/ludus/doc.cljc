(ns ludus.doc
 	(:require [ludus.interpreter :as interpreter]
  		[ludus.base :as base]
  		[ludus.data :as data]
  		[clojure.string :as string]))

(def prelude interpreter/ludus-prelude)

(def exports-only (dissoc prelude ::data/type ::data/struct))

(defn map-values [f] (fn [[k v]] [k (f v)]))

(def ludus-doc (base/doc- :body))

(def with-docs (into {} (map (map-values ludus-doc)) exports-only))

(def sorted-names (-> with-docs keys sort))

(defn escape-underscores [the-str] (string/replace the-str #"_" "\\_"))

(defn escape-punctuation [the-str] (string/replace the-str #"[\!\?]" ""))

(defn toc-entry [name] (let [escaped (escape-underscores name)] (str "•[" escaped "](#" (escape-punctuation escaped) ")")))

(def toc (string/join "&nbsp;&nbsp;&nbsp; " (map toc-entry sorted-names)))

(defn compose-entry [name]
 	(let [the-doc (get with-docs name)
      		header (str "### " name "\n")
      		lines (string/split-lines the-doc)]
  		(if (empty? lines) (str header "No documentation available.\n")
    		(let [
          		description (second lines)
          		pattern-lines (subvec lines 2)
          		patterns (string/join "\n" pattern-lines)
          		]
      		(str header "\n" description "```\n" patterns "\n```")
      		))))

(def entries (string/join "\n\n" (map compose-entry sorted-names)))

(def doc-file
 	(str 
    "# Ludus prelude documentation
These functions are available in every Ludus script.
The documentation for any function can be found within Ludus by passing the function to `doc!`, 
e.g., running `doc! (add)` will send the documentation for `add` to the console.

## A few notes
**Naming conventions.** Functions whose name ends with a question mark, e.g., `eq?`, return booleans.
Functions whose name ends with an exclamation point, e.g., `make!`, change state in some way.
In other words, they _do things_ rather than _calculating values_. 
Functions whose name includes a slash either convert from one value to another, e.g. `deg/rad`,
or they are variations on a function, e.g. `div/0` as a variation on `div`.

**How entries are formatted.** Each entry has a brief (sometimes too brief!) description of what it does.
It is followed by the patterns for each of its function clauses.

"
    toc
    "
## Function documentation
"
    entries
    ))

(spit "prelude.md" doc-file)
