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

(defn toc-entry [name] (let [escaped (escape-underscores name)] (str "[" escaped "](#" (escape-punctuation escaped) ")")))

(def alphabetical-list (string/join "&nbsp;&nbsp;&nbsp; " (map toc-entry sorted-names)))

(def topics {
            	"math" ["abs" "add" "angle" "atan/2" "ceil" "cos" "dec" "deg/rad" "deg/turn" "dist" "div"
                   		"div/0" "div/safe" "even?" "floor" "gt?" "gte?" "heading/vector" "inc" "lt?" "lte?" "mod"
                   		"mult" "neg" "neg?" "odd?" "pi" "pos?" "rad/deg" "rad/turn" "random" "range" "round"
                   		"sin" "square" "sub" "sum_of_squares" "tan" "tau" "turn/deg" "turn/rad" "zero?"]
            	"boolean" ["and" "bool" "bool?" "false?" "or" "not"]
            	"dicts" ["assoc" "assoc?" "dict" "diff" "dissoc" "get" "keys" "update" "values"]
            	"lists" ["append" "at" "butlast" "concat" "count" "each!" "first" "fold" "last" "list" "list?" "map" "ordered?" "range"
                    		"rest" "second" "slice"]
             "sets" ["set" "set?"]
            	"strings" ["count" "join" "show" "string" "string?"]
            	"types and values" ["bool?" "coll?" "dict?" "eq?" "fn?" "keyword?" "list?" "neq?" "nil?" "number?" "ordered?" "show" "some" "some?" "type"]
            	"references and state" ["deref" "make!" "update!"]
            	"results" ["err" "err?" "ok" "ok?" "unwrap!" "unwrap_or"]
            	"errors" ["assert!" "panic!"]
            	"turtle graphics" ["back!" "background!" "bk!" "clear!" "fd!" "forward!" "goto!" "heading" "heading/vector" "home!" "left!" "lt!" "pc!" "pd!" "pencolor" 
                                "pencolor!" "pendown!" "pendown?" "penup!" "penwidth" "penwidth!" "position" "pu!" "pw!" "render_turtle!" "reset_turtle!" 
                                "right!" "rt!" "turtle_state"]
            	"environment and i/o" ["doc!" "flush!" "print!" "prn!" "report!"]
            	})

(defn topic-entry [topic] (str "### " (string/capitalize topic) "\n" 
                            (->> topic (get topics) sort (map toc-entry) (string/join "&nbsp;&nbsp;&nbsp; "))
                            "\n"))

(def by-topic (let [the-topics (-> topics keys sort)
                    topics-entries (map topic-entry the-topics)]
                (string/join "\n" topics-entries)))

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
      		(str header description "\n```\n" patterns "\n```")
      		))))

(def entries (string/join "\n\n" (map compose-entry sorted-names)))

(def doc-file
 	(str 
    "# Ludus prelude documentation
These functions are available in every Ludus script.
The documentation for any function can be found within Ludus by passing the function to `doc!`, 
e.g., running `doc! (add)` will send the documentation for `add` to the console.

For more information on the syntax & semantics of the Ludus language, see [language.md](./language.md).

## A few notes
**Naming conventions.** Functions whose name ends with a question mark, e.g., `eq?`, return booleans.
Functions whose name ends with an exclamation point, e.g., `make!`, change state in some way.
In other words, they _do things_ rather than _calculating values_. 
Functions whose name includes a slash either convert from one value to another, e.g. `deg/rad`,
or they are variations on a function, e.g. `div/0` as a variation on `div`.

**How entries are formatted.** Each entry has a brief (sometimes too brief!) description of what it does.
It is followed by the patterns for each of its function clauses.
This should be enough to indicate order of arguments, types, and so on.

**Patterns often, but do not always, indicate types.** Typed patterns are written as `foo as :bar`,
where the type is indicated by the keyword. 
Possible ludus types are: `:nil`, `:boolean`, `:number`, `:keyword` (atomic values);
`:string` (strings are their own beast); `:tuple` and `:list` (indexed collections); `:set` (sets are specific), 
`:dict` and `:ns` (associative collections); and `:ref` (references).

**Conventional types.** Ludus has two types based on conventions.
* _Result tuples._ Results are a way of modeling the result of a calculation that might fail.
The two possible values are `(:ok, value)` and `(:err, msg)`.
`msg` is usually a string describing what went wrong.
To work with result tuples, see [`unwrap!`](#unwrap) and [`unwrap_or`](#unwrap_or).
That said, usually you work with these using pattern matching.

* _Vectors._ Vectors are 2-element tuples of x and y coordinates.
The origin is `(0, 0)`. 
Many math functions take vectors as well as numbers, e.g., `add` and `mult`.
You will see vectors indicated in patterns by an `(x, y)` tuple.
You can see what this looks like in the last clause of `add`: `((x1, y1), (x2, y2))`.

## Functions by topic

"
    by-topic
    
    "
    
## All functions, alphabetically
"
    alphabetical-list
    "
## Function documentation
"
    entries
    ))

(spit "prelude.md" doc-file)
