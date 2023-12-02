& this file, uniquely, gets `base` loaded as context. See src/ludus/base.cljc for exports

fn rest {
	"Returns all but the first element of a list, as a list."
	(xs as :list) -> base :rest (xs) 
}

fn inc {
	"Increments a number."
	(x as :number) -> base :inc (x)
}

fn dec {
	"Decrements a number."
	(x as :number) -> base :dec (x)
	(x) -> report (x)
}

fn nth {
	"Returns the element at index n of a list or tuple. Zero-indexed: the first element is at index 0."
	(xs as :list, n as :number) -> base :nth (xs, inc(n))
	(xs as :tuple, n as :number) -> base :nth (xs, inc (n))
}

fn first {
	"Returns the first element of a list or tuple."
	(xs) -> nth (xs, 0)
}

fn second {
	"Returns the second element of a list or tuple."
	(xs) -> nth (xs, 1)
}

fn count {
	"Returns the number of elements in a collection (including string)."
	(xs as :list) -> dec (base :count (report(xs)))
	(xs as :tuple) -> dec (base :count (xs))
	(xs as :dict) -> base :count (xs)
	(xs as :string) -> base :count (xs)
	(xs as :set) -> base :count (xs)
	(xs as :struct) -> dec (base :count (xs))
}

fn list {
	"Takes a tuple, and returns it as a list."
	(xs as :tuple) -> base :into ([], base :rest (xs))
}

fn fold {
	"Folds a list."
	(f as :fn, xs as :list) -> fold (f, xs, f ())
	(f as :fn, xs as :list, root) -> loop (root, first (xs), rest (xs)) with {
		(prev, curr, []) -> f (prev, curr)
		(prev, curr, remaining) -> recur (
			f (prev, curr)
			first (remaining)
			rest (remaining)
		)
	}
}

fn map {
	"Maps over a list."
	(f as :fn, xs) -> {
		fn mapper (prev, curr) -> conj (prev, f (curr))
		fold (mapper, xs, [])
	}
}

fn conj {
	"Adds an element to a list or set. Short for conjoin."
	() -> []
	(xs as :list) -> xs
	(xs as :list, x) -> base :conj (xs, x)
	(xs as :set) -> xs
	(xs as :set, x) -> base :conj (xs, x)
}

fn add {
	"Adds numbers."
	() -> 0
	(x as :number) -> x
	(x as :number, y as :number) -> base :add (x, y)
	(x as :number, y as :number, ...zs) -> fold (base :add, add (x, y), zs)
}

fn print {
	"Sends a text representation of a Ludus value to stdout."
	(x) -> base :print (x)
}

fn show {
	"Returns a text representation of a Ludus value as a string."
	(x) -> base :show (x)
}

fn type {
	"Returns a keyword representing the type of the value passed in."
	(x) -> base :type (x)
}

fn prn {
	"Prints the underlying Clojure data structure of a Ludus value."
	(x) -> base :prn (x)
}

fn concat {
	"Combines two lists, strings, or sets."
	(x as :string, y as :string) -> base :str (x, y)
	(xs as :list, ys as :list) -> base :concat (xs, ys)
	(xs as :set, ys as :set) -> base :concat (xs, ys)
	(xs, ys, ...zs) -> fold (concat, zs, concat(xs, ys))
}

fn report {
	"Prints a value, then returns it."
	(x) -> {
		print (x)
		x
	}
	(msg as :string, x) -> print (concat (msg, show (x))) 
}

fn deref {
	"Resolves a ref into a value."
	(r as :ref) -> base :deref (r)
}

fn set! {
	"Sets the value of a ref."
	(r as :ref, value) -> base :set! (r, value)
}

ns prelude {
	first
	second
	rest
	nth
	count
	conj
	fold
	map
	list
	inc
	dec
	add
	print
	show
	prn
	type
	report
	concat
	deref
	set!
}

prelude