& this file, uniquely, gets `base` loaded as context. See src/ludus/base.cljc for exports

fn rest {
	"Returns all but the first element of a list or tuple, as a list."
	(xs as :list) -> base :rest (xs)
	(xs as :tuple) -> base :rest (xs)
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

fn print {
	"Sends a text representation of Ludus values to the console."
	(...args) -> base :print (args)
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
	(msg as :string, x) -> {
		print (concat (msg, show (x)))
		x
	}
}

fn ref? {
	"Returns true if a value is a ref."
	(r as :ref) -> true
	(_) -> false
}

fn deref {
	"Resolves a ref into a value."
	(r as :ref) -> base :deref (r)
}

fn set! {
	"Sets the value of a ref."
	(r as :ref, value) -> base :set! (r, value)
}

fn update! {
	"Updates a ref by applying a function to its value. Returns the new value."
	(r as :ref, f as :fn) -> {
		let current = deref (r)
		let new = f (current)
		set! (r, new)
	}
}

fn number? {
	"Returns true if a value is a number."
	(x as :number) -> true
	(_) -> false
}

fn add {
	"Adds numbers or vectors."
	() -> 0
	(x as :number) -> x
	(x as :number, y as :number) -> base :add (x, y)
	(x, y, ...zs) -> fold (base :add, zs, base :add (x, y))
	& add vectors
	((x1, y1), (x2, y2)) -> (base :add (x1, y1), base :add (x2, y2))
}

fn sub {
	"Subtracts numbers or vectors."
	() -> 0
	(x as :number) -> x
	(x as :number, y as :number) -> base :sub (x, y)
	(x, y, ...zs) -> fold (base :sub, zs, base :sub (x, y))
	((x1, y1), (x2, y2)) -> (base :sub (x1, x2), base :sub (x2, y2))
}

fn mult {
	"Multiplies numbers or vectors."
	() -> 1
	(x as :number) -> x
	(x as :number, y as :number) -> base :mult (x, y)
	(x, y, ...zs) -> fold (base :mult, mult (x, y), zs)
	(scalar as :number, (x, y)) -> (mult (x, scalar), mult (y, scalar))
	((x, y), scalar as :number) -> mult (scalar, (x, y))
}

fn div {
	"Divides numbers. Panics on division by zero."
	(x as :number) -> x
	(_, 0) -> panic! ("Division by zero.")
	(x as :number, y as :number) -> base :div (x, y)
	(x, y, ...zs) -> {
		let divisor = fold (mult, zs, y)
		div (x, divisor)
	}
}

fn div/0 {
	"Divides number. Returns 0 on division by zero."
	(x as :number) -> x
	(_, 0) -> 0
	(x as :number, y as :number) -> base :div (x, y)
	(x, y, ...zs) -> {
		let divisor = fold (mult, zs, y)
		div/0 (x, divisor)
	}
}

fn zero? {
	"Returns true if a number is 0."
	(0) -> true
	(_) -> false
}

fn eq? {
	"Returns true if all arguments have the same value."
	(x) -> true
	(x, y) -> base :eq (x, y)
	(x, y, ...zs) -> loop (y, zs) with {
		(a, [b]) -> base :eq (a, b)
		(a, [b, ...cs]) -> if base :eq (a, b)
			then recur (b, cs)
			else false 
	}
}

fn gt? {
	"Returns true if numbers are in decreasing order."
	(x as :number) -> true
	(x as :number, y as :number) -> base :gt (x, y)
	(x, y, ...zs) -> loop (y, zs) with {
		(a, [b]) -> base :gt (a, b)
		(a, [b, ...cs]) -> if base :gt (a, b)
			then recur (b, cs)
			else false 
	}
}

fn gte? {
	"Returns true if numbers are in decreasing or flat order."
	(x as :number) -> true
	(x as :number, y as :number) -> base :gte (x, y)
	(x, y, ...zs) -> loop (y, zs) with {
		(a, [b]) -> base :gte (a, b)
		(a, [b, ...cs]) -> if base :gte (a, b)
			then recur (b, cs)
			else false
	}
}

fn lt? {
	"Returns true if numbers are in increasing order."
	(x as :number) -> true
	(x as :number, y as :number) -> base :lt (x, y)
	(x, y, ...zs) -> loop (y, zs) with {
		(a, [b]) -> base :lt (a, b)
		(a, [b, ...cs]) -> if base :lt (a, b)
			then recur (b, cs)
			else false
	}
}

fn lte? {
	"Returns true if numbers are in increasing or flat order."
	(x as :number) -> true
	(x as :number, y as :number) -> base :lte (x, y)
	(x, y, ...zs) -> loop (y, zs) with {
		(a, [b]) -> base :lte (a, b)
		(a, [b, ...cs]) -> if base :lte (a, b)
			then recur (b, cs)
			else false
	}
}

fn neg? {
	"Returns true if a value is a negative number, otherwise returns false."
	(x as :number) if lt? (x, 0) -> true
	(_) -> false
}

fn pos? {
	"Returns true if a value is a positive number, otherwise returns false."
	(x as :number) if gt? (x, 0) -> true
	(_) -> false
}

fn nil? {
	"Returns true if a value is nil."
	(nil) -> true
	(_) -> false
}

fn bool? {
	"Returns true if a value is of type :boolean."
	(false) -> true
	(true) -> true
	(_) -> false
}

fn bool {
	"Returns false if a value is nil or false, otherwise returns true."
	(nil) -> false
	(false) -> false
	(_) -> true
}

fn not {
	"Returns false if a value is truthy, true if a value is falsy."
	(nil) -> true
	(false) -> true
	(_) -> false
}

& TODO: make `and` and `or` special forms which lazily evaluate arguments
fn and {
	"Returns true if all values passed in are truthy."
	() -> true
	(x) -> bool (x)
	(x, y) -> base :and (x, y)
	(x, y, ...zs) -> fold (base :and, zs, base :and (x, y))
}

fn or {
	"Returns true if any value passed in is truthy."
	() -> true
	(x) -> bool (x)
	(x, y) -> base :or (x, y)
	(x, y, ...zs) -> fold (base :or, zs, base :or (x, y))
}

fn assoc {
	"Takes a dict, key, and value, and returns a new dict with the key set to value."
	(d as :dict) -> d
	(d as :dict, key as :keyword, value) -> base :assoc (d, key, value)
	(d as :dict, (key as :keyword, value)) -> base :assoc (d, key, value)
}

fn dissoc {
	"Takes a dict and a key, and returns a new dict with the key and associated value omitted."
	(d as :dict) -> d
	(d as :dict, key as :keyword) -> base :dissoc (d, key)
}

fn coll? {
	"Returns true if a value is a collection: dict, struct, list, tuple, or set."
	(coll as :dict) -> true
	(coll as :struct) -> true
	(coll as :list) -> true
	(coll as :tuple) -> true
	(coll as :set) -> true
	(_) -> false
}

fn ordered? {
	"Returns true if a value is an indexed collection: list or tuple."
	(coll as :list) -> true
	(coll as :tuple) -> true
	(_) -> false
}

fn assoc? {
	"Returns true if a value is an associative collection: a dict, struct, or namespace."
	(assoc as :dict) -> true
	(assoc as :struct) -> true
	(assoc as :ns) -> true
	(_) -> false
}

fn get {
	"Takes a dict or struct, key, and optional default value; returns the value at key. If the value is not found, returns nil or the default value. Returns nil or default if the first argument is not a dict or struct."
	(key as :keyword, coll) -> base :get (key, coll)
	(key as :keyword, coll, default) -> base :get (key, coll, default)
}

& TODO: make this less awkward once we have tail recursion

&&&&&&&&& TODO:
&& Fix bug here:
&& The second pattern in `each` and `foo` hangs when the first argument is a function
&& But maybe not other kinds of values (works fine if it's a keyword)
&& See interpreter.cljc line 76 for more info.
fn each {
	"Takes a list and applies a function, presumably with side effects, to each element in the list. Returns nil."
	(f as :fn, []) -> nil
	(f as :fn, [x]) -> { f (x); nil }
	(f, [...xs]) -> loop (xs) with {
		([x]) -> { f (x); nil }
		([x, ...xs]) -> { f (x); recur (xs) }
	}
}

fn foo {
	&(h, []) -> :empty
	(h, [x]) -> :one
	(h, [...xs]) -> :more
}

fn panic! {
	"Causes Ludus to panic, outputting any arguments as messages."
	() -> base :panic! ()
	(...args) -> base :panic! (args)
}

& base turtle graphics
& forward, fd (pixels)
& back, bk (pixels)
& left, lt (turns)
& right, rt (turns)
& penup, pu ()
& pendown, pd ()
& pencolor, pc (color)
& penwidth (pixels)
& clear ()
& goto ((x, y))
& home ()
& turtlestate () -> @{:position (x, y), :heading turns, :visible boolean, :pen penstate}
& position () -> (x, y)
& turtleheading () -> turns
& penstate () -> @{:down :boolean, :color (r, g, b, a), :width pixels}

print ("Loaded Prelude.")

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
	print
	show
	prn
	type
	report
	concat
	deref
	set!
	add
	sub
	mult
	div
	div/0
	zero?
	neg?
	pos?
	eq?
	gt?
	gte?
	lt?
	lte?
	nil?
	bool?
	bool
	not
	and
	or
	coll?
	ordered?
	assoc?
	assoc
	get
	each
	panic!
	foo
}