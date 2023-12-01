& this file, uniquely, gets `base` loaded. See src/ludus/base.cljc for exports

fn first {
	"Returns the first element of a list or tuple."
	(xs as :list) -> extern (:first, xs)
	(xs as :tuple) -> extern (:second, xs)
}

fn nth {
	"Returns the element at index n of a list or tuple. Zero-indexed: the first element is at index 0."
	(xs as :list, n as :number) -> extern (:nth, xs, n)
	(xs as :tuple, n as :number) -> extern (:nth, xs, extern (:inc, n))
}

fn count {
	"Returns the number of elements in a list or tuple."
	(xs as :list) -> extern (count, xs)
	(xs as :tuple) -> extern (:dec, extern (:count, xs))
}

fn list {
	"Takes a tuple, and returns it as a list."
	(xs as :tuple) -> extern (:into, [], extern (:rest, xs))
}

fn fold {
	"Folds a collection."
	(f as :function, xs as :list) -> extern (:reduce, f, xs)
	(f as :function, root, xs as :list) -> extern (:reduce, f, root, xs)
}

fn conj {
	"Adds an element to a list. Short for conjoin."
	() -> []
	(xs as :list) -> xs
	(xs as :list, x) -> extern (:conj, xs, x)
	& (xs, x, ...ys) -> extern (:conj, xs, x, ...ys)
}

fn ludus/add {
	"Adds numbers."
	() -> 0
	(x as :number) -> x
	(x as :number, y as :number) -> add (x, y)
	(x as :number, y as :number, ...zs) -> fold (ludus/add, add (x, y), zs)
}

ns math {
	:add ludus/add
}

print ("From Prelude, hello.")

ns prelude {
	first
	nth
	conj
	fold
	list
}

print (prelude)

prelude