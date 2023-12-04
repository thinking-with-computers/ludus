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
}

fn nth {
	"Returns the element at index n of a list or tuple. Zero-indexed: the first element is at index 0."
	(xs as :list, n as :number) -> when {
		neg? (n) -> nil
		gte? (n, count (xs)) -> nil
		else -> base :nth (xs, inc (n))
	}
	(xs as :tuple, n as :number) -> when {
		neg? (n) -> nil
		gte? (n, count (xs)) -> nil
		else -> base :nth (xs, inc (n))
	}
}

fn first {
	"Returns the first element of a list or tuple."
	(xs) -> nth (xs, 0)
}

fn second {
	"Returns the second element of a list or tuple."
	(xs) -> nth (xs, 1)
}

fn last {
	"Returns the last element of a list or tuple."
	(xs) -> nth (xs, sub (count (xs), 1))
}

fn butlast {
	"Returns a list, omitting the last element."
	(xs as :list) -> base :slice (xs, sub (count (xs), 1))
}

fn slice {
	"Returns a slice of a list, representing a sub-list."
	(xs as :list, end as :number) -> slice (xs, 0, end) 
	(xs as :list, start as :number, end as :number) -> when {
		gte? (start, end) -> []
		gt? (end, count (xs)) -> slice (xs, start, count (xs))
		neg? (start) -> slice (xs, 0, end)
		else -> {
			let slice = base :slice (xs, inc (start), inc (end))
			base :into ([], slice)
		}
	}
}

fn count {
	"Returns the number of elements in a collection (including string)."
	(xs as :list) -> dec (base :count (xs))
	(xs as :tuple) -> dec (base :count (xs))
	(xs as :dict) -> base :count (xs)
	(xs as :string) -> base :count (xs)
	(xs as :set) -> base :count (xs)
	(xs as :struct) -> dec (base :count (xs))
}

fn list? {
	"Returns true if the value is a list."
	(l as :list) -> true
	(_) -> false
}

fn list {
	"Takes a value and returns it as a list. For values, it simply wraps them in a list. For collections, conversions are as follows. A tuple->list conversion preservers order and length. Unordered collections do not preserve order. Associative collections return lists of (key, value) tuples."
	(x) -> base :to_list (x)
}

fn set {
	"Takes an ordered collection--list or tuple--and turns it into a set."
	(xs as :list) -> base :into (${}, xs)
	(xs as :tuple) -> base :into (${}, xs)
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
		fn mapper (prev, curr) -> append (prev, f (curr))
		fold (mapper, xs, [])
	}
	(kw as :keyword, xs) -> {
		fn mapper (prev, curr) -> append (prev, kw (curr))
		fold (mapper, xs, [])
	}
}

fn append {
	"Adds an element to a list or set."
	() -> []
	(xs as :list) -> xs
	(xs as :list, x) -> base :conj (xs, x)
	(xs as :set) -> xs
	(xs as :set, x) -> base :conj (xs, x)
}

ref console = []

fn add_msg! {
	"Adds a message to the console."
	(msgs) -> {
		let strs = map (show, msgs)
		let msg = fold (concat, strs, "")
		update! (console, append (_, msg))
		:ok
	}
}

fn print! {
	"Sends a text representation of Ludus values to the console."
	(...args) -> {
		base :print (args)
		add_msg! (args)
	}
}

fn show {
	"Returns a text representation of a Ludus value as a string."
	(x) -> base :show (x)
}

fn type {
	"Returns a keyword representing the type of the value passed in."
	(x) -> base :type (x)
}

fn prn! {
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

fn report! {
	"Prints a value, then returns it."
	(x) -> {
		print! (x)
		x
	}
	(msg as :string, x) -> {
		print! (concat (msg, show (x)))
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

fn make! {
	"Sets the value of a ref."
	(r as :ref, value) -> base :set! (r, value)
}

fn update! {
	"Updates a ref by applying a function to its value. Returns the new value."
	(r as :ref, f as :fn) -> {
		let current = deref (r)
		let new = f (current)
		make! (r, new)
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
	((x1, y1), (x2, y2)) -> (add (x1, x2), add (y1, y2))
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

fn div/safe {
	"Divides a number. Returns a result tuple."
	(x as :number) -> (:ok, x)
	(_, 0) -> (:err, "Division by zero")
	(x, y) -> (:ok, div (x, y))
	(x, y, ...zs) -> {
		let divisor = fold (mult, zs, y)
		div/safe (x, divisor)
	}
}

fn abs {
	"Returns the absolute value of a number."
	(0) -> 0
	(n) -> if neg? (n) then mult (-1, n) else n
}

fn neg {
	"Multiplies a number by -1, negating it."
	(n as :number) -> mult (n, -1)
}

fn angle {
	"Calculates the angle between two vectors."
	(v1, v2) -> sub (atan/2 (v2), atan/2 (v1))
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
	() -> #{}
	(dict as :dict) -> dict
	(dict as :dict, key as :keyword, value) -> base :assoc (dict, key, value)
	(dict as :dict, (key as :keyword, value)) -> base :assoc (dict, key, value)
}

fn dissoc {
	"Takes a dict and a key, and returns a new dict with the key and associated value omitted."
	(dict as :dict) -> dict
	(dict as :dict, key as :keyword) -> base :dissoc (dict, key)
}

fn update {
	"Takes a dict, key, and function, and returns a new dict with the key set to the result of applying the function to original value held at the key."
	(dict as :dict) -> dict
	(dict as :dict, key as :keyword, updater as :fn) -> base :assoc (dict, key, updater (get (key, dict)))
}

fn keys {
	"Takes an associative collection and returns a list of keys in that collection. Returns an empty list on anything other than a collection."
	(coll) -> if not (assoc? (coll))
		then []
		else do coll > list > map (first, _)
}

fn values {
	"Takes an associative collection and returns a list of values in that collection. Returns an empty list on anything other than a collection."
	(coll) -> if not (assoc? (coll))
		then []
		else do coll > list > map (second, _)
}

fn diff {
	"Takes two associate data structures and returns a dict describing their differences. Does this shallowly, offering diffs only for keys in the original dict."
	(d1 as :dict, d2 as :dict) -> {
		let key1 = keys (d1)
		let key2 = keys (d2)
		let all = do concat (d1, d2) > set > list
		let diffs = fold ()
	}
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
	(key as :keyword) -> get (key, _)
	(key as :keyword, coll) -> base :get (key, coll)
	(key as :keyword, coll, default) -> base :get (key, coll, default)
}

fn dict {
	"Takes a struct or ns, and returns it as a dict. Or, takes a list or tuple of (key, value) tuples and returns it as a dict. Returns dicts unharmed."
	(struct as :struct) -> base :to_dict (struct)
	(ns_ as :ns) -> base :to_dict (ns_)
	(dict as :dict) -> dict
	(list as :list) -> fold (assoc, list)
	(tup as :tuple) -> do tup > list > dict
}

& TODO: make this less awkward once we have tail recursion
fn each! {
	"Takes a list and applies a function, presumably with side effects, to each element in the list. Returns nil."
	(f! as :fn, []) -> nil
	(f! as :fn, [x]) -> { f! (x); nil }
	(f! as :fn, [...xs]) -> loop (xs) with {
		([x]) -> { f! (x); nil }
		([x, ...xs]) -> { f! (x); recur (xs) }
	}
}

fn panic! {
	"Causes Ludus to panic, outputting any arguments as messages."
	() -> base :panic! ()
	(...args) -> base :panic! (args)
}

fn doc! {
	"Prints the documentation of a function to the console."
	(f as :fn) -> base :doc (f) 
	(_) -> :none
}

&&& Trigonometry functions

let pi = base :pi

let tau = mult (2, pi)

fn sin {
	"Returns the sine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in."
	(a as :number) -> do a > turn/rad > base :sin
	(a as :number, :turns) -> do a > turn/rad > base :sin
	(a as :number, :degrees) -> do a > deg/rad > base :sin
	(a as :number, :radians) -> base :sin (a)
}

fn cos {
	"Returns the cosine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in."
	(a as :number) -> do a > turn/rad > base :cos
	(a as :number, :turns) -> do a > turn/rad > base :cos
	(a as :number, :degrees) -> do a > deg/rad > base :cos
	(a as :number, :radians) -> base :cos (a)
}

fn tan {
	"Returns the sine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in."
	(a as :number) -> do a > turn/rad > base :tan
	(a as :number, :turns) -> do a > turn/rad > base :tan
	(a as :number, :degrees) -> do a > deg/rad > base :tan
	(a as :number, :radians) -> base :tan (a)
}

fn rotate {
	"Rotates a vector by an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in."
	((x, y), angle) -> rotate ((x, y), angle, :turns)
	((x, y), angle, units as :keyword) -> (
		sub (mult (x, cos (angle, units)), mult (y, sin (angle, units)))
		add (mult (x, sin (angle, units)), mult (y, cos (angle, units)))
	)
}

fn turn/deg {
	"Converts an angle in turns to an angle in degrees."
	(a as :number) -> mult (a, 360)
}

fn deg/turn {
	"Converts an angle in degrees to an angle in turns."
	(a as :number) -> div (a, 360)
}

fn turn/rad {
	"Converts an angle in turns to an angle in radians."
	(a as :number) -> mult (a, tau)
}

fn rad/turn {
	"Converts an angle in radians to an angle in turns."
	(a as :number) -> div (a, tau)
}

fn deg/rad {
	"Converts an angle in degrees to an angle in radians."
	(a as :number) -> mult (tau, div (a, 360))
}

fn rad/deg {
	"Converts an angle in radians to an angle in degrees."
	(a as :number) -> mult (360, div (a, tau))
}

fn atan/2 {
	"Returns an angle from a slope. Takes an optional keyword argument to specify units. Takes either two numbers or a vector tuple."
	(x as :number, y as :number) -> do base :atan_2 (x, y) > rad/turn
	(x, y, :turns) -> atan/2 (x, y)
	(x, y, :radians) -> base :atan_2 (x, y)
	(x, y, :degrees) -> do base :atan_2 (x, y) > rad/deg
	((x, y)) -> atan/2 (x, y)
	((x, y), units as :keyword) -> atan/2 (x, y, units)
}

fn mod {
	"Returns the modulus of num and div. Truncates towards negative infinity."
	(num as :number, y as :number) -> base :mod (num, div)
}

fn square {
	"Squares a number."
	(x as :number) -> mult (x, x)
}

fn sqrt {
	"Returns the square root of a number."
	(x as :number) -> base :sqrt (x) 
}

fn sum_of_squares {
	"Returns the sum of squares of numbers."
	() -> 0
	(x as :number) -> square (x)
	(x as :number, y as :number) -> add (square (x), square (y))
	(x, y, ...zs) -> fold (sum_of_squares, zs, sum_of_squares (x, y))
}

fn dist {
	"Returns the distance from the origin to a point described by (x, y)."
	(x as :number, y as :number) -> sqrt (sum_of_squares (x, y))
	((x, y)) -> dist (x, y)
}

&&& Number functions
fn random {
	"Returns a random number. With zero arguments, returns a random number between 0 (inclusive) and 1 (exclusive). With one argument, returns a random number between 0 and n. With two arguments, returns a random number between m and n."
	() -> base :random ()
	(n as :number) -> base :random (n)
	(m as :number, n as :number) -> add (m, random (n))
}

fn floor {
	"Truncates a number towards negative infinity. With positive numbers, it returns the integer part. With negative numbers, returns the next more-negative integer."
	(n as :number) -> base :floor (n)
}

fn ceil {
	"Truncates a number towards positive infinity. With negative numbers, it returns the integer part. With positive numbers, returns the next more-positive integer."
	(n as :number) -> base :ceil (n)
}

fn round {
	"Rounds a number to the nearest integer."
	(n as :number) -> base :round (n)
}

fn range {
	"Returns the set of integers between start (inclusive) and end (exclusive) as a list. With one argument, starts at 0. If end is less than start, returns an empty list."
	(end as :number) -> base :range (0, end)
	(start as :number, end as :number) -> base :range (start, end)
}

&&& Turtle & other graphics
let colors = @{
	:white (255, 255, 255)
	:light_gray (150, 150, 150)
	:dark_gray (50, 50, 50)
	:red (255, 0, 0)
	:green (0, 255, 0)
	:blue (0, 0, 255)
}

& the initial turtle state
let turtle_init = #{
	:position (0, 0) & let's call this the origin for now
	:heading 0 & this is straight up
	:pendown? true
	:color colors :white
	:penwidth 1
	:visible? true
}

& turtle_commands is a list of commands, expressed as tuples
& the first member of each tuple is the command
ref turtle_commands = []

& and a list of turtle states
ref turtle_states = [turtle_init]

ref p5_calls = [
	(:background, 0)
]

ref bgcolor = (0, 0, 0, 255)

fn add_call! (call) -> update! (p5_calls, append (_, call))

fn add_command! (command) -> {
	update! (turtle_commands, append (_, command))	
	let prev = do turtle_states > deref > last
	let curr = apply_command (prev, command)
	update! (turtle_states, append (_, curr))
	let call = state/call ()
	if call then { add_call! (call); :ok } else :ok
}

fn make_line ((x1, y1), (x2, y2)) -> (:line, x1, y1, x2, y2)

let turtle_radius = 20

let turtle_angle = 0.375

let turtle_color = (100, 100, 100, 100)

fn render_turtle! () -> {
	let state = do turtle_states > deref > last
	if state :visible?
		then {
			add_call! ((:push))	
			let (r, g, b, a) = turtle_color
			add_call! ((:fill, r, g, b, a))
			add_call! ((:noStroke))
			let #{heading, :position (x, y)} = state
			add_call! ((:translate, x, y))
			add_call! ((:rotate, turn/deg (heading)))
			add_call! ((:beginShape))
			let head_unit = heading/vector (heading)
			let first = mult (head_unit, turtle_radius)
			let (x1, y1) = first
			let (x2, y2) = rotate (first, turtle_angle)
			let (x3, y3) = rotate (first, neg (turtle_angle))
			add_call! ((:vertex, x1, y1))
			add_call! ((:vertex, x2, y2))
			add_call! ((:vertex, x3, y3))
			add_call! ((:endShape))
			add_call! ((:pop))
			:ok
		}
		else :ok
}

fn state/call () -> {
	let cmd = do turtle_commands > deref > last > first
	let states = deref (turtle_states)
	let curr = last (states)
	let prev = nth (states, sub (count (states), 2))
	print! ("Curr, prev, command", curr, prev, cmd)
	match cmd with {
		:forward -> if curr :pendown?
			then make_line (curr :position, prev :position)
			else nil
		:back -> if curr :pendown?
			then make_line (curr :position, prev :position)
			else nil
		:home -> if curr :pendown?
			then make_line (curr :position, prev :position)
			else nil
		:goto -> if curr :pendown?
			then make_line (curr :position, prev :position)
			else nil
		:penwidth -> (:strokeWeight, curr :penwidth)
		:pencolor -> {
			let (r, g, b, a) = curr :pencolor
			(:stroke, r, g, b, a)
		}
		:clear -> (:background, 0, 0, 0, 255)
		else -> nil
	}
}

fn forward! {
	"Moves the turtle forward by a number of steps. Alias: fd!"
	(steps as :number) -> add_command! ((:forward, steps))
}

let fd! = forward!

fn back! {
	"Moves the turtle backward by a number of steps. Alias: bk!"
	(steps as :number) -> add_command! ((:back, steps))
}

let bk! = back!

fn left! {
	"Rotates the turtle left, measured in turns. Alias: lt!"
	(turns as :number) -> add_command! ((:left, turns))
}

let lt! = left!

fn right! {
	"Rotates the turtle right, measured in turns. Alias: rt!"
	(turns as :number) -> add_command! ((:right, turns))
}

let rt! = right!

fn penup! {
	"Lifts the turtle's pen, stopping it from drawing. Alias: pu!"
	() -> add_command! ((:penup))
}

let pu! = penup!

fn pendown! {
	"Lowers the turtle's pen, causing it to draw. Alias: pd!"
	() -> add_command! ((:pendown))
}

let pd! = pendown!

fn pencolor! {
	"Changes the turtle's pen color. Takes a single grayscale value, an rgb tuple, or an rgba tuple. Alias: pc!"
	(gray as :number) -> add_command! ((:pencolor, (gray, gray, gray, 255)))
	((r as :number, g as :number, b as :number)) -> add_command! ((:pencolor, (r, g, b, 255)))
	((r as :number, g as :number, b as :number, a as :number)) -> add_command! ((:pencolor, (r, g, b, a)))
}

let pc! = pencolor!

fn penwidth! {
	"Sets the width of the turtle's pen, measured in pixels. Alias: pw!"
	(width as :number) -> add_command! ((:penwidth, width))
}

let pw! = penwidth!

fn background! {
	"Sets the background color behind the turtle and path. Alias: bg!"
	(gray as :number) -> make! (bgcolor, (gray, gray, gray, 255))
	((r as :number, g as :number, b as :number)) -> make! (bgcolor, (r, b, g, 255))
	((r as :number, g as :number, b as :number, a as :number)) -> make! (bgcolor, (r, g, b, a))
}

let bg! = background!

fn home! {
	"Sends the turtle home: to the centre of the screen, pointing up. If the pen is down, the turtle will draw a path to home."
	() -> add_command! ((:home))
}

fn clear! {
	"Clears the canvas and sends the turtle home."
	() -> add_command! ((:clear))
}

fn goto! {
	"Sends the turtle to (x, y) coordinates. If the pen is down, the turtle will draw a path to its new location."
	(x as :number, y as :number) -> add_command! ((:goto, (x, y)))
	((x, y)) -> goto! (x, y)
}

fn heading/vector {
	"Takes a turtle heading, and returns a unit vector of that heading."
	(heading) -> {
		& 0 is 90º/0.25T, 0.25 is 180º/0.5T, 0.5 is 270º, 0.75 is 0º
		let angle = add (heading, 0.25)
		(cos (angle), sin (angle))
	}
}

fn apply_command {
	"Takes a turtle state and a command and calculates a new state."
	(state, command) -> match command with {
		(:goto, (x, y)) -> assoc (state, :position, (x, y))
		(:right, turns) -> update (state, :heading, sub (_, turns))
		(:left, turns) -> update (state, :heading, add (_, turns))
		(:forward, steps) -> {
			let #{heading, position} = state
			let unit = heading/vector (heading)
			let vect = mult (steps, unit)
			update (state, :position, add (vect, _))
		}
		(:back, steps) -> {
			let #{heading, position} = state
			let unit = heading/vector (heading)
			let vect = mult (steps, unit)
			update (state, :position, sub (_, vect))
		}
		(:penup) -> assoc (state, :pendown?, false)
		(:pendown) -> assoc (state, :pendown?, true)
		(:penwidth, pixels) -> assoc (state, :penwidth, pixels)
		(:pencolor, color) -> assoc (state, :pencolor, color)
	}
}

fn turtle_state {
	"Returns the turtle's current state."
	() -> do turtle_states > deref > last
}

& position () -> (x, y)
fn position {
	"Returns the turtle's current position."
	() -> turtle_state () :position
}

fn heading {
	"Returns the turtle's current heading."
	() -> turtle_state () :heading
}

fn pendown? {
	"Returns the turtle's pen state: true if the pen is down."
	() -> turtle_state () :pendown?
}

fn pencolor {
	"Returns the turtle's pen color as an (r, g, b, a) tuple."
	() -> turtle_state () :pencolor
}

fn penwidth {
	"Returns the turtle's pen width in pixels."
	() -> turtle_state () :pencolor
}

ns prelude {
	first
	second
	rest
	nth
	last
	butlast
	slice
	count
	append
	fold
	map
	list
	set
	inc
	dec
	print!
	console
	show
	prn!
	type
	report!
	concat
	deref
	make!
	update!
	add
	sub
	mult
	div
	div/0
	div/safe
	angle
	abs
	neg
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
	dissoc
	update
	get
	dict
	keys
	values
	diff
	each!
	panic!
	doc!
	sin
	cos
	tan
	turn/rad
	rad/turn
	turn/deg
	deg/turn
	rad/deg
	deg/rad
	atan/2
	mod
	square
	sum_of_squares
	dist
	random
	pi
	tau
	floor
	ceil
	round
	range
	forward!, fd!
	back!, bk!
	right!, rt!
	left!, lt!
	penup!, pu!
	pendown!, pd!
	pencolor!, pc!
	penwidth!, pw!
	home!, clear!, goto!,
	heading, position, pendown?
	pencolor, penwidth
	heading/vector
	turtle_state
	p5_calls, turtle_states, turtle_commands
	render_turtle!
}
