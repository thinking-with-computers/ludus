fn fib {
	"Tells you a fib number."
	(0) -> 0
	(1) -> 1
	(n) -> add (
			fib (dec (n))
			fib (sub (n, 2))
		)
}

fn unwrap {
	((:some, value)) -> value
	((:ok, value)) -> value
}

fn default (default_value) -> fn (maybe) -> when maybe is {
	(:ok, value) -> value
	(:err, _) -> default_value
	nil -> default_value
	value -> value
}

fn some (value) -> (:some, value)

fn ok (value) -> (:ok, value)

let foo = unwrap ((:ok, 42))

print (:foo, foo)

let bar = unwrap ((:some, 23))

print (:bar, bar)

let baz = do 69 > default (12) > print (:baz, _)

let quux = do nil > default (12) > print (:quux, _)

& unwrap ((:err, "message"))

fn map {
	(f) -> fn mapper (xs) -> map (f, xs)
	(f, xs) -> {
		let n = count (xs)
		loop (0, []) with (i, ys) -> if eq (i, n)
				then ys
				else recur (
					inc (i)
					conj (ys, f (nth (i, xs))))
	}
}

fn reduce {
	(f) -> fn reducer {
		(xs) -> reduce (f, xs)
		(xs, init) -> reduce (f, xs, init)
	}
	(f, xs) -> {
		let first_x = first (xs)
		let more_xs = rest (xs)
		reduce (f, more_xs, first_x)
	}
	(f, xs, init) -> {
		let n = count (xs)
		loop (0, init) with (i, acc) -> if eq (i, n)
				then acc
				else {
					let curr = nth (i, xs)
					let next = f (acc, curr)
					recur (inc (i), next)
				}
	}
}

fn filter {
	(f) -> fn filterer (xs) -> filter (f, xs)
	(f, xs) -> {
		let n = count (xs)
		loop (0, []) with (i, ys) -> when {
				eq (i, n) -> ys
				f (nth (i, xs)) -> recur (inc (i), conj (ys, nth (i, xs)))
				else -> recur (inc (i), ys)
			}
	}
}

& fn shuffle

ref x = 4

set! (x, "foo")

set! (x, :foo)

deref (x)

let greater_than_two? = gt (_, 2)

fn square (x) -> mult (x, x)

let xs = [1, 2, 3]

let ys = #{:a 1, :b 2}

ys :a

:a (ys)

let y = 1

do y > inc > square > sub (_, 3) 
