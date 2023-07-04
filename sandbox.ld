

fn map {
	(f) -> fn mapper (xs) -> map (f, xs)
	(f, xs) -> {
		let n = count (xs)
		loop (0, []) with (i, ys) -> if eq (i, n)
				then ys
				else recur (inc (i), conj (ys, f (nth (i, xs))))
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

let greater_than_two = gt (_, 2)

fn square (x) -> mult (x, x)

let xs = [1, 2, 3]

map (square, xs)
