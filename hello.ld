fn fib {
	(0) -> 1
	(1) -> 1
	(n) -> add (
		fib (sub (n, 1)), 
		fib (sub (n, 2)))
}

fn fact {
	(n) -> fact (n, 1)
	(1, acc) -> acc
	(n, acc) -> fact (sub (n, 1), mult (n, acc))
}

ns Recursive {
	fact
	fib
}

:factl (Recursive) (6)