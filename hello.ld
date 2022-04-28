fn fib (n) -> if eq (n, 0)
	then 1
	else if eq (n, 1)
		then 1
		else add (fib (sub (n, 1)), fib (sub (n, 2)))

fn fact {
	(n) -> fact (n, 1)
	(1, acc) -> acc
	(n, acc) -> fact (sub (n, 1), mult (n, acc))
}

ns Recursive {
	fact
	fib
}

Recursive :fib (25)