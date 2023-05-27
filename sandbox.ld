fn unwrap {
	((:some, value)) -> value
	((:ok, value)) -> value
}

fn default (default_value) -> fn (maybe) -> match maybe with {
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

unwrap ((:err, "message"))
