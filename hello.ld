import "foo.ld" as Foo

do #{:foo "bar"
	:bar (2, 3, :quux)
	:baz [12, 34]} > show > print