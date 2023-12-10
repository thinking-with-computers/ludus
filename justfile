# start a repl
repl:
	clj -X:repl

build:
	shadow-cljs release node
