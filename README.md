![Ludus logo](logo.png)
## Ludus: A friendly, dynamic, functional language

A reference implementation of an interpreter for the Ludus programming language, using Clojure as a host language.

Ludus is part of the [_Thinking with Computers_ project](https://thinking-with-computers.github.io), run by Scott Richmond at the University of Toronto. Ludus is our research language, which aspires to be a free translation of Logo for the 2020s.

### Status
Pre-alpha, still under active development. See [the ludus-spec repo for progress notes and additional documentation](https://github.com/thinking-with-computers/ludus-spec/blob/main/todo.md).

### Use
* Clone this repo.
	- `git clone https://github.com/thinking-with-computers/cludus`
* Have Clojure and Leiningen installed.
	- On a Mac: `brew install clojure leiningen`
* `lein run {script}`, it runs your script.
* Alternately, download a binary on the [releases page](https://github.com/thinking-with-computers/cludus/releases).

### `Hello, world!`
Ludus is a scripting language. At current it does not have a REPL (our aim is to get interactive coding absolutely correct).

Either
```
"Hello, world!"
```
`=> "Hello, world!"`

Ludus scripts (and blocks) simply return their last expression; this script returns the bare string (to `stdout`) and exits.

Or:
```
print ("Hello, world!")
```
```
=> Hello, world! 
=> :ok
```

Or, you can use a the `print` function, which sends a string to `stdout`. Because `print` returns the keyword `:ok` when it completes, that is the result of the last expression in the script--and so Ludus also prints this.

### More on Ludus
Most of the (very active, somewhat messy) thinking about Ludus is housed in the [ludus-spec repository](https://github.com/thinking-with-computers/ludus-spec).
