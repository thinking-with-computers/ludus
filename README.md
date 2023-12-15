![Ludus logo](logo.png)
## Ludus: A friendly, dynamic, functional language
Ludus is a scripting programming language that is designed to be friendly, dynamic, and functional.

This repo currently contains an in-progress reference implementation of an interpreter for the Ludus programming language, using Clojure as a host language.

Ludus is part of the [_Thinking with Computers_ project](https://thinking-with-computers.github.io), run by Scott Richmond at the University of Toronto. Ludus is our research language, which aspires to be a free translation of Logo for the 2020s.

Here are our design goals:

#### Friendly
Ludus, like Logo, is meant to be a teaching language, often for students who don't think of themselves as "computer people." Our intended audience are humanities and art people at the University level (undergrads, grads, faculty). Everything is kept as simple as possible, but no simpler. Everything is consistent as possible. We aspire to the best error messages we can muster, which is important for a language to be teachable. That means being as strict as we can muster, _in order to be friendlier_.

Our current development target is Ludus on the web: https://thinking-with-computers/ludus-web. That wires what we do on the langauge interpreter (here in this repo) to a web frontent.

Naturally, it starts with Logo's famed turtle graphics.

#### Dynamic
Statically typed programming languages generally give more helpful error messages than dynamic ones, but learning a type system (even one with robust type inference) requires learning two parallel systems: the type system and the expression system. Type systems only really make sense once you've learned why they're necessary. And their benefits seem (to us, anyway) to be largely necessary when writing long-lived, maintainable, multiauthor code. Ludus code is likely to be one-off, expressive, and single-authored.

To stay friendly, Ludus is dynamic. But: despite the dynamism, we aim to be as strict as possible. Certainly, we want to avoid the type conversion shenanigans of a language like JavaScript.

#### Functional
Ludus is emphatically functional: it uses functions for just about everything. This is both because your humble PI had his world reordered when he learned his first functional language (Elixir), and because the research into Logo and the programming cultures of MIT in the 1970s revolve around extremely functional Lisp code (i.e., Scheme). Logo is a weird little language, but it is a descendant of Lisp. So is Ludus.

Also, we believe that Ludus's immutable bindings and persistent or immutable data structures and careful approach to manipulating state lead to a lot of good pedagogical results. Learning a programming language involves learning how to model what's going on inside the computer; Ludus, we think, makes that simpler and easier.

If you're looking for cognate languages, Ludus takes a _lot_ of design inspiration from Clojure and Elixir (which itself took a lot from Clojure). (The current--quick, dirty, and slow--version of LUdus written in Clojure, in no small part to have access to persistent data structures that compile to JS.) Clojure and Elixir are great! If you're asking why you should use Ludus instead of them, you're already at the point where you should be using them. Ludus is, maybe, for the people whom you'd like to work with in 5 years at your Pheonix shop.

### Status
Pre-alpha, still under active development. Lots of things change all the time. See [the ludus-spec repo for progress notes and additional documentation](https://github.com/thinking-with-computers/ludus-spec/blob/main/todo.md).

The current version of Ludus is a pure function that runs in JavaScript. We have plans for more and better things.

### Use
Current emphasis is on the web version: see https://github.com/thinking-with-computers/ludus-web/.

Or, if you're on a Mac and want to open a terminal:
* Clone this repo.
	- `git clone https://github.com/thinking-with-computers/ludus`
* Have Clojure and Leiningen installed.
	- On a Mac: `brew install clojure leiningen`
* `lein run {script}`, it runs your script.
* `lein run`, it runs a REPL.

~Or, download a binary on the [releases page](https://github.com/thinking-with-computers/ludus/releases). (At current: M1 Mac only.)~

### Main features
* Expression-oriented: everything returns a value
* Pattern matching in all the places
* No operators: everything is called as a function
* Easy-peasy partial application with placeholders
* Function pipelines
* Persistent or immutable data structures
* Careful, explicit state management using `ref`erences
* Clean, concise, expressive syntax
* Value-based equality; only functions are reference types

#### Under construction
* Tail call optimization
* Actor model style concurrency?
* ~Strong nominal data typing, including tagged unions~
	- ~Exhaustiveness-checking in `match` expressions in dynamically-typed code~
* Faster, bytecode-based VM written in a systems language, compiled to WASM for web use.
* Recursive descent parser with excellent parsing error message, just as soon as the syntax is fully stabilized.

### `Hello, world!`
Ludus is a scripting language. At current it does not have a good REPL. Our aim is to get interactive coding absolutely correct, and our efforts in [ludus-web](https://github.com/thinking-with-computers/ludus-web) are currently under way to surface the right interactivity models for Ludus.

Either:
```
"Hello, world!"
```
`=> "Hello, world!"`

Ludus scripts (and blocks) simply return their last expression; this script returns the bare string and exits.

Or:
```
print! ("Hello, world!")
```
```
=> Hello, world! 
=> :ok
```

Or, you can use a the `print!` function, which sends a string to a console (`stdout` on Unix, or a little console box on the web). Because `print!` returns the keyword `:ok` when it completes, that is the result of the last expression in the script--and so Ludus also prints this.

### Some code
Fibonacci numbers:
```
& fibonacci!, with multi-clause fns/pattern matching

fn fib {
	"Returns the nth fibonacci number."
	(1) -> 1
	(2) -> 1
	(n) -> add (
		fib (sub (n, 1))
		fib (sub (n, 2)))
}

fib (10) &=> 55
```

### More on Ludus
See the [language reference](/language.md) and [the documentation for the prelude](/prelude.md).
