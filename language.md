# Ludus language reference

This is not intended for beginners, but to be a language overview for experienced users. That said, it may help beginners orient themselves in the language.

## Comments
Ludus's comment character is `&`. Anything after an ampersand on a line is ignored. There are no multiline comments.

## Atomic values
Ludus has four types of atomic values.

### `nil`
`nil` is Ludus's representation of nothing. In the grand Lisp tradition, Ludus can, and frequently does, use `nil`-punning. Its type is `:nil`.

### Booleans
`true` and `false`. That said, in all conditional constructs, `nil` and `false` are "falsy," and everything else is "truthy." Their type is `:boolean`.

### Numbers
Ludus has numbers. At current, they rely on underlying number types. When Ludus runs in the browser, numbers are Javascript 64-bit floats. When Ludus runs at the command line in JVM-based Clojure, Ludus numbers could be ints, floats, or ratios, depending. Numbers are more complicated than you think.

Number literals in Ludus are either integers or decimal floating point numbers. 

Numbers' type is `:number`.

### Keywords
Ludus keywords begin with a colon and a letter, e.g. `:keyword`. Types are represented as keywords. Some functions take an optional units argument as a keyword, e.g. `:radians`. Keywords are also used as keys for associative collections. Keywords' type is `:keyword`.

Keywords must begin with an ASCII upper- or lower-case letter, and can then include any letter character, as well as `_`, `/`, `!`, `?`, and `*`.

## Strings
Ludus strings are dependent on the host platform. Given that Ludus either runs in Javascript or the JVM, strings are UTF-16 strings. Eventually, Ludus will use UTF-8 strings. Strings are much more complicated than you think. Strings are emphatically not a collection type.

Strings' type is `:string`.

## Collections
Ludus has a few different types of collections, in increasing order of complexity.

### Separators
In all collection literals, members are written with a separator between them. On the same line, use a comma; or a newline will also separate elements. You may use as many 

### Tuples
Tuples are fully-imutable, ordered collections of any kinds of values, delimited by parentheses, e.g. `(1, :a, "foo")`. At current, they have no length limit (although they eventually will). Unlike in some languages, tuples can be empty or contain a single element: `()` and `(:foo)` are both just fine. Tuples largely cannot be manipulated functionally; they must be written as literals and unpacked using pattern matching. They can, however, be converted to lists, either through pattern matching or the `list` function. Their type is `:tuple`.

### Lists
Lists are persistent and immutable ordered collections of any kinds of values, delimited by square braces, e.g. `[1, :a, "foo"]`. They are currently implemented using Clojure's persistent vectors. Their type is `:list`.

Lists may be combined using splats, written with ellipses, e.g., `[...foo, ...bar]`.

### Sets
Sets are persistent and immutable unordered collections of any kinds of values, which can only contain one instance of any given value. They are written similarly to ordered collections: `${1, :a, "foo"}`. Their type is `:set`.

### Dictionaries, or dicts
Dicts are persistent and immutable associative collections of any kinds of values. Dicts use keywords as keys (and cannot use any other kind of Ludus value as a key, not even strings), but can store any values. Dict literals are written as keyword-value pairs: `#{:a 1, :b false}`. Single words may be used as a shorthand for a key-value pair. Accessing a key that holds no value returns `nil`. Their type is `:dict`. 

### Namespaces
Namespaces are immutable collections of bindings. They may only be described at the top level of a script. Accessing a key that has no value on a namepsace results in a panic. Their type is `:ns`.

They are written with the form `ns`, then a word that will be bound as their name, and then an associative structure (pairs or word shorthands), delimited by `{}`, e.g.:

```
ns foo {
  :bar "bar"
  :baz 42
  quux
}
```
## Expressions
Ludus is an expression-based language: all forms in the language are expressions and return values. That said, not all expressions may be used everywhere.

### Toplevel expressions
Some expressions may only be used in the "top level" of a script. Because they are the toplevel, they are assured to be statically knowable. These include: `ns`, `use`, `import`, and `test`.

### Non-binding expressions
Some forms may take any expression that does _not_ [bind a name](#Words-and-bindings), for example, any entry in a collection, or the right-hand side of a `let` binding. This is because binding a name in some positions is ambiguous, or nonsensical.

### Simple expressions
Many complex forms will only accept "simple" expressions. Formally, simple expressions are either literal (atomic, string, or collection literals) or synthetic expressions. They are expressions which do not take sub-expressions.

## Words and bindings
Ludus uses "words" to bind values to names. Words must start with a lower case ASCII letter, and can subsequently include any letter character (modulo backing character encoding), as well as , `_`, `/`, `?`, `!`, and `*`.

Ludus binds values to names immutably and permanently: no name may ever be re-bound to a different value. (Although see [refs](#references-and-state), below.

Attempting to use an unbound name (a word that has not had a value bound to it) will result in a panic.

### `let` bindings: a very short introduction
Ludus's basic binding form is `let`:

```
let foo = :bar & `foo` is now bound to `bar` for the rest of the scope.

let foo = :baz & panic!
```

`let` bindings are more complex; we will return to these below.

## Patterns
Ludus makes extensive use of pattern-matching. Patterns do two jobs at once: they match values (or don't); and they bind names. The left-hand side of the examples just above in the `let` binding is not just a word: it is a pattern. Patterns also arise in conditional forms and function declarations.

### The placeholder: `_`
The simplest pattern is the placeholder: it matches against anything, and does not bind a name. It is written as a single underscore: `_`, e.g., `let _ = :foo`.

#### Ignored names
If you wish to be a bit more explict than using a placeholder, you can use an ignored name, which is a name that starts with an underscore: `_foo`. This is not bound, is not a valid name, and can be used however much you wish, even multiple times in the same pattern.

### Literal patterns
Patterns can be literal atomic values or strings: `0`, `false`, `nil`, `"foo"`, etc. That means you can write `let 0 = 0` or `let :foo = :foo`, and everything will be jsut fine.

Literals match against, well, literal values: if the pattern and the value are the same, they match! If not, they don't match.

Literal values do not bind anything.

### Word patterns
Word patterns match against any value, and bind that value to the word as a name. The scope of that binding depends on the form the pattern is in. `let foo = :bar` binds `:bar` to `foo` for the rest of the scope.

#### Typed patterns
Word patterns can, optionally, take a type, using the `as` reserved word, and the keyword representing the desired type: `let foo as :number = 42`.

### Collection patterns
Tuples, lists, and dicts can be destructured using patterns. They are written nearly identically to their literal counterparts. Collection patterns are composed of any number of simpler patterns or other collection patterns. They bind any names nested in them, match literals in them, etc.

#### Tuple patterns
Tuple patterns are delimited using parens, using commas or newlines to separate any number of other patterns. Consider `let (x, y, z) = (1, 2, 3)`. `x`, `y`, and `z` are now bound to 1, 2, and 3, respectively.

The last item in a tuple pattern can be a splat--`...`--which either discards any remaining unenumerated values in a tuple, or binds them to a list. Without a splat, tuples patterns only match against tuples of the same length.

```
let mytup = (1, 2, 3)
let (x, _, y) = mytup & x is now 1, y is now 3
let (a, ...) = mytup & a is now 1; a bare splat (without a name) is just fine
let (_, ...cs) = mytup & cs is now [2, 3]
let (p, q) = mytup & panic! no match
let () = () & empty tuples are also patterns
```

#### List patterns
List patterns are identical to tuple patterns, but they are written using square braces. They also match against a specific number of members, and may take a splat in the last position, e.g. `let [first, ...rest] = [1, 2, 3]`.

#### Dict patterns
Dict patterns are written either with shorthand words, or keyword-pattern pairs. Consider: `let #{:a foo, :b 12, c} = #{:a 1, :b 12, :c 4}`. `foo` is now 1; `b` is now 12, `c` is now 4. If a dict does not hold a value at a particular key, there is no match.

Dict patterns may also use a splat as their last member: `let #{:a 1, ...b} = #{:a 1, :b 2, :c 3}` will bind `b` to `#{:b 2, :c 3}`.

## `let` bindings
`let` bindings are the basic form of matching and binding in Ludus. It is written `let {pattern} = {non-binding expression}`. The pattern can be arbitrarily complex. If the left-hand side of a `let` binding does not match, Ludus will raise a panic, halting evaluation of the script.

## Scope and blocks
Ludus is lexically scoped. Bindings are valid for the remainder of the scope they act on. To introduce a new scope, Ludus uses a block, a collection of expressions delimited by curly braces and separated by semicolons or newlines. The value of a block, as well as the value of a script, is the last expression in it. In `let foo = {:this; :set; :of; :expressions; "is actually"; :ignored }`, `foo` will be bound to `ignored`.

That said, you can use bindings in blocks, which will not be available outside that block--but blocks can use bidnings from their parent scope:

```
let outer = 42

let first = {
  let inner = 23
  add (outer, inner)
} & first is now bound to 65

inner & panic! unbound name inner

```

### Shadowing
Even though names are bound permanently in Ludus, it is perfectly possible (and occasionally quite useful) to "shadow" names from an enclosing scope.

```
let x = 42

let y = {
  let first = x
  let x = 23 & this is fine
  let second = x
  add (first, second)
} & y is now 65

```

## Conditional forms
Ludus has a robust set of conditionoal forms, all of which are expressions and resolve to a single value.

### `if`
`if` evaluates a condition; if the result of the condition is truthy, it evaluates is `then` branch; if the condition is falsy, it evaluates its `else` branch. Both branches must always be present. Newlines may come before `then` and `else`.

`if {simple expression} then {non-binding expression} else {non-binding expression}`

#### `if let`
There is one exception to the above: a single `let` form can be used as the condition. In that case, if the `let` binding is successful--the pattern matches--the `then` branch is evaluated with any resulting bindings. If the pattern does not match, in place of a panic, the `else` branch is evaluated. Note that if the right-hand value of the `let` expression is falsy, the `else` branch will also be evaluated.

```
if let (:ok, value) = might_fail ()
  then do_something_with (value)
  else recover
```

### `when`
`when` is like Lisp's `cond`: it takes a series of clauses, separated by semicolons or newlines, delimited by curly braces. Clauses are written `lhs -> rhs`. `when` expressions are extremely useful for avoiding nested `if`s. 

The left hand of a clause is a simple expression; the right hand of a clause is any expression. When the left hand is truthy, the right hand is evaluated, and the result of that evaluation is returned; no further clauses are evaluated. If no clause has a truthy left-hand value, then a panic is raised. `else` and `_` are valid left-hand expressions, which are always truthy in this context.

```
when {
  maybe () -> :something
  mabye_not () -> :something_else
  else -> :always
}
```

### `match`
A `match` form is the most powerful conditional form in Ludus (for now). It consists of a test expression, and a series of clauses. The test expression must be a simple expression, followed by `with`, and then a series of clauses separated by a semicolon or newline, delimited by curly braces.

```
match may_fail () with {
  (:ok, value) -> calculate_result (value)
  (:err, msg) -> { log! (msg); recover_somehow () }
}
```

The left hand of a match clause is a pattern; the right hand is an expression: `pattern -> expression`. If the pattern matches the test expression of a clause, the expression is evaluated with any bindings from the pattern, and `match` form evaluates to the result of that expression.

If a test expression does not match against any clause's pattern, a panic is raised. As with `when` expressions, `_` and `else` always match.

Ludus does not attempt to do any exhaustiveness checking on match forms. 

## Functions
Ludus is an emphatically functional language. Almost everything in Ludus is accomplished by applying functions to values, or calling functions with arguments. (These are precise synonyms.)

Functions have the type `:fn`.

### Calling functions
Functions are called by placing a tuple with arguments immediately after a function name, e.g. `add (1, 2)` adds `1` and `2`. Because they are represented as tuples, arguments must be explicitly written; splats cannot be used to pass an arbitrary number of arguments to a function.

### Defining functions
Functions have three increasingly complex forms to define them. All of them include the concept of a function clause, which is just a match clause whose left hand side must be a _tuple_ pattern.

### Function forms

#### Anonymous lambda
An anonymous lambda is written `fn {tuple pattern} -> {expression}`, `fn (x, y) -> if gt? (x, y) then x else add (x, y)`. Lambdas may only have one clause.

#### Named functions
A named function is identical to a lambda, with the one change that a word follows the `fn` reserved word: `fn {name} {tuple pattern} -> {expression}`. E.g., `fn add_1 (x) -> add (x, 1)`. The name of the function is bound for the remainder of the scope.

#### Compound functions
Compound functions are functions that have multiple clauses. They must be named, and in place of a single clause, they consist in multiple clauses, separated by semicolons or newlines, delimited by curly braces. Optionally, compound functions may have a docstring, which explains the function's purpose and use, before any of the function clauses.

An exampele from Ludus's Prelude:

```
fn some {
	"Takes a possibly `nil` value and a default value. Returns the value if it's not `nil`, returns the default if it's `nil`."
	(nil, default) -> default
	(value, _) -> value 
}
```

### Closures
Functions in Ludus are closures: function bodies have access not only to their specific scope, but any enclosing scope. Note that function bodies may have access to names bound after them in their scope, so long as the function is _called_ after any names it accesses are bound.

### The Prelude
The Prelude is a substantial set of functions that is available in any given Ludus script. (It is, itself, just a Ludus file that has special access to host functions.) Because of that, a large number of functions are always available. The prelude documentation is [here](/prelude.md).

### Partial application
Functions in Ludus can be partially applied by using a placeholder in the arguments. Partial application may only use a single placeholder (partially applied functions are always unary), but it can be anywhere in the arguments: `let add_1 = add(1, _)` or `let double = mult(_, 2)`.

Unary functions and called keywords may _not_ be partially applied: it is redundant.

Because of "partial application," Ludus has a concept of an "argument tuple" (which may include a single placeholder) in addition to a tuple literal (which may not include a placeholder).

### Function pipelines, or `do` forms
In place of nesting function calls inside other function calls, Ludus allows for a more streamlined version of function application: the `do` form or function pipeline:

```
let silly_result = do 23
  > mult (_, 2)
  > add (1, _)
  > sub (_, 2)
  > div (_, 9) & silly_result is 5
```

### Called keywords
Keywords may be called as functions, in which case they extract the value stored at that key in the value passed in:

```
let foo = #{:a 1, :b 2}
let bar = :a (foo) & `bar` is now 1
```

Called keywords can be used in pipelines.

## Synthetic expressions
Synthetic expressions are valid combinations of words, keywords, and argument tuples which allow for calling functions and extracting values from associative collections. The root--first term--of a synthetic expression must be a word or a keyword; subsequent terms must be either argument tuples or keywords.

```
let foo = #{:a 1, :b #{:c "bar" :d "baz"}}

let bar = foo :b :c & `bar` is now "bar"

let baz = :b (foo) :d & `baz` is now "baz"

```

## Looping forms
Ludus will have optimized tail calls--the most straightforward way to accomplish repeating behaviour is function recursion. There are two additional looping forms, `repeat` and `loop`.

### `repeat`
`repeat` is a help to learners, and is useful for executing side effects multiple times. It is written `repeat {number|word} { {exprs} }`. From turtle graphics:

```
repeat 4 {
  forward! (100)
  right! (0.25)
}
```
Note that `repeat` does two interesting things:

1. It never returns a value other than `nil`. If it's in the block, it disappears.
2. Unlike everything else in Ludus, it requires a block. You cannot write `repeat 4 forward (100)`. (Watch this space.)

### `loop`/`recur`

## Environment and context: the toplevel

### `import`
`import` allows for the evaluation of other Ludus scripts: `import "path/to/file" as name`. `import` just evaluates that file, and then binds a name to the result of evaluating that script. This, right now, is quite basic: circular imports are currently allowed but will lead to endless recursion; results are not cached, so each `import` in a chain re-evaluates the file; and so on.

### `use`
`use` loads the contents of a namespace into a script's context. To ensure that this is statically checkable, this must be at the toplevel.

### `ns`

## Errors: panic! in the Ludus script

