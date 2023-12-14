# Ludus prelude documentation
These functions are available in every Ludus script.
The documentation for any function can be found within Ludus by passing the function to `doc!`, 
e.g., running `doc! (add)` will send the documentation for `add` to the console.

For more information on the syntax & semantics of the Ludus language, see [language.md](./language.md).

## A few notes
**Naming conventions.** Functions whose name ends with a question mark, e.g., `eq?`, return booleans.
Functions whose name ends with an exclamation point, e.g., `make!`, change state in some way.
In other words, they _do things_ rather than _calculating values_. 
Functions whose name includes a slash either convert from one value to another, e.g. `deg/rad`,
or they are variations on a function, e.g. `div/0` as a variation on `div`.

**How entries are formatted.** Each entry has a brief (sometimes too brief!) description of what it does.
It is followed by the patterns for each of its function clauses.
This should be enough to indicate order of arguments, types, and so on.

**Patterns often, but do not always, indicate types.** Typed patterns are written as `foo as :bar`,
where the type is indicated by the keyword. 
Possible ludus types are: `:nil`, `:boolean`, `:number`, `:keyword` (atomic values);
`:string` (strings are their own beast); `:tuple` and `:list` (indexed collections); `:set` (sets are specific), 
`:dict` and `:ns` (associative collections); and `:ref` (references).

**Conventional types.** Ludus has two types based on conventions.
* _Result tuples._ Results are a way of modeling the result of a calculation that might fail.
The two possible values are `(:ok, value)` and `(:err, msg)`.
`msg` is usually a string describing what went wrong.
To work with result tuples, see [`unwrap!`](#unwrap) and [`unwrap_or`](#unwrap_or).
That said, usually you work with these using pattern matching.

* _Vectors._ Vectors are 2-element tuples of x and y coordinates.
The origin is `(0, 0)`. 
Many math functions take vectors as well as numbers, e.g., `add` and `mult`.
You will see vectors indicated in patterns by an `(x, y)` tuple.
You can see what this looks like in the last clause of `add`: `((x1, y1), (x2, y2))`.

## Functions by topic

### Boolean
[and](#and)&nbsp;&nbsp;&nbsp; [bool](#bool)&nbsp;&nbsp;&nbsp; [bool?](#bool)&nbsp;&nbsp;&nbsp; [false?](#false)&nbsp;&nbsp;&nbsp; [not](#not)&nbsp;&nbsp;&nbsp; [or](#or)

### Dicts
[assoc](#assoc)&nbsp;&nbsp;&nbsp; [assoc?](#assoc)&nbsp;&nbsp;&nbsp; [dict](#dict)&nbsp;&nbsp;&nbsp; [diff](#diff)&nbsp;&nbsp;&nbsp; [dissoc](#dissoc)&nbsp;&nbsp;&nbsp; [get](#get)&nbsp;&nbsp;&nbsp; [keys](#keys)&nbsp;&nbsp;&nbsp; [update](#update)&nbsp;&nbsp;&nbsp; [values](#values)

### Environment and i/o
[doc!](#doc)&nbsp;&nbsp;&nbsp; [flush!](#flush)&nbsp;&nbsp;&nbsp; [print!](#print)&nbsp;&nbsp;&nbsp; [prn!](#prn)&nbsp;&nbsp;&nbsp; [report!](#report)

### Errors
[assert!](#assert)&nbsp;&nbsp;&nbsp; [panic!](#panic)

### Lists
[append](#append)&nbsp;&nbsp;&nbsp; [at](#at)&nbsp;&nbsp;&nbsp; [butlast](#butlast)&nbsp;&nbsp;&nbsp; [concat](#concat)&nbsp;&nbsp;&nbsp; [count](#count)&nbsp;&nbsp;&nbsp; [each!](#each)&nbsp;&nbsp;&nbsp; [first](#first)&nbsp;&nbsp;&nbsp; [fold](#fold)&nbsp;&nbsp;&nbsp; [last](#last)&nbsp;&nbsp;&nbsp; [list](#list)&nbsp;&nbsp;&nbsp; [list?](#list)&nbsp;&nbsp;&nbsp; [map](#map)&nbsp;&nbsp;&nbsp; [ordered?](#ordered)&nbsp;&nbsp;&nbsp; [range](#range)&nbsp;&nbsp;&nbsp; [rest](#rest)&nbsp;&nbsp;&nbsp; [second](#second)&nbsp;&nbsp;&nbsp; [slice](#slice)

### Math
[abs](#abs)&nbsp;&nbsp;&nbsp; [add](#add)&nbsp;&nbsp;&nbsp; [angle](#angle)&nbsp;&nbsp;&nbsp; [atan/2](#atan/2)&nbsp;&nbsp;&nbsp; [ceil](#ceil)&nbsp;&nbsp;&nbsp; [cos](#cos)&nbsp;&nbsp;&nbsp; [dec](#dec)&nbsp;&nbsp;&nbsp; [deg/rad](#deg/rad)&nbsp;&nbsp;&nbsp; [deg/turn](#deg/turn)&nbsp;&nbsp;&nbsp; [dist](#dist)&nbsp;&nbsp;&nbsp; [div](#div)&nbsp;&nbsp;&nbsp; [div/0](#div/0)&nbsp;&nbsp;&nbsp; [div/safe](#div/safe)&nbsp;&nbsp;&nbsp; [even?](#even)&nbsp;&nbsp;&nbsp; [floor](#floor)&nbsp;&nbsp;&nbsp; [gt?](#gt)&nbsp;&nbsp;&nbsp; [gte?](#gte)&nbsp;&nbsp;&nbsp; [heading/vector](#heading/vector)&nbsp;&nbsp;&nbsp; [inc](#inc)&nbsp;&nbsp;&nbsp; [lt?](#lt)&nbsp;&nbsp;&nbsp; [lte?](#lte)&nbsp;&nbsp;&nbsp; [mod](#mod)&nbsp;&nbsp;&nbsp; [mult](#mult)&nbsp;&nbsp;&nbsp; [neg](#neg)&nbsp;&nbsp;&nbsp; [neg?](#neg)&nbsp;&nbsp;&nbsp; [odd?](#odd)&nbsp;&nbsp;&nbsp; [pi](#pi)&nbsp;&nbsp;&nbsp; [pos?](#pos)&nbsp;&nbsp;&nbsp; [rad/deg](#rad/deg)&nbsp;&nbsp;&nbsp; [rad/turn](#rad/turn)&nbsp;&nbsp;&nbsp; [random](#random)&nbsp;&nbsp;&nbsp; [range](#range)&nbsp;&nbsp;&nbsp; [round](#round)&nbsp;&nbsp;&nbsp; [sin](#sin)&nbsp;&nbsp;&nbsp; [square](#square)&nbsp;&nbsp;&nbsp; [sub](#sub)&nbsp;&nbsp;&nbsp; [sum_of_squares](#sum_of_squares)&nbsp;&nbsp;&nbsp; [tan](#tan)&nbsp;&nbsp;&nbsp; [tau](#tau)&nbsp;&nbsp;&nbsp; [turn/deg](#turn/deg)&nbsp;&nbsp;&nbsp; [turn/rad](#turn/rad)&nbsp;&nbsp;&nbsp; [zero?](#zero)

### References and state
[deref](#deref)&nbsp;&nbsp;&nbsp; [make!](#make)&nbsp;&nbsp;&nbsp; [update!](#update)

### Results
[err](#err)&nbsp;&nbsp;&nbsp; [err?](#err)&nbsp;&nbsp;&nbsp; [ok](#ok)&nbsp;&nbsp;&nbsp; [ok?](#ok)&nbsp;&nbsp;&nbsp; [unwrap!](#unwrap)&nbsp;&nbsp;&nbsp; [unwrap_or](#unwrap_or)

### Sets
[set](#set)&nbsp;&nbsp;&nbsp; [set?](#set)

### Strings
[count](#count)&nbsp;&nbsp;&nbsp; [join](#join)&nbsp;&nbsp;&nbsp; [show](#show)&nbsp;&nbsp;&nbsp; [string](#string)&nbsp;&nbsp;&nbsp; [string?](#string)

### Turtle graphics
[back!](#back)&nbsp;&nbsp;&nbsp; [background!](#background)&nbsp;&nbsp;&nbsp; [bk!](#bk)&nbsp;&nbsp;&nbsp; [clear!](#clear)&nbsp;&nbsp;&nbsp; [fd!](#fd)&nbsp;&nbsp;&nbsp; [forward!](#forward)&nbsp;&nbsp;&nbsp; [goto!](#goto)&nbsp;&nbsp;&nbsp; [heading](#heading)&nbsp;&nbsp;&nbsp; [heading/vector](#heading/vector)&nbsp;&nbsp;&nbsp; [home!](#home)&nbsp;&nbsp;&nbsp; [left!](#left)&nbsp;&nbsp;&nbsp; [lt!](#lt)&nbsp;&nbsp;&nbsp; [pc!](#pc)&nbsp;&nbsp;&nbsp; [pd!](#pd)&nbsp;&nbsp;&nbsp; [pencolor](#pencolor)&nbsp;&nbsp;&nbsp; [pencolor!](#pencolor)&nbsp;&nbsp;&nbsp; [pendown!](#pendown)&nbsp;&nbsp;&nbsp; [pendown?](#pendown)&nbsp;&nbsp;&nbsp; [penup!](#penup)&nbsp;&nbsp;&nbsp; [penwidth](#penwidth)&nbsp;&nbsp;&nbsp; [penwidth!](#penwidth)&nbsp;&nbsp;&nbsp; [position](#position)&nbsp;&nbsp;&nbsp; [pu!](#pu)&nbsp;&nbsp;&nbsp; [pw!](#pw)&nbsp;&nbsp;&nbsp; [render_turtle!](#render_turtle)&nbsp;&nbsp;&nbsp; [reset_turtle!](#reset_turtle)&nbsp;&nbsp;&nbsp; [right!](#right)&nbsp;&nbsp;&nbsp; [rt!](#rt)&nbsp;&nbsp;&nbsp; [turtle_state](#turtle_state)

### Types and values
[bool?](#bool)&nbsp;&nbsp;&nbsp; [coll?](#coll)&nbsp;&nbsp;&nbsp; [dict?](#dict)&nbsp;&nbsp;&nbsp; [eq?](#eq)&nbsp;&nbsp;&nbsp; [fn?](#fn)&nbsp;&nbsp;&nbsp; [keyword?](#keyword)&nbsp;&nbsp;&nbsp; [list?](#list)&nbsp;&nbsp;&nbsp; [neq?](#neq)&nbsp;&nbsp;&nbsp; [nil?](#nil)&nbsp;&nbsp;&nbsp; [number?](#number)&nbsp;&nbsp;&nbsp; [ordered?](#ordered)&nbsp;&nbsp;&nbsp; [show](#show)&nbsp;&nbsp;&nbsp; [some](#some)&nbsp;&nbsp;&nbsp; [some?](#some)&nbsp;&nbsp;&nbsp; [type](#type)

    
## All functions, alphabetically
[abs](#abs)&nbsp;&nbsp;&nbsp; [add](#add)&nbsp;&nbsp;&nbsp; [and](#and)&nbsp;&nbsp;&nbsp; [angle](#angle)&nbsp;&nbsp;&nbsp; [append](#append)&nbsp;&nbsp;&nbsp; [assert!](#assert)&nbsp;&nbsp;&nbsp; [assoc](#assoc)&nbsp;&nbsp;&nbsp; [assoc?](#assoc)&nbsp;&nbsp;&nbsp; [at](#at)&nbsp;&nbsp;&nbsp; [atan/2](#atan/2)&nbsp;&nbsp;&nbsp; [back!](#back)&nbsp;&nbsp;&nbsp; [background!](#background)&nbsp;&nbsp;&nbsp; [bg!](#bg)&nbsp;&nbsp;&nbsp; [bgcolor](#bgcolor)&nbsp;&nbsp;&nbsp; [bk!](#bk)&nbsp;&nbsp;&nbsp; [bool](#bool)&nbsp;&nbsp;&nbsp; [bool?](#bool)&nbsp;&nbsp;&nbsp; [butlast](#butlast)&nbsp;&nbsp;&nbsp; [ceil](#ceil)&nbsp;&nbsp;&nbsp; [clear!](#clear)&nbsp;&nbsp;&nbsp; [coll?](#coll)&nbsp;&nbsp;&nbsp; [colors](#colors)&nbsp;&nbsp;&nbsp; [concat](#concat)&nbsp;&nbsp;&nbsp; [console](#console)&nbsp;&nbsp;&nbsp; [cos](#cos)&nbsp;&nbsp;&nbsp; [count](#count)&nbsp;&nbsp;&nbsp; [dec](#dec)&nbsp;&nbsp;&nbsp; [deg/rad](#deg/rad)&nbsp;&nbsp;&nbsp; [deg/turn](#deg/turn)&nbsp;&nbsp;&nbsp; [deref](#deref)&nbsp;&nbsp;&nbsp; [dict](#dict)&nbsp;&nbsp;&nbsp; [diff](#diff)&nbsp;&nbsp;&nbsp; [dissoc](#dissoc)&nbsp;&nbsp;&nbsp; [dist](#dist)&nbsp;&nbsp;&nbsp; [div](#div)&nbsp;&nbsp;&nbsp; [div/0](#div/0)&nbsp;&nbsp;&nbsp; [div/safe](#div/safe)&nbsp;&nbsp;&nbsp; [doc!](#doc)&nbsp;&nbsp;&nbsp; [each!](#each)&nbsp;&nbsp;&nbsp; [eq?](#eq)&nbsp;&nbsp;&nbsp; [err](#err)&nbsp;&nbsp;&nbsp; [err?](#err)&nbsp;&nbsp;&nbsp; [even?](#even)&nbsp;&nbsp;&nbsp; [false?](#false)&nbsp;&nbsp;&nbsp; [fd!](#fd)&nbsp;&nbsp;&nbsp; [first](#first)&nbsp;&nbsp;&nbsp; [floor](#floor)&nbsp;&nbsp;&nbsp; [flush!](#flush)&nbsp;&nbsp;&nbsp; [fn?](#fn)&nbsp;&nbsp;&nbsp; [fold](#fold)&nbsp;&nbsp;&nbsp; [forward!](#forward)&nbsp;&nbsp;&nbsp; [get](#get)&nbsp;&nbsp;&nbsp; [goto!](#goto)&nbsp;&nbsp;&nbsp; [gt?](#gt)&nbsp;&nbsp;&nbsp; [gte?](#gte)&nbsp;&nbsp;&nbsp; [heading](#heading)&nbsp;&nbsp;&nbsp; [heading/vector](#heading/vector)&nbsp;&nbsp;&nbsp; [home!](#home)&nbsp;&nbsp;&nbsp; [inc](#inc)&nbsp;&nbsp;&nbsp; [join](#join)&nbsp;&nbsp;&nbsp; [keys](#keys)&nbsp;&nbsp;&nbsp; [keyword?](#keyword)&nbsp;&nbsp;&nbsp; [last](#last)&nbsp;&nbsp;&nbsp; [left!](#left)&nbsp;&nbsp;&nbsp; [list](#list)&nbsp;&nbsp;&nbsp; [lt!](#lt)&nbsp;&nbsp;&nbsp; [lt?](#lt)&nbsp;&nbsp;&nbsp; [lte?](#lte)&nbsp;&nbsp;&nbsp; [make!](#make)&nbsp;&nbsp;&nbsp; [map](#map)&nbsp;&nbsp;&nbsp; [mod](#mod)&nbsp;&nbsp;&nbsp; [mult](#mult)&nbsp;&nbsp;&nbsp; [neg](#neg)&nbsp;&nbsp;&nbsp; [neg?](#neg)&nbsp;&nbsp;&nbsp; [nil?](#nil)&nbsp;&nbsp;&nbsp; [not](#not)&nbsp;&nbsp;&nbsp; [odd?](#odd)&nbsp;&nbsp;&nbsp; [ok](#ok)&nbsp;&nbsp;&nbsp; [ok?](#ok)&nbsp;&nbsp;&nbsp; [or](#or)&nbsp;&nbsp;&nbsp; [ordered?](#ordered)&nbsp;&nbsp;&nbsp; [p5_calls](#p5_calls)&nbsp;&nbsp;&nbsp; [panic!](#panic)&nbsp;&nbsp;&nbsp; [pc!](#pc)&nbsp;&nbsp;&nbsp; [pd!](#pd)&nbsp;&nbsp;&nbsp; [pencolor](#pencolor)&nbsp;&nbsp;&nbsp; [pencolor!](#pencolor)&nbsp;&nbsp;&nbsp; [pendown!](#pendown)&nbsp;&nbsp;&nbsp; [pendown?](#pendown)&nbsp;&nbsp;&nbsp; [penup!](#penup)&nbsp;&nbsp;&nbsp; [penwidth](#penwidth)&nbsp;&nbsp;&nbsp; [penwidth!](#penwidth)&nbsp;&nbsp;&nbsp; [pi](#pi)&nbsp;&nbsp;&nbsp; [pos?](#pos)&nbsp;&nbsp;&nbsp; [position](#position)&nbsp;&nbsp;&nbsp; [print!](#print)&nbsp;&nbsp;&nbsp; [prn!](#prn)&nbsp;&nbsp;&nbsp; [pu!](#pu)&nbsp;&nbsp;&nbsp; [pw!](#pw)&nbsp;&nbsp;&nbsp; [rad/deg](#rad/deg)&nbsp;&nbsp;&nbsp; [rad/turn](#rad/turn)&nbsp;&nbsp;&nbsp; [random](#random)&nbsp;&nbsp;&nbsp; [range](#range)&nbsp;&nbsp;&nbsp; [render_turtle!](#render_turtle)&nbsp;&nbsp;&nbsp; [report!](#report)&nbsp;&nbsp;&nbsp; [reset_turtle!](#reset_turtle)&nbsp;&nbsp;&nbsp; [rest](#rest)&nbsp;&nbsp;&nbsp; [right!](#right)&nbsp;&nbsp;&nbsp; [round](#round)&nbsp;&nbsp;&nbsp; [rt!](#rt)&nbsp;&nbsp;&nbsp; [second](#second)&nbsp;&nbsp;&nbsp; [set](#set)&nbsp;&nbsp;&nbsp; [show](#show)&nbsp;&nbsp;&nbsp; [sin](#sin)&nbsp;&nbsp;&nbsp; [slice](#slice)&nbsp;&nbsp;&nbsp; [some](#some)&nbsp;&nbsp;&nbsp; [some?](#some)&nbsp;&nbsp;&nbsp; [square](#square)&nbsp;&nbsp;&nbsp; [string](#string)&nbsp;&nbsp;&nbsp; [string?](#string)&nbsp;&nbsp;&nbsp; [sub](#sub)&nbsp;&nbsp;&nbsp; [sum_of_squares](#sum_of_squares)&nbsp;&nbsp;&nbsp; [tan](#tan)&nbsp;&nbsp;&nbsp; [tau](#tau)&nbsp;&nbsp;&nbsp; [turn/deg](#turn/deg)&nbsp;&nbsp;&nbsp; [turn/rad](#turn/rad)&nbsp;&nbsp;&nbsp; [turtle_commands](#turtle_commands)&nbsp;&nbsp;&nbsp; [turtle_state](#turtle_state)&nbsp;&nbsp;&nbsp; [turtle_states](#turtle_states)&nbsp;&nbsp;&nbsp; [type](#type)&nbsp;&nbsp;&nbsp; [unwrap!](#unwrap)&nbsp;&nbsp;&nbsp; [unwrap_or](#unwrap_or)&nbsp;&nbsp;&nbsp; [update](#update)&nbsp;&nbsp;&nbsp; [update!](#update)&nbsp;&nbsp;&nbsp; [values](#values)&nbsp;&nbsp;&nbsp; [zero?](#zero)
## Function documentation
### abs
Returns the absolute value of a number.
```
(0)
(n)
```

### add
Adds numbers or vectors.
```
()
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
((x1, y1), (x2, y2))
```

### and
Returns true if all values passed in are truthy.
```
()
(x)
(x, y)
(x, y, ...zs)
```

### angle
Calculates the angle between two vectors.
```
(v1, v2)
```

### append
Adds an element to a list or set.
```
()
(xs as :list)
(xs as :list, x)
(xs as :set)
(xs as :set, x)
```

### assert!
Asserts a condition: returns the value if the value is truthy, panics if the value is falsy. Takes an optional message.
```
(value)
(value, message)
```

### assoc
Takes a dict, key, and value, and returns a new dict with the key set to value.
```
()
(dict as :dict)
(dict as :dict, key as :keyword, value)
(dict as :dict, (key as :keyword, value))
```

### assoc?
Returns true if a value is an associative collection: a dict, struct, or namespace.
```
(assoc as :dict)
(assoc as :struct)
(assoc as :ns)
(_)
```

### at
Returns the element at index n of a list or tuple. Zero-indexed: the first element is at index 0.
```
(xs as :list, n as :number)
(xs as :tuple, n as :number)
```

### atan/2
Returns an angle from a slope. Takes an optional keyword argument to specify units. Takes either two numbers or a vector tuple.
```
(x as :number, y as :number)
(x, y, :turns)
(x, y, :radians)
(x, y, :degrees)
((x, y))
((x, y), units as :keyword)
```

### back!
Moves the turtle backward by a number of steps. Alias: bk!
```
(steps as :number)
```

### background!
Sets the background color behind the turtle and path. Alias: bg!
```
(gray as :number)
((r as :number, g as :number, b as :number))
((r as :number, g as :number, b as :number, a as :number))
```

### bg!
Sets the background color behind the turtle and path. Alias: bg!
```
(gray as :number)
((r as :number, g as :number, b as :number))
((r as :number, g as :number, b as :number, a as :number))
```

### bgcolor
No documentation available.


### bk!
Moves the turtle backward by a number of steps. Alias: bk!
```
(steps as :number)
```

### bool
Returns false if a value is nil or false, otherwise returns true.
```
(nil)
(false)
(_)
```

### bool?
Returns true if a value is of type :boolean.
```
(false)
(true)
(_)
```

### butlast
Returns a list, omitting the last element.
```
(xs as :list)
```

### ceil
Truncates a number towards positive infinity. With negative numbers, it returns the integer part. With positive numbers, returns the next more-positive integer.
```
(n as :number)
```

### clear!
Clears the canvas and sends the turtle home.
```
()
```

### coll?
Returns true if a value is a collection: dict, struct, list, tuple, or set.
```
(coll as :dict)
(coll as :struct)
(coll as :list)
(coll as :tuple)
(coll as :set)
(coll as :ns)
(_)
```

### colors
No documentation available.


### concat
Combines two lists, strings, or sets.
```
(x as :string, y as :string)
(xs as :list, ys as :list)
(xs as :set, ys as :set)
(xs, ys, ...zs)
```

### console
No documentation available.


### cos
Returns the cosine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in.
```
(a as :number)
(a as :number, :turns)
(a as :number, :degrees)
(a as :number, :radians)
```

### count
Returns the number of elements in a collection (including string).
```
(xs as :list)
(xs as :tuple)
(xs as :dict)
(xs as :string)
(xs as :set)
(xs as :struct)
```

### dec
Decrements a number.
```
(x as :number)
```

### deg/rad
Converts an angle in degrees to an angle in radians.
```
(a as :number)
```

### deg/turn
Converts an angle in degrees to an angle in turns.
```
(a as :number)
```

### deref
Resolves a ref into a value.
```
(r as :ref)
```

### dict
Takes a struct or ns, and returns it as a dict. Or, takes a list or tuple of (key, value) tuples and returns it as a dict. Returns dicts unharmed.
```
(struct as :struct)
(ns_ as :ns)
(dict as :dict)
(list as :list)
(tup as :tuple)
```

### diff
Takes two associate data structures and returns a dict describing their differences. Does this shallowly, offering diffs only for keys in the original dict.
```
(d1 as :dict, d2 as :dict)
```

### dissoc
Takes a dict and a key, and returns a new dict with the key and associated value omitted.
```
(dict as :dict)
(dict as :dict, key as :keyword)
```

### dist
Returns the distance from the origin to a point described by (x, y).
```
(x as :number, y as :number)
((x, y))
```

### div
Divides numbers. Panics on division by zero.
```
(x as :number)
(_, 0)
(x as :number, y as :number)
(x, y, ...zs)
```

### div/0
Divides numbers. Returns 0 on division by zero.
```
(x as :number)
(_, 0)
(x as :number, y as :number)
(x, y, ...zs)
```

### div/safe
Divides a number. Returns a result tuple.
```
(x as :number)
(_, 0)
(x, y)
(x, y, ...zs)
```

### doc!
Prints the documentation of a function to the console.
```
(f as :fn)
(_)
```

### each!
Takes a list and applies a function, presumably with side effects, to each element in the list. Returns nil.
```
(f! as :fn, [])
(f! as :fn, [x])
(f! as :fn, [...xs])
```

### eq?
Returns true if all arguments have the same value.
```
(x)
(x, y)
(x, y, ...zs)
```

### err
Takes a value and wraps it in an :err result tuple, presumably as an error message.
```
(msg)
```

### err?
Takes a value and returns true if it is an :err result tuple.
```
((:err, _))
(_)
```

### even?
Returns true if a value is an even number, otherwise returns false.
```
(x as :number)
(_)
```

### false?
Returns true if a value is false, otherwise returns false. Useful to distinguish between false and nil.
```
(false)
(_)
```

### fd!
Moves the turtle forward by a number of steps. Alias: fd!
```
(steps as :number)
```

### first
Returns the first element of a list or tuple.
```
(xs)
```

### floor
Truncates a number towards negative infinity. With positive numbers, it returns the integer part. With negative numbers, returns the next more-negative integer.
```
(n as :number)
```

### flush!
Clears the console, and returns the messages.
```
()
```

### fn?
Returns true if an argument is a function.
```
(f as :fn)
(_)
```

### fold
Folds a list.
```
(f as :fn, xs as :list)
(f as :fn, xs as :list, root)
```

### forward!
Moves the turtle forward by a number of steps. Alias: fd!
```
(steps as :number)
```

### get
Takes a dict or struct, key, and optional default value; returns the value at key. If the value is not found, returns nil or the default value. Returns nil or default if the first argument is not a dict or struct.
```
(key as :keyword)
(key as :keyword, coll)
(key as :keyword, coll, default)
```

### goto!
Sends the turtle to (x, y) coordinates. If the pen is down, the turtle will draw a path to its new location.
```
(x as :number, y as :number)
((x, y))
```

### gt?
Returns true if numbers are in decreasing order.
```
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
```

### gte?
Returns true if numbers are in decreasing or flat order.
```
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
```

### heading
Returns the turtle's current heading.
```
()
```

### heading/vector
Takes a turtle heading, and returns a unit vector of that heading.
```
(heading)
```

### home!
Sends the turtle home: to the centre of the screen, pointing up. If the pen is down, the turtle will draw a path to home.
```
()
```

### inc
Increments a number.
```
(x as :number)
```

### join
Takes a list of strings, and joins them into a single string, interposing an optional separator.
```
([])
([str as :string])
(strs as :list)
([str, ...strs], separator as :string)
```

### keys
Takes an associative collection and returns a list of keys in that collection. Returns an empty list on anything other than a collection.
```
(coll)
```

### keyword?
Returns true if a value is a keyword, otherwise returns false.
```
(kw as :keyword)
(_)
```

### last
Returns the last element of a list or tuple.
```
(xs)
```

### left!
Rotates the turtle left, measured in turns. Alias: lt!
```
(turns as :number)
```

### list
Takes a value and returns it as a list. For values, it simply wraps them in a list. For collections, conversions are as follows. A tuple->list conversion preservers order and length. Unordered collections do not preserve order. Associative collections return lists of (key, value) tuples.
```
(x)
```

### lt!
Rotates the turtle left, measured in turns. Alias: lt!
```
(turns as :number)
```

### lt?
Returns true if numbers are in increasing order.
```
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
```

### lte?
Returns true if numbers are in increasing or flat order.
```
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
```

### make!
Sets the value of a ref.
```
(r as :ref, value)
```

### map
Maps over a list.
```
(f as :fn, xs)
(kw as :keyword, xs)
```

### mod
Returns the modulus of num and div. Truncates towards negative infinity.
```
(num as :number, y as :number)
```

### mult
Multiplies numbers or vectors.
```
()
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
(scalar as :number, (x, y))
((x, y), scalar as :number)
```

### neg
Multiplies a number by -1, negating it.
```
(n as :number)
```

### neg?
Returns true if a value is a negative number, otherwise returns false.
```
(x as :number)
(_)
```

### nil?
Returns true if a value is nil.
```
(nil)
(_)
```

### not
Returns false if a value is truthy, true if a value is falsy.
```
(nil)
(false)
(_)
```

### odd?
Returns true if a value is an odd number, otherwise returns false.
```
(x as :number)
(_)
```

### ok
Takes a value and wraps it in an :ok result tuple.
```
(value)
```

### ok?
Takes a value and returns true if it is an :ok result tuple.
```
((:ok, _))
(_)
```

### or
Returns true if any value passed in is truthy.
```
()
(x)
(x, y)
(x, y, ...zs)
```

### ordered?
Returns true if a value is an indexed collection: list or tuple.
```
(coll as :list)
(coll as :tuple)
(_)
```

### p5_calls
No documentation available.


### panic!
Causes Ludus to panic, outputting any arguments as messages.
```
()
(...args)
```

### pc!
Changes the turtle's pen color. Takes a single grayscale value, an rgb tuple, or an rgba tuple. Alias: pc!
```
(gray as :number)
((r as :number, g as :number, b as :number))
((r as :number, g as :number, b as :number, a as :number))
```

### pd!
Lowers the turtle's pen, causing it to draw. Alias: pd!
```
()
```

### pencolor
Returns the turtle's pen color as an (r, g, b, a) tuple.
```
()
```

### pencolor!
Changes the turtle's pen color. Takes a single grayscale value, an rgb tuple, or an rgba tuple. Alias: pc!
```
(gray as :number)
((r as :number, g as :number, b as :number))
((r as :number, g as :number, b as :number, a as :number))
```

### pendown!
Lowers the turtle's pen, causing it to draw. Alias: pd!
```
()
```

### pendown?
Returns the turtle's pen state: true if the pen is down.
```
()
```

### penup!
Lifts the turtle's pen, stopping it from drawing. Alias: pu!
```
()
```

### penwidth
Returns the turtle's pen width in pixels.
```
()
```

### penwidth!
Sets the width of the turtle's pen, measured in pixels. Alias: pw!
```
(width as :number)
```

### pi
No documentation available.


### pos?
Returns true if a value is a positive number, otherwise returns false.
```
(x as :number)
(_)
```

### position
Returns the turtle's current position.
```
()
```

### print!
Sends a text representation of Ludus values to the console.
```
(...args)
```

### prn!
Prints the underlying Clojure data structure of a Ludus value.
```
(x)
```

### pu!
Lifts the turtle's pen, stopping it from drawing. Alias: pu!
```
()
```

### pw!
Sets the width of the turtle's pen, measured in pixels. Alias: pw!
```
(width as :number)
```

### rad/deg
Converts an angle in radians to an angle in degrees.
```
(a as :number)
```

### rad/turn
Converts an angle in radians to an angle in turns.
```
(a as :number)
```

### random
Returns a random number. With zero arguments, returns a random number between 0 (inclusive) and 1 (exclusive). With one argument, returns a random number between 0 and n. With two arguments, returns a random number between m and n.
```
()
(n as :number)
(m as :number, n as :number)
```

### range
Returns the set of integers between start (inclusive) and end (exclusive) as a list. With one argument, starts at 0. If end is less than start, returns an empty list.
```
(end as :number)
(start as :number, end as :number)
```

### render_turtle!

```
()
```

### report!
Prints a value, then returns it.
```
(x)
(msg as :string, x)
```

### reset_turtle!
Resets the turtle to its original state.
```
()
```

### rest
Returns all but the first element of a list or tuple, as a list.
```
(xs as :list)
(xs as :tuple)
```

### right!
Rotates the turtle right, measured in turns. Alias: rt!
```
(turns as :number)
```

### round
Rounds a number to the nearest integer.
```
(n as :number)
```

### rt!
Rotates the turtle right, measured in turns. Alias: rt!
```
(turns as :number)
```

### second
Returns the second element of a list or tuple.
```
(xs)
```

### set
Takes an ordered collection--list or tuple--and turns it into a set.
```
(xs as :list)
(xs as :tuple)
```

### show
Returns a text representation of a Ludus value as a string.
```
(x)
```

### sin
Returns the sine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in.
```
(a as :number)
(a as :number, :turns)
(a as :number, :degrees)
(a as :number, :radians)
```

### slice
Returns a slice of a list, representing a sub-list.
```
(xs as :list, end as :number)
(xs as :list, start as :number, end as :number)
```

### some
Takes a possibly nil value and a default value. Returns the value if it's not nil, returns the default if it's nil.
```
(nil, default)
(value, _)
```

### some?
Returns true if a value is not nil.
```
(nil)
(_)
```

### square
Squares a number.
```
(x as :number)
```

### string
Converts a value to a string by using `show`. If it is a string, returns it unharmed. Use this to build up strings of differen kinds of values.
```
(x as :string)
(x)
(x, ...xs)
```

### string?
Returns true if a value is a string.
```
(x as :string)
(_)
```

### sub
Subtracts numbers or vectors.
```
()
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
((x1, y1), (x2, y2))
```

### sum_of_squares
Returns the sum of squares of numbers.
```
()
(x as :number)
(x as :number, y as :number)
(x, y, ...zs)
```

### tan
Returns the sine of an angle. Default angle measure is turns. An optional keyword argument specifies the units of the angle passed in.
```
(a as :number)
(a as :number, :turns)
(a as :number, :degrees)
(a as :number, :radians)
```

### tau
No documentation available.


### turn/deg
Converts an angle in turns to an angle in degrees.
```
(a as :number)
```

### turn/rad
Converts an angle in turns to an angle in radians.
```
(a as :number)
```

### turtle_commands
No documentation available.


### turtle_state
Returns the turtle's current state.
```
()
```

### turtle_states
No documentation available.


### type
Returns a keyword representing the type of the value passed in.
```
(x)
```

### unwrap!
Takes a result tuple. If it's :ok, then returns the value. If it's not :ok, then it panics. If it's not a result tuple, it also panics.
```
((:ok, value))
((:err, msg))
(_)
```

### unwrap_or
Takes a value that is a result tuple and a default value. If it's :ok, then it returns the value. If it's :err, returns the default value.
```
((:ok, value), _)
((:err, _), default)
```

### update
Takes a dict, key, and function, and returns a new dict with the key set to the result of applying the function to original value held at the key.
```
(dict as :dict)
(dict as :dict, key as :keyword, updater as :fn)
```

### update!
Updates a ref by applying a function to its value. Returns the new value.
```
(r as :ref, f as :fn)
```

### values
Takes an associative collection and returns a list of values in that collection. Returns an empty list on anything other than a collection.
```
(coll)
```

### zero?
Returns true if a number is 0.
```
(0)
(_)
```