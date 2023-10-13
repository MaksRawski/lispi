# LISP I interpreter 
![coverage](https://gitlab.com/MaksRawski/lisp-interpreter/badges/main/coverage.svg)

Welcome to LISP I, the world's most beautiful programming language.
First discovered by Paul McCarthy in 1960 and described in the paper 
["Recursive functions of symbolic expressions and their computation by machine"](https://www-formal.stanford.edu/jmc/recursive.pdf).
Two years later explained in further detail in the 
[LISP I Programmer's Manual](https://bitsavers.org/pdf/mit/rle_lisp/LISP_I_Programmers_Manual_Mar60.pdf).

This project is an interpreter for the language ~~and I'm on a quest to implement every function described in the manual
(there is 90 of them listed in the "Alphabetic Index to Functions" on the page 147 of the manual).~~
Well, some of them seem to be *internal* in a way so for now I want to get enough of 
them implemented to at least be able to write the Wang Algorithm (3.4 section of the programmer's manual).

## Usage
NOTE: This project is **FAR** from being finished. Right now only a basic REPL is available.

After installing from crates.io with `cargo install lispi`, you can get access to the REPL by just running `lispi`.
In which you can for example:

``` lisp
>> (define (fac 
     (lambda (n) 
       (cond 
         ((equal n 0) 1) 
         (T (prdct n (fac (sum n -1))))))))
```
```
NIL
```


``` lisp
>> (fac 5)
```
```
120
```


## Available functions
### Elementary
- `ATOM` - checks if symbol is an atom - not a list
- `EQ`   - compares atoms
- `CAR`  - gets first element of a list
- `CDR`  - gets the "tail" of the list, everything but the first element
- `CONS` - constructs a list of 2 elements

### Special Forms
- `QUOTE` - returns its argument as a literal
- `COND`  - takes pairs (predicate expression) as its arguments and returns value of 
the expression matching the first predicate that returned T
- `AND`   - returns false if any of its arguments is F
- `OR`    - returns true if any of its arguments is T

- `LAMBDA` - allows you to define your own function! Look at an example to get a sense of how to use it
- `LABEL` - Kind of like `define` but limits the scope. 
In `(label name expression)` `name` is associated with `expression` only in the body of that expression.
Useful only for recursive functions that you don't want `define`d.

### Other
- `EQUAL`  - compares two expressions
- `SUM`    - returns the sum of two numbers
- `PRDCT`  - returns the product of two numbers

... More is coming!

## Implementation
Because I'm using rust, which is already an abstraction over assembly, I didn't have to have the exact same structure as the one shown in the manual.
Which I guess in a way makes this LISP implementation less of a List Processor. 

### symbols
(look at page 41 of programmer's manual for reference)
Instead of having a place in memory with an association list of all symbols (which in turn would have their own property lists)
I've just used enums for the built-in symbols, and during parsing I create an AST with appropriate variants. Which when `eval`'d run
the appropriate subroutines that are just regular rust functions.

### side effects
Even though LISP is a purely functional language we still need side effects for things such
as `define` which will populate the **global** association list. For that I implement a state
monad of sorts. That's because after each call to `eval`, it not only returns the expression 
but also the new association list which will be used in later calls to `eval` for the REPL.


## LICENSE
Obviously MIT. Available in the `LICENSE` file.
