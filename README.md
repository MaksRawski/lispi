# LISP I interpreter 
[![coverage](https://gitlab.com/MaksRawski/lispi/badges/main/coverage.svg)](https://maksrawski.gitlab.io/lispi/)

It was the year 1960 when the brilliant mind of Paul McCarthy, from the AI reasearch team at MIT, stumbled upon something extraordinary, 
that would change the world of computing forever. What he discovered is documented in the paper
["Recursive functions of symbolic expressions and their computation by machine"](https://www-formal.stanford.edu/jmc/recursive.pdf)
in which he introduces a formalism for defining functions recursively, emphasizing its potential as both a programming language and 
a framework for computational theory. It explores the concepts of S-expressions and S-functions as well as the practial applications 
of them such as mechanical theorem proving. Because of the heavy use of lists used in the implementation it became known as LISP 
(for LISt Processor).

Two years later, in 1962, an appendix, known as the 
[LISP I Programmer's Manual](https://bitsavers.org/pdf/mit/rle_lisp/LISP_I_Programmers_Manual_Mar60.pdf) saw the light of day.
It primarily consisted of a detailed list of functions tailored for the IBM704, forming the core of what we now know as LISP I.
This implementation was more than just a system; it was a revolution waiting to unfold.

Welcome to `lispi`, an interpreter for this wonderful language. The goal of this project is to implement all functions described in the programmer's manual.
The "Alphabetic Index to Functions" on page 147 lists a total of 90 functions, however I found that only about 40 of them are intended for the user,
rest being just used in definitions of those functions and seem useless on their own.

[Available functions](#available-functions) section in this readme provides the current project status and is updated
everytime a new function is added. It can also serve as a simple reference for some of LISP I functions.

## Usage
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
fac
```


``` lisp
>> (fac 5)
```
```
120
```

You can evaluate files by running `lispi FILE.lisp`. It will evaluate S-expressions in file
sequentially and print their values. 

It might be worth noting that for the IBM704 they used `apply` as the "universal LISP function", which required you to 
specify the name of the function as the first symbol in "line"  then the arguments and then an association list which most of the time would be empty!
Because this is quite awkward to use I've decided to stick with the more modern approach where each "line" of a program is `eval`'d.

## Available functions
### Elementary
- `ATOM` - checks if symbol is an atom i.e. not a list
- `EQ`   - compares atoms
- `CAR`  - gets first element of a list
- `CDR`  - gets the "tail" of the list, everything but the first element
- any composition (of less than 3) of CAR and CDR such as `CADAR`
- `CONS` - constructs a cons object of 2 elements

### Special Forms
- `QUOTE` - returns its argument as a literal
- `COND`  - takes pairs (predicate expression) as its arguments and returns value of 
the expression matching the first predicate that returned T
- `AND`   - returns F if any of its arguments is F
- `OR`    - returns T if any of its arguments is T

- `DEFINE` - Takes pairs `(symbol value)` and assigns to each symbol corresponding value. 
When the symbol is evaluated the assigned value is returned. `DEFINE` returns the last defined symbol.
- `LABEL` - Kind of like `DEFINE` but limits the scope. Takes two arguments: `name` and a `expression` 
and associates `name` with `expression` only in the body of that expression.
- `LAMBDA` - Takes a list of dummy variables and an expression. 
When evaluated with the same number of arguments as the there are dummy variables,
it substitutes them inside the expression and then evaluates the expression itself.

### Arithmetic
- `SUM`    - returns the sum of two numbers
- `PRDCT`  - returns the product of two numbers
- `EXPT`   - returns the exponentiation of the first number raised to the power of the second number

### Other
- `NOT`    - returns T if F and F if T
- `NULL`   - returns T if NIL or 0 else F
- `EQUAL`  - compares two expressions
- `LIST`   - creates a list with the arguments provided
- `ERROR`  - allows the program to exit early
- `TRACKLIST` - Takes function names as arguments and returns them as a list.
Anytime a function that is `TRACKLIST`ed is `eval`'d two messages will be printed to the screen:
first one before evaluation with the name of the function and its arguments,
second one after evaluation with the value that it returned.


## Implementation
Because I'm using rust, which is already an abstraction over assembly, I didn't have to have the exact same structure as the one shown in the manual.
Which I guess in a way makes this LISP implementation less of a list processor. 

### Symbols
(look at page 88 of programmer's manual for reference)

Instead of having a place in memory with an association list of all symbols (which in turn would have their own property lists with pointers
to e.g. subroutines in case of builtin functions) I've just used enums for the built-in symbols, and during parsing I create an AST with appropriate variants.
The AST when `eval`'d runs the _subroutines_ that are just regular rust functions.
Symbols defined at runtime are stored in an association list where they are paired with only their value and when `eval`'d swaped with their associated value.

### Side effects
Right now the only function which produces a side effect is `DEFINE` which will populate the **global** association list.
I implemented it by means of a state monad (kinda). That's because after each call to `eval`, alongside the resulting value of the expression 
new association list is also returned which will be used in later calls to `eval` inside the REPL.

## LICENSE
Obviously MIT. Available in the `LICENSE` file.
