# 0.3.2
+ parser
  - lines (both in files and REPL) starting with `;` will be interpreted as comments
  - `,` can now be used as a seperator - an alternative to whitespace
+ functions added:
  - TRACKLIST, ERROR
  - LIST
+ performance improved by over 16% 🥳

# 0.3.1
+ functions added:
  - CAR CDR compositions
  - NOT, NULL, AND, OR 
  - EXPT
+ parenthesis are now constantly highlighted in the REPL
+ removed history hinting

# 0.3.0
+ added file support
  + with `--load` you can load symbols defined in file into REPL

# 0.2.1
+ fix: `cons` now returns an actual cons object

# 0.2.0
+ added `define`
+ added numeric functions: sum and prdct

# 0.1.0
+ basic REPL with simple parenthesis highlighting
+ elementary functions and label and lambda only
