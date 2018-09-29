# hse-parsing
A simple recursive descent parser. Written for the formal languages course in HSE. The language is described with the following grammar:

```
S -> Expr | \epsilon

Expr -> Ident = Expr
      | Term restExpr

restExpr -> (+|-) Term restExpr | \epsilon

Term -> Power restTerm

restTerm -> (*|/) Power restTerm | \epsilon

Power -> Factor (^ Power)?

Factor -> Ident 
        | Num 
        | '(' Expr ')'
        | '-' Factor
Char -> ('a' | 'b' | ... | 'z')

Ident -> Char (Char | Digit)*

Digit -> ('0' | '1' | ... | '9')

Num -> (Digit)+
```

Running the build script `build.sh` generates an executable `Main`. `Main` parses several inputs specified and terminates.

To run parser on your input, load `Main` into the interpreter `ghci` and execute `parse <input>` or modify `Main.hs`.

This should work on any version of the haskell compiler, but has only been tested on `ghc 8.4.2`.
