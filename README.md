# hslisp
**hslisp** is LISP variant coded in Haskell.

# 1. Execution
- Execute with no program argument : run REPL without any task.
- Execute with arguments : Load files enumerated in program arguments, and run REPL.

For example, `runhaskell main.hs stdlib.hs`, loads stdlib.hs and runs REPL.

Of course, instead of runhaskell, you can compile and run this with Haskell compiler such as ghc.

# 2. Syntax

### Real number : `[+\-]?\d+\.\d+` 
- Floating point number literal. Uses `Double` type in Haskell.
  
### Integer number : `[+\-]?\d+`  
- Integral number literal. Uses `Integer` type in Haskell.

### Boolean literal : `(true|false)`
- Boolean literal. Uses `Bool` type in Haskell.

### Character literal : `'[^']'`
- Character literal. Uses `Char` type in Haskell.

### String literal : `<Too complex to express this in regex>`
- String literal. Almost same syntax with C-string.
- This is actually a Data List of Chars.

### Symbol : `\\\[+\-\*\/%=?><a-zA-Z][+\-\*\/%=?><\d\\]\*`
### Quote : `'<Expr>`
- Syntactic sugar for `(quote <Expr>)`

### Finite Data List : `[<Expr>...]`
- Finite data list. All the elements are evaluated when list is evaluated.
- List can contain all possible types in this language.
- Containing values of different types in a single list is also granted.

### Finite Range List : `[<Expr> ~ <Expr>]`
- Finite Range list. Only begin expression and end expression are evaluted when list is evaluted.
- Begin expression type and end expression type must be same.
- Possible types for begin/end expresson : Integer, Real, Char

### Finite Range List(2) : `[<Expr> <Expr> ~ <Expr>]`
- Finite Range list. It's almost same with above one, but this has two begin expressions.
- For instance, [1 3 ~ 9] equals to [1 3 5 7 9].
- Only two begin expressions and end expression are evaluted when list is evaluted.

### Infinite Range List : `[<Expr> ~ ]`
- Infinite Range list. It's almost same with Finite Range list, but this has no end expression, so infinite.
- Only begin expression is evaluted when list is evaluted.

### Infinite Range List(2) : `[<Expr> <Expr> ~ ]`
- Infinite Range List. It's almost same with above one, but this has two begin expressions.
- Only two begin expressions are evaluted when list is evaluted.

### Lambda : `[\λ](<Symbol>...)[.]<Expr>`
- Syntactic sugar for `(lambda (<Symbol>...) <Expr>)`

### Function Call : `(<Symbol> <Expr>...)`
