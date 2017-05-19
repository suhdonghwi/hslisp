# hslisp
**hslisp** is LISP variant coded in Haskell.

hslisp is (partially) lazy-evaluted pure functional programming language.

Note there is no way to perform I/O in this language. Seperating pure computations and impure computations(I/O) appropriately is very complex problem. Think about Monad system in Haskell, and I have no ability to apply that in my language.

# 0. Examples

- Reverse alphabet list<br>
`(reverse ['a' ~ 'z'])` (reverse function is defined in stdlib.hl) <br>
→ `['z' 'y' 'x' 'w' 'v' 'u' 't' 's' 'r' 'q' 'p' 'o' 'n' 'm' 'l' 'k' 'j' 'i' 'h' 'g' 'f' 'e' 'd' 'c' 'b' 'a']`

- Solution for [Project Eulter Problem No 1.](https://projecteuler.net/problem=1) <br>
`(sum (unique (++ [3 6 ~ 999] [5 10 ~ 999])))`<br>
→ 233168

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
- Function Call. Call arguments are not evaluted in this moment.

# 3. Special-formed functions

### define : `(define <Symbol> <Expr>)`
- Defines constant to global context.
- Example :<br>
`(define ten 10)`

### lambda : `(lambda (<Symbol>...) <Expr>)`
- Returns anonymous function.

### defun : `(defun <Symbol> (<Symbol>...) <Expr>)`
- Syntactic sugar for `(define <Symbol> (lambda (<Symbol>...) <Expr>))`
- Example :<br>
`(defun add (x y) (+ x y))`

### let : `(let (<Symbol> <Expr>)+ <Expr>)`
- Defines local constants and evalutes last argument, and returns it.
- Example :<br>
`(let (a 10) (b 15) (* a b))`

# 4. Built-in normal functions

### + : `(+ <Expr>...)`
- Returns sum of arguments.
- Return type is same with type of arguments.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### - : `(- <Expr> <Expr>)`
- Returns (First argument) - (Second argument). 
- Return type is same with type of arguments.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### \* : `(\* <Expr>...)`
- Returns product of arguments.
- Return type is same with type of arguments.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### / : `(/ <Expr> <Expr>)`
- Returns (First argument) / (Second argument). 
- Return type is same with type of arguments.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### % : `(% <Expr> <Expr>)`
- Returns (First argument) % (Second argument). (modulo)
- Return type is same with type of arguments.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### = : `(= <Expr>...)`
- Returns `true`(boolean) if all the arguments are same, otherwise `false`.
- Only Integer, Real and Boolean type are allowed for argument type.
- Every arguments type must be same.

### /= : `(= <Expr>...)`
- Returns `false`(boolean) if all the arguments are same, otherwise `true`.
- Only Integer, Real and Boolean type are allowed for argument type.
- Every arguments type must be same.

### > : `(> <Expr> <Expr>)`
- Returns `true`(boolean) if (First argument) is greater than (Second argument), otherwise `false`.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### >= : `(>= <Expr> <Expr>)`
- Returns `true`(boolean) if (First argument) is greater than or equal to (Second argument), otherwise `false`.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### < : `(< <Expr> <Expr>)`
- Returns `false`(boolean) if (First argument) is greater than (Second argument), otherwise `true`.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### <= : `(<= <Expr> <Expr>)`
- Returns `false`(boolean) if (First argument) is greater than or equal to (Second argument), otherwise `true`.
- Only Integer and Real type are allowed for argument type.
- Every arguments type must be same.

### and : `(and <Expr>...)`
- Returns true(boolean) if all the arguments are `true`, other wise `false`.
- Only Boolean type is allowed for argument type.

### or : `(and <Expr>...)`
- Returns true(boolean) if one of the arguments is `true`, otherwise `false`.
- Only Boolean type is allowed for argument type.

### if : `(if <Expr> <Expr> <Expr>)`
- Returns (Second argument) if (First Argument) is true, otherwise (Third argument).
- Type of (First Argument) must be boolean.

### head : `(head <Expr>)`
- Returns first element of data list(First Argument).
- Only first element of list is evaluted, so `(head [1 ~ ])` returns 1 without any overhead.

### tail : `(tail <Expr>)`
- Returns tail(List except first element) of data list(First Argument).

### ++ : `(++ <Expr> <Expr>)`
- Returns list concatenated (First Argument) and (Second Argument), where both are data list.

### length : `(length <Expr>)`
- Returns length of (First Argument, data list).

### quote : `(quote <Expr>)`
- Nothing is evaluted, and returns first argument.

### eval : `(eval <Expr>)`
- Evalutes quoted expression (First Argument), and returns it.

### apply : `(apply <Expr> <Expr>)`
- Feeds data list (Second Argument) elements as argument to function (First Argument).
