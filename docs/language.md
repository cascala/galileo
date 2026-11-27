<!-- syntax stamp: 800c3cb2e537f0fa -->

# The Galileo language

This is the reference for the language Galileo reads: at the `galileo>` prompt, in `.gg`
scripts, in Jupyter notebooks and in the browser playground. Every command is listed,
with its usage and an example, in the [command reference](builtins.md); inside Galileo,
`help` lists them and `help(name)` describes one.

The examples below are transcripts. A line starting with `galileo>` is what you type,
a line starting with `...>` continues it, and the lines after it are what Galileo shows.
They are shown with `explain = false`, so without the short explanations that some
commands add.

## Statements and results

Each line is a statement, and Galileo shows its result. A `;` at the end of a statement
hides the result, and `;` also separates statements on one line. `ans` is the last result
that was not assigned to a name.

```galileo
galileo> 2 + 3
5
galileo> ans * 10
50
galileo> a = 7; b = a^2
49
```

`//` starts a comment that runs to the end of the line.

```galileo
galileo> 6*7   // the answer
42
```

A statement can span lines. Inside `( )` a new line is just space, inside `[ ]` it starts
a new matrix row, and a line ending in `\` continues on the next one. The prompt waits for
the rest, and so do blocks (`for`, `while`, `if`, `function`) until their `end`.

```galileo
galileo> 1 + 2 + \
...> 3
6
galileo> max(4,
...> 9)
9
```

## Numbers

Whole numbers and fractions are exact, with no size limit. Roots and constants stay
symbolic until you ask for a number with `eval`.

```galileo
galileo> 1/3 + 2/6
2/3
galileo> 2^100
1267650600228229401496703205376
galileo> sqrt(8)
2*sqrt(2)
galileo> eval(5/4)
1.25
```

A number with a decimal point, like `0.5` or `1e-3`, is a decimal (a 64-bit float), and
anything combined with a decimal becomes one. `eval(x, digits)` gives as many significant
digits as you ask for, up to 100000, and the digits carry through `+ - * /`.

```galileo
galileo> 0.5 + 1/4
0.75
galileo> eval(pi, 30)
3.14159265358979323846264338328
```

The constants are `pi`, `e`, and `i` (or `j`) for the imaginary unit. `true` and `false`
are the two booleans, and `inf` stands for infinity in limits and integrals.

```galileo
galileo> (2 + 3*i)*(2 - 3*i)
13
galileo> i^2
-1
```

## Names and variables

`name = value` stores a value. Names start with a letter or `_`, and may contain letters,
digits and `_`. A name that has no value is a symbol, so expressions stay symbolic.

```galileo
galileo> p = (x + 1)^2
(x+1)^2
galileo> expand(p)
x^2+2*x+1
```

Names in a stored expression are looked up again each time it is used (late binding):

```galileo
galileo> f = a2*x;
galileo> a2 = 3;
galileo> f
3*x
```

`who` lists the variables you defined, and `clear` removes them all.

A one-letter name, or a letter followed by digits or `_` and more, used as a function
(`y(t)`, `f2(x)`) is an undefined function. Any other unknown function name is an error.

## Derivatives

`d(y, t)` is the derivative of the function `y` at `t`, and `d(y, t, 2)` the second
derivative. When `y` has no value it is an unknown function and the derivative stays
symbolic, so it can be stored and used later, for example in `dsolve` or `eulerlagrange`.
`d(expr, x)` and `d(expr, x, k)` differentiate any expression, like `deriv`.

```galileo
galileo> d(x^3, x, 2)
6*x
galileo> deriv(y(t)^2, t)
2*y(t)*d(y, t)
galileo> T = d(y, t)^2/2;
galileo> eulerlagrange(T - y^2/2, y, t)
d(y, t, 2) == -y
galileo> sinx(1)
Error: unknown function sinx
```

The prime is never a derivative: `y'` is the transpose of `y` (see below), and `y'(t)`
is an error that suggests `d(y, t)`.

## Operators

From loosest to tightest binding:

| Operators | Meaning |
|---|---|
| `a:b`, `a:step:b` | range |
| `\|\|` | or |
| `&&` | and |
| `==` `~=` `!=` `<` `>` `<=` `>=` | comparison; `==` between expressions is an equation |
| `+` `-` | sum, difference |
| `*` `/` `\` `.*` `./` | product, quotient, left division (`A\b` solves `A*x = b`), elementwise product and quotient |
| unary `-` `+` `!` | negation, not |
| `^` `.^` | power and elementwise power, right associative |
| `f(args)` `A[i]` `A'` `A.'` | call, index from 0, conjugate transpose, transpose |

So `-x^2` is `-(x^2)`, `2^3^2` is `2^9`, and `2^-1` is allowed.

```galileo
galileo> 2^3^2
512
galileo> -2^2
-4
galileo> 2^-1
1/2
galileo> 3 > 2 && 1 == 1
true
```

## Calling and defining functions

A function call is a name followed directly by `(`, with no space: `sin(x)`. The
[command reference](builtins.md) lists all built-in functions. A few commands also work
without parentheses: `help`, `help name`, `who`, `whos`, `clear`, `ls`, `pwd`, `cd folder`
and `exit` (or `quit`). The arguments of `load(...)`, `help(...)`, `cd(...)` and
`metric.generate(...)` are taken as text, so they need no quotes.

`f(x) := expression` (or `f(x) = expression`) defines a function in one line.

```galileo
galileo> g(x) := x^2 + 1
g(x) := x^2+1
galileo> g(3)
10
galileo> deriv(g(t), t)
2*t
```

Longer functions are blocks closed by `end`. Their variables are their own. A function
`function name(a, b)` returns with `return value`; a function written as
`function out = name(a, b)` returns the value of `out` when it ends.

```galileo
galileo> function r = steps(n)
...>   r = 0;
...>   while n ~= 1
...>     if mod(n, 2) == 0
...>       n = n/2;
...>     else
...>       n = 3*n + 1;
...>     end
...>     r = r + 1;
...>   end
...> end
galileo> steps(27)
111
```

## Matrices and vectors

Square brackets build a matrix: spaces or commas separate entries, and `;` or a new line
separates rows. Inside brackets, `[a -b]` has two entries while `[a - b]` and `[a-b]`
have one.

```galileo
galileo> A = [1 2; 3 4]
[1, 2;
 3, 4]
galileo> A'
[1, 3;
 2, 4]
galileo> [1 2*i]'
[   1;
 -2*i]
galileo> [1 2*i].'
[  1;
 2*i]
galileo> det(A)
-2
galileo> A \ [5; 6]
[ -4;
 9/2]
```

`a:b` and `a:step:b` are ranges, which make row vectors.

```galileo
galileo> 1:5
[1, 2, 3, 4, 5]
galileo> 10:-3:1
[10, 7, 4, 1]
```

`*`, `/` and `^` are the matrix operations; `.*`, `./` and `.^` work entry by entry.

```galileo
galileo> [1 2 3] .* [4 5 6]
[4, 10, 18]
galileo> [1 2 3].^2
[1, 4, 9]
```

`x(k)` counts positions from 1 and `x[k]` from 0; `end` inside the parentheses is the last
position. Assigning to a part changes it, and a vector grows when you assign past its end.

```galileo
galileo> v = [10 20 30];
galileo> v(1)
10
galileo> v[1]
20
galileo> v(end + 1) = 40
[10, 20, 30, 40]
```

A command that returns several values hands them out with `[a, b] = ...`:

```galileo
galileo> [Q, R] = qr([3 0; 4 5]);
galileo> R
[5, 4;
 0, 3]
```

## Strings

Strings are written in double quotes. `+` joins a string with anything.

```galileo
galileo> s = "x = " + 3
"x = 3"
galileo> upper("galileo")
"GALILEO"
galileo> format("{} + {} = {}", 1, 2, 3)
"1 + 2 = 3"
```

The other string commands are `str`, `num`, `split`, `join`, `replace`, `contains`,
`lower`, `trim` and `length`; `print(a, b, ...)` shows values as text.

## Lists and records

A list holds values of any kind: `{1, "a", [1 2]}`. A record has named fields:
`{mass: 3, name: "ball"}`. `p.field` reads a field, and `p.field = v` makes or changes one.

```galileo
galileo> L = {1, "a", [1 2]};
galileo> L(2)
"a"
galileo> ball = {mass: 3, name: "ball"};
galileo> ball.mass
3
galileo> ball.speed = 2;
galileo> fields(ball)
{"mass", "name", "speed"}
galileo> sq(k) := k^2;
galileo> map(sq, {1, 2, 3})
{1, 4, 9}
```

`append`, `concat`, `reverse`, `map(f, L)` and `filter(f, L)` work on lists (`f` is the
name of a function), and `fields` and `has` on records.

## Units

A number followed by a unit is a quantity: `3 m/s`, `9.81 m/s^2`, `2 kg*m/s^2`,
`1.5 kW*h`. A unit can also follow a matrix or parentheses: `[1 2 3] m`, `(a + b) kg`.
Compound units are written without spaces around `*` and `/` (`kg*m/s^2`); with spaces,
`3 m * g` multiplies `3 m` by the variable `g`. Unit names mean units only right after a
number, a matrix or `)`, so `m` and `s` stay free as variable names. Inside `[ ]`, where a
space separates entries, put quantities in parentheses: `[(1 m) (2 s)]`.

Quantities multiply and divide like any product, and units that cancel give a number.
A sum converts to the units of its first term, and adding different dimensions is an
error. `convert(q, km/h)` changes units, `convert(q)` gives SI base units, and `value(q, km/h)`
is the bare number (for plots and tables).

```galileo
galileo> v = 3 m/s;
galileo> v*(2 s)
6 m
galileo> 1 km + 300 m
1.3 km
galileo> convert(2 kg*3 m/s^2, N)
6 N
galileo> convert(100 km/h, m/s)
27.7777777777778 m/s
galileo> (3 km)/(1500 m)
2
galileo> sin(30 deg)
1/2
galileo> sqrt(16 m^2)
4 m
galileo> 3 m > 200 cm
true
galileo> [1 2 3] m
[1, 2, 3] m
```

The units are the SI ones, with the prefixes G, M, k, m, u (micro), n and p (and c for
cm): m, g, s, A, K, mol, N, J, W, Wh, Pa, Hz, C, V, ohm, F, H, T, Wb, eV, L, cal and bar;
also min, h, day, yr, atm, lb, ft, inch, mi, mph, AU, ly, and the angles rad and deg.
`const.c`, `const.G`, `const.g`, `const.h`, `const.hbar`, `const.k` (or `const.kB`), `const.NA`, `const.R`,
`const.e`, `const.me`, `const.mp`, `const.eps0`, `const.mu0`, `const.sigma`, `const.Mearth`,
`const.Rearth`, `const.Msun`, `const.Rsun` and `const.Mmoon` are physical constants with
their units, and `dimension(q)` names the dimension of a quantity.

```galileo
galileo> const.c
299792458 m/s
galileo> dimension(const.G)
"length^3/(mass*time^2)"
galileo> h = 10 m;
galileo> convert(sqrt(2*h/const.g))
1.4280869812290344 s
```

## Control flow

`if`, `for` and `while` are blocks closed by `end`. Statements inside show their results
unless they end in `;`. On one line, separate the parts with commas.

```galileo
galileo> for k = 1:3
...>   k^2
...> end
1
4
9
galileo> s = 0; for x in [2 3 5], s = s + x; end; s
10
galileo> n = 7;
galileo> if mod(n, 2) == 0, "even", else, "odd", end
"odd"
```

* `for k = range ... end` or `for x in values ... end` walks a range, a vector, the rows of
  a matrix, a list or a string.
* `while condition ... end` repeats while the condition holds.
* `if c1 ... elseif c2 ... else ... end` picks a branch. `if(c, a, b)` is a function that
  gives `a` or `b`.
* `break` leaves a loop and `continue` starts its next turn.

## Explanations

Results are always plain Galileo values, which can be pasted back in. Many commands also
add a short explanation, shown apart from the result (as `//` comments at the prompt).
`explain = false` turns explanations off, `explain = "teach"` makes them longer for
learning, and `explain = true` brings back the short ones. The environment variable
`GALILEO_EXPLAIN` sets the start: `0` for off, `teach` for teaching.

## Scripts

A script is a text file of statements, usually named `*.gg`. `load(file)` runs one as if
it were typed in (`.gg` may be left off, and a name with spaces is quoted:
`load("my file.gg")`). From a shell, `galileo file.gg` runs a script and
`galileo -i file.gg` runs it and keeps the prompt open. An error stops the script and
names the file and line: `file.gg:12: ...`.

## Files

`read` and `save` move data in and out. `save(x, "file")` picks the format from the
extension: `.csv` or `.tsv` for tables, `.png`, `.svg` or `.eps` for plots, `.tex` for
LaTeX, `.gg` for Galileo that `load` reads back, and `.txt` for text. It replaces a file
of the same name and returns the name. `read("file.csv")` gives a record of columns when
the first row holds names, and a matrix when it does not.

```galileo
galileo> d = {t: [0 1 2], y: [1 4 9]};
galileo> save(d, "squares.csv")
"squares.csv"
galileo> e = read("squares.csv");
galileo> e.y
[1, 4, 9]
galileo> save([1 2; 3 4], "m.csv");
galileo> read("m.csv")
[1, 2;
 3, 4]
galileo> latex(x^2/3)
"\frac{x^{2}}{3}"
```

`ls` lists the current folder as a list of names (folders end in `/`), `pwd` names it,
`cd folder` changes it and `rm(file)` removes a file. A name with spaces is quoted:
`rm("old data.csv")`.

```galileo
galileo> rm(m.csv)
galileo> ls
{"squares.csv"}
```

The same commands work in the terminal, in Jupyter, in JupyterLite and in the playground,
so a script that uses names relative to its folder runs unchanged in all of them. In the
browser, files live in the browser: the playground's file panel has a `+` button to add
files and buttons to download, rename and delete them, and JupyterLite's file browser does
the same for its files.

## Changes from the Scala version

* `'` is always the (conjugate) transpose and `.'` the plain transpose, as in Matlab and
  Julia; derivatives are written `d(y, t)`, never `y'`.
* Inside a matrix, `[a -b]` has two entries (as in Matlab); `[a - b]` and `[a-b]` have one.
* `exit` is not available in a notebook.
