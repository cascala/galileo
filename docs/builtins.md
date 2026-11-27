<!-- Generated from src/help.rs: GALILEO_BLESS=1 cargo test --test reference -->

# Galileo commands

Every built-in command, by topic. Inside Galileo, `help` lists them and `help(name)` shows the same text as here. How statements, operators, functions, matrices, strings, lists and blocks are written is described in the [language reference](language.md).

[Basics](#basics) · [Files](#files) · [Functions](#functions) · [Algebra](#algebra) · [Calculus](#calculus) · [Matrices](#matrices) · [Plotting](#plotting) · [Language](#language) · [Complex numbers](#complex-numbers) · [Number theory](#number-theory) · [Factoring lab](#factoring-lab) · [Groups](#groups) · [Rings and fields](#rings-and-fields) · [Statistics](#statistics) · [Complex analysis](#complex-analysis) · [Differential equations](#differential-equations) · [Differential geometry](#differential-geometry) · [Discovery](#discovery) · [Multivariable calculus](#multivariable-calculus) · [Special functions](#special-functions) · [Units](#units) · [Signals](#signals) · [Graphs](#graphs) · [Random processes](#random-processes) · [Partial differential equations](#partial-differential-equations) · [Calculus of variations](#calculus-of-variations)

## Basics

### help

`help or help(name)`

Lists all commands, or describes one command.

Example: `help(deriv)`

### explain

`explain = "brief", explain = "teach", explain = "off"`

Results are always plain Galileo values; explain sets how much commands add to them, as one of three levels. explain = "brief" (the default) adds a short explanation, shown apart from the result (as // comments in plain text). explain = "teach" gives longer explanations for learning: they walk through the method step by step with the numbers of this run, say why each step is needed and what to notice (gnfs, nfspoly, polyselect and solve so far; other commands give their full help text). explain = "off" turns explanations off for the session. Any other value, true and false included, is an error. The environment variable GALILEO_EXPLAIN sets the starting level: off, brief or teach. A trailing ; hides both.

Example: `explain = "off"`

### exactdigits

`exactdigits = 38, exactdigits = 0`

Typed decimals hold their exact value: 0.1 is exactly one tenth, and + - * / and whole powers of decimals are exact, so 0.1 + 0.2 - 0.3 is 0.0 and 0.1*x + 0.2*x == 0.3*x is true. A result with no finite decimal form is an exact fraction: 0.1/3 is 1/30 (eval gives its digits). A decimal stays exact while its numerator and denominator have at most exactdigits digits; beyond that it becomes a 64-bit float (about 16 digits). The default and the largest value is 38; exactdigits = 0 makes every decimal a float. Functions like sqrt, sin and exp, and powers like 0.5^0.5, always give floats.

Example: `exactdigits = 10`

### who

`who`

Lists the variables you defined, with their types (whos does the same).

Example: `who`

Also found as `whos`.

### clear

`clear`

Removes all variables you defined.

Example: `clear`

### eval

`eval(expr), eval(expr, digits)`

Evaluates an expression numerically, for example a fraction or a constant like pi. eval(expr, digits) gives that many significant digits (up to 100000): eval(pi, 50). The result is a decimal that keeps its digits through + - * / with exact numbers and with other such decimals (the smaller digit count wins); with an ordinary decimal, or inside a function like sin, it becomes an ordinary decimal (about 16 digits). A decimal typed with more than 17 digits keeps them all. Works for numbers, pi, e, + - * / ^, sqrt, exp, log, the trigonometric and hyperbolic functions and their inverses, entry by entry for matrices.

Example: `eval(sqrt(2), 40)`

### info

`info(expr)`

Shows the internal structure of an expression.

Example: `info(x^2+1)`

### load

`load(file), load(RSA)`

Runs the Galileo statements in a file and shows their results, as if they were typed in (a statement ending in ; stays quiet). Quote a name with spaces, load("my file.gg"); .gg may be left off. A file can load others, found next to it. An error stops the file and says where: file.gg:12: ... In the browser, load reads the files of the playground's file panel or of JupyterLite's file browser. In Docker, mount the folder first: docker run -it --rm -v "$PWD:/work" cascala/galileo. load(RSA) instead defines the RSA Factoring Challenge numbers: RSA100, RSA110, ... RSA617 by decimal digits and RSA576, RSA640, RSA704, RSA768, RSA896, RSA1024, RSA1536, RSA2048 by bits, with the two primes of each factored one as RSA100p and RSA100q and so on. Factors are included up to RSA250, plus RSA270 and RSA896 (RSA-896, factored in September 2026, is the largest so far); RSA260 was factored in September 2026 too, but its factors are not included yet.

Example: `load(RSA)`

### define

`f(x) := expr, f(x, y) := expr (or f(x) = expr)`

Defines your own function. Call it like a built-in one: f(3), f(t + 1). d(f, x) and d(f, x, 2) are its derivatives at x. Functions can call themselves together with if(...). ans in the body is replaced by the last result when the function is defined, so deriv(f(x), x) followed by g(x) := ans makes g the derivative.

Example: `f(x) := x^2 + 1; f(3)`

Also found as `:=`.

### if

`if(condition, a, b), or the block if condition ... elseif condition ... else ... end`

As a block, runs the statements of the first branch whose condition is true (a matrix counts as true when all its entries do); elseif and else are optional, and end closes it. Inside a block, statements show their results unless they end in ;. As a function, if(condition, a, b) is a when the condition is true, b otherwise; only the chosen branch is computed, so recursive functions stop. When the recursive call is the whole branch, as in count(n, k) := if(k == n, k, count(n, k + 1)), it is run as a loop and may take millions of steps. Conditions use == ~= < > <= >= and && || !.

Example: `fact(n) := if(n == 0, 1, n*fact(n-1)); fact(10)`

Also found as `elseif`, `else`.

## Files

### read

`read("file")`

Reads a file from the current folder. A .csv or .tsv table whose first row has names gives a record of columns: d = read("data.csv"), then d.time and d.temp are its columns (vectors of numbers, or lists of strings for text). Names are made fit to type: "temp (C)" becomes temp_C. Without a header row the table is a matrix. The separator (comma, semicolon or tab) is found from the first line; quoted cells may hold commas; an empty cell is NaN. Any other file comes back as one string. In the browser, add files with the + button of the playground's file panel, or the upload button of JupyterLite's file browser.

Example: `save([1 2; 3 4], "m.csv"); read("m.csv")`

### save

`save(x, "file"), save("file.gg")`

Writes x to a file, in the format its extension names, replacing a file of that name: .csv or .tsv for tables (a matrix row by row, a record as named columns, a list as one column), .png, .svg or .eps for a plot, 3D plot or graph (PNG at twice the size on screen; EPS keeps lines and text sharp at any size, with see-through colors mixed with white), .tex for LaTeX (as display math \[ ... \]), .gg for Galileo that load reads back (x = value), and .txt for text. save("file.gg") writes all your variables and f(x) := ... functions. Returns the file name. In the browser the file shows up in the playground's file panel, or in JupyterLite's file browser, where it can be downloaded.

Example: `save(plot(sin(x), x, 0, 2*pi), "sine.png")`

### ls

`ls, ls("folder")`

The names of the files in the current folder (or another one), as a list of strings; folder names end in /. pwd gives the current folder, cd folder (or cd("folder")) changes it, and cd .. goes up.

Example: `ls`

Also found as `cd`.

### pwd

`pwd`

The current folder, as a string. Files named without a folder are read and written there.

Example: `pwd`

### rm

`rm(file)`

Removes a file (not a folder): rm(old.csv), or rm("my file.csv") with quotes for a name with spaces.

Example: `save(1, "tmp.txt"); rm(tmp.txt)`

### latex

`latex(x)`

The LaTeX of a value, as a string: an expression, a matrix (as a pmatrix), a list or a record. save(x, "file.tex") writes it to a file.

Example: `latex(x^2/3 + sqrt(y))`

## Functions

### sin

`sin(x), cos(x), tan(x), sec(x), csc(x), cot(x)`

Trigonometric functions, in radians. Exact at multiples of pi/12 (15 degrees). sec, csc and cot are 1/cos, 1/sin and 1/tan, and are written that way.

Example: `sin(pi/6)`

Also found as `cos`, `tan`.

### asin

`asin(x), acos(x), atan(x)`

Inverse trigonometric functions.

Example: `asin(1)`

Also found as `acos`, `atan`.

### exp

`exp(x)`

Exponential function.

Example: `exp(1)`

### log

`log(x)`

Natural logarithm.

Example: `log(e)`

### sqrt

`sqrt(x)`

Square root; sqrt(-1) is the imaginary unit i.

Example: `sqrt(16)`

### abs

`abs(x)`

Absolute value; for a complex number a + b*i the modulus sqrt(a^2 + b^2).

Example: `abs(-3)`

### sinh

`sinh(x), cosh(x), tanh(x), asinh(x), acosh(x), atanh(x), sech(x), csch(x), coth(x)`

Hyperbolic functions and their inverses. sech, csch and coth are 1/cosh, 1/sinh and 1/tanh, and are written that way.

Example: `deriv(tanh(x), x)`

Also found as `cosh`, `tanh`, `asinh`, `acosh`, `atanh`.

### atan2

`atan2(y, x)`

Angle of the point (x, y), in (-pi, pi].

Example: `atan2(1.0, -1.0)`

### functions

`a(t), A(r), f(x, y)`

A call to an unknown one-letter name (optionally with digits or a subscript, like a_1) is an undefined function. deriv uses the chain rule: deriv(a(t)^2, t) is 2*a(t)*d(a, t), where d(a, t) is the derivative of a at t. Partial derivatives of functions of several variables are written d(f(x, y), x).

Example: `deriv(a(t)^2, t)`

### max

`max(a, b, ...), min(a, b, ...)`

Largest or smallest of numbers (matrix entries count individually).

Example: `max(1, 2/3, pi)`

Also found as `min`.

### factorial

`factorial(n), binomial(n, k)`

n! and the binomial coefficient n choose k, exact.

Example: `binomial(10, 3)`

Also found as `binomial`.

### floor

`floor(x), ceil(x), round(x), round(x, d)`

Round down, up, or to the nearest whole number; round(x, d) keeps d decimals. Matrices are rounded entry by entry.

Example: `round(2.5)`

Also found as `ceil`, `round`.

### mod

`mod(a, b), gcd(a, b), lcm(a, b), gcd(f, g, p)`

Remainder of a / b (with the sign of b), greatest common divisor, least common multiple. gcd of two polynomials in one variable gives their monic greatest common divisor over the rationals (Euclid's algorithm), or modulo a prime p with gcd(f, g, p).

Example: `mod(17, 5)`

Also found as `gcd`, `lcm`.

## Algebra

### simplify

`simplify(expr)`

Combines fractions, cancels common factors and applies sin^2 + cos^2 = 1, keeping the simplest form.

Example: `simplify(sin(y)^2 + cos(y)^2)`

### expand

`expand(expr)`

Multiplies out products and powers.

Example: `expand((a+b)^2)`

### factor

`factor(expr), factor(expr, field)`

Factors a polynomial with rational coefficients over the integers, in any variables: (a*x + b)*(c*x + d) comes back from its expanded form. factor(expr, n) factors modulo a prime n (factor(x^4 + 1, 17)); factor(expr, sqrt(d)) over the numbers a + b*sqrt(d), with i for the Gaussian numbers (factor(x^2 + y^2, i)); these two take one variable, or a homogeneous polynomial in two. Irreducible factors of any degree are found (Zassenhaus: factor mod a prime, lift, recombine); x^n - 1 and x^n + 1 split into cyclotomic polynomials. Other expressions get their common factors pulled out. A whole number of any size is factored into primes, given as rows [prime, exponent]: factor(360) is [2, 3; 3, 2; 5, 1], and the explanation shows 2^3 * 3^2 * 5 (trial division, Pollard p - 1 and rho). A number a^n + b^n or a^n - b^n (typed that way, or recognized as b^n + 1 or b^n - 1) is first cut into its algebraic pieces, values of cyclotomic polynomials split again by Aurifeuillean factors, and then each piece is factored. A piece that does not split in about a second gets a row of its own with the whole composite number; the explanation calls it C followed by its number of digits.

Example: `factor(x^4 + 4)`

### subs

`subs(expr, x, value)`

Replaces the variable x by a value. x can also be an expression that occurs in expr, such as a derivative: subs(m*d(x, t)^2/2, d(x, t), v) gives m*v^2/2.

Example: `subs(x^2 + 1, x, 3)`

### solve

`solve(f, x), solve(f, x, N) or solve([f1 f2], [x y])`

Solves f = 0 (or an equation lhs == rhs) for x and returns all solutions in a row. An equation in one of sin(x), cos(x), tan(x) gives the solutions with 0 <= x < 2*pi. Polynomials are solved exactly up to degree 4: rational roots first, then the quadratic formula, Cardano's formula for cubics (written with cos when all three roots are real) and Ferrari's method for quartics, also with symbolic coefficients. Higher degrees are factored over the rationals and the pieces of degree 4 or less are solved exactly; numeric (complex) roots otherwise. explain = "teach" walks through the formula with the numbers of the equation. Rational functions work too. solve(f, x, N) gives the roots of a polynomial in Z/N (integers mod N): roots mod each prime with gcd(f, x^p - x), lifted to prime powers (Hensel) and combined (Chinese remainder theorem). With a list of equations and unknowns, solves a linear system, or a polynomial system (by substitution, factoring and resultants), giving the real solutions one per row. For other equations use fsolve.

Example: `solve(x^3 - 3*x + 1, x)`

### fsolve

`fsolve(f, x, guess) or fsolve([f1 f2], [x y], [x0 y0])`

A numeric solution of f = 0 near the guess (Newton's method); for a system, Newton's method with the Jacobian.

Example: `fsolve(cos(x) - x, x, 1)`

### coeff

`coeff(p, x, n), degree(p, x)`

Coefficient of x^n in the polynomial p, and the degree of p in x.

Example: `coeff((x+1)^3, x, 2)`

Also found as `degree`.

### numer

`numer(f), denom(f)`

Numerator and denominator after bringing f over a common denominator.

Example: `denom(1/x + 1/y)`

Also found as `denom`.

### complexity

`complexity(expr)`

Size of an expression (number of nodes).

Example: `complexity(a*b + a*c)`

### and

`and(a, b), or(a, b), xor(a, b), not(a); also a && b, a || b, !a`

Logic on true, false and numbers (nonzero is true).

Example: `and(true, false)`

Also found as `or`, `xor`, `not`.

### equations

`lhs == rhs`

Compares two values: true or false when that can be decided, otherwise an equation, which solve, fsolve, implicitplot and dsolve accept.

Example: `solve(x^2 == 2*x + 3, x)`

Also found as `==`.

### polydiv

`polydiv(p, q, x)`

Long division of polynomials in x: [quotient, remainder] with p = quotient*q + remainder and the remainder of lower degree than q.

Example: `polydiv(x^3 - 1, x - 1, x)`

### trigexpand

`trigexpand(expr), trigreduce(expr)`

trigexpand applies the addition formulas: sin(a+b), cos(2x), tan(a+b), ... in terms of sines and cosines of the single angles. trigreduce goes the other way: products and powers of sin and cos become sums of sines and cosines of multiple angles.

Example: `trigexpand(cos(2*x))`

Also found as `trigreduce`.

## Calculus

### d

`d(y, t), d(y, t, k), d(expr, x), d(expr, x, k)`

The derivative. d(y, t) is the derivative of the function y at t, and d(y, t, 2) the second derivative: y may be unknown (it then stays symbolic, so T = d(y, t)^2/2 can be stored and used later in dsolve or eulerlagrange), a function you defined (d(f, 2) is f'(2)), or a variable holding an expression. d(expr, x, k) differentiates any expression k times with respect to x, like deriv. The prime ' is never a derivative: A' is the transpose.

Example: `d(y, t, 2) + y`

### deriv

`deriv(expr, x)`

Derivative of expr with respect to the variable given (other letters count as constants: a partial derivative).

Example: `deriv(x^3 - 2*x, x)`

Also found as `diff`.

### integrate

`integrate(f, x) or integrate(f, x, a, b)`

Antiderivative of f with respect to x (without the constant; log(x) stands for log|x|), or the definite integral from a to b. a and b can be inf or -inf; over the whole line it also does a polynomial times e^(quadratic) and, by residues, a rational function times cos(kx) or sin(kx). If no formula is found, a definite integral with numeric limits is computed numerically. int is the same.

Example: `integrate(x*exp(x), x)`

Also found as `int`.

### nintegrate

`nintegrate(f, x, a, b)`

Numeric definite integral (adaptive Gauss-Kronrod); a and b can be inf or -inf.

Example: `nintegrate(exp(-x^2), x, -inf, inf)`

### limit

`limit(f, x, a) or limit(f, x, a, left), limit(f, x, a, right)`

Limit of f as x approaches a, from both sides or one side. a can be inf or -inf. Uses l'Hopital's rule for 0/0 and inf/inf.

Example: `limit(sin(x)/x, x, 0)`

### taylor

`taylor(f, x, a, n) or taylor(f, x, n)`

Taylor polynomial of f around x = a (default 0) up to the term (x-a)^n. series is the same.

Example: `taylor(exp(x), x, 0, 4)`

Also found as `series`.

### sum

`sum(f, k, a, b), prod(f, k, a, b), sum(A)`

Sum or product of f for k = a, a+1, ..., b; sum(A) adds the entries of a matrix. With a symbolic or infinite upper limit, sum finds closed forms for polynomials (sum(k^2, k, 1, n)), geometric series and 1/k^2, 1/k^4, 1/k^6.

Example: `sum(1/k^2, k, 1, 10)`

Also found as `prod`, `product`.

### odesolve

`odesolve(rhs, unknowns, t, start, t0, t1 (, steps))`

Solves the system of first-order equations dy/dt = rhs numerically (Runge-Kutta) from t = t0 to t = t1 and returns the unknowns at t1: a number for one unknown, a row otherwise. The arguments are the same as the first six of odeplot; see help(odeplot).

Example: `odesolve([v, -x], [x v], t, [1 0], 0, pi)`

### odeplot

`odeplot(rhs, unknowns, t, start, t0, t1, [X Y] (, steps))`

Solves dy/dt = rhs numerically and plots the solution. The arguments: (1) rhs, a row with the right-hand side of each equation, dy1/dt, dy2/dt, ...; (2) unknowns, the names y1, y2, ... in the same order; (3) t, the name of the independent variable; (4) start, the values of y1, y2, ... at t0; (5, 6) t0 and t1, where the solution starts and stops; (7) [X Y], what to draw: two expressions in t and the unknowns for the horizontal and vertical axes ([t y1] plots y1 against t, [y1 y2] is a phase portrait, [r*cos(phi) r*sin(phi)] turns polar coordinates into a picture), or three for a 3D curve; (8) optionally the number of Runge-Kutta steps (default 20000). The equations must be first order: write d(y, t, 2) = f as two equations, d(y, t) = w and d(w, t) = f, with unknowns [y w].

Example: `odeplot([v, -x], [x v], t, [1 0], 0, 2*pi, [t x])`

### table

`table(expr, k, a, b (, step)) or table(expr, k, [v1, v2, ...])`

A row with expr for each k; when expr is itself a row, the rows stack into a matrix.

Example: `table(k^2, k, 1, 10)`

### iterate

`iterate(f, x, x0, n)`

x0, f(x0), f(f(x0)), ... : n steps of x -> f(x), where f is an expression in x.

Example: `iterate(x/2 + 1/x, x, 1.0, 5)`

### dsolve

`dsolve(ode, y, x) or dsolve(ode, y, x, x0, [y(x0), d(y, x0), ...])`

Exact solution of a differential equation written with y, d(y, x), d(y, x, 2), ...: linear with constant coefficients of any order (forcing by polynomials, exponentials, sines and cosines), first-order linear y' + p(x) y = q(x), separable y' = f(x) g(y) (logistic, y' = x/y, ...), and Cauchy–Euler x^2 y'' + b x y' + c y = 0. A list of first-order equations with a list of unknowns solves a linear system: dsolve([d(x, t) == y, d(y, t) == -x], [x, y], t, 0, [1, 0]). Without start values the answer has constants C1, C2, ....

Example: `dsolve(d(y, x) == y*(1 - y/10), y, x, 0, 1)`

### odedata

`odedata(rhs, unknowns, t, start, t0, t1, n)`

Like odesolve, but returns the whole solution as a table with n+1 rows [t y1 y2 ...], evenly spaced from t0 to t1, ready for scatter or discover.

Example: `odedata([v, -x], [x v], t, [1 0], 0, 6, 60)`

### orbitdata

`orbitdata(rule, x, x0, r, r0, r1, nr, skip, keep)`

For nr+1 values of the parameter r from r0 to r1: iterate x -> rule (an expression in x and r) from x0, throw away the first skip values and keep the next keep values. Returns rows [r x] for scatter: a bifurcation diagram. Computed with decimals.

Example: `scatter(orbitdata(r*x*(1-x), x, 0.5, r, 2.8, 4, 200, 500, 25), [r x])`

## Matrices

### inv

`inv(A)`

Inverse of a matrix; works with symbolic entries. Matrices are written [a b; c d]. To solve A x = b use A\b, which also gives the least squares solution when A has more rows than columns.

Example: `A = [a b; c d]; inv(A)`

### det

`det(A)`

Determinant.

Example: `det([a b; c d])`

### qr

`[Q, R] = qr(A)`

QR factorization A = Q*R: Gram-Schmidt on the columns of A gives Q with orthonormal columns (Q^T*Q = I), and R is upper triangular with R[i, j] = q_i . a_j. Exact (with square roots) for exact entries. The columns of A must be independent. Least squares A x = b becomes R x = Q^T b.

Example: `[Q, R] = qr([1 1; 1 0; 0 1])`

### gramschmidt

`gramschmidt(A)`

Orthonormal basis of the column space of A (Gram-Schmidt), as the columns of a matrix; dependent columns are skipped. Also orth(A).

Example: `gramschmidt([1 1; 1 0; 0 1])`

Also found as `orth`.

### chol

`chol(A)`

Cholesky factorization of a symmetric positive definite matrix: the lower triangular L with A = L*L^T and a positive diagonal. Gives an error, naming the first pivot that is not positive, when A is not positive definite: this is the quickest test.

Example: `chol([4 2; 2 3])`

Also found as `cholesky`.

### posdef

`posdef(A)`

true when A is symmetric positive definite (x^T*A*x > 0 for every x not 0), tested by elimination: every pivot must be positive. The note lists the pivots. Also isposdef(A).

Example: `posdef([2 -1; -1 2])`

Also found as `isposdef`.

### cond

`cond(A)`

Condition number: the largest singular value divided by the smallest (inf for a singular matrix). Solving A x = b can magnify relative errors in b by this factor.

Example: `cond([1 1; 1 1.0001])`

### svd

`svd(A) or [U, S, V] = svd(A)`

Singular value decomposition A = U*S*V^T: U and V orthogonal, S diagonal with the singular values s1 >= s2 >= ... >= 0 (square roots of the eigenvalues of A^T*A). svd(A) gives the singular values. Exact when A^T*A has rational eigenvalues, numeric otherwise. The number of nonzero singular values is the rank, and keeping the k largest gives the best rank-k approximation.

Example: `[U, S, V] = svd([3 0; 4 5])`

### jordan

`jordan(A) or [P, J] = jordan(A)`

Jordan form A = P*J*P^-1: J is block diagonal with eigenvalues on the diagonal and 1s just above it inside a block. All blocks 1x1 means A is diagonalizable. The columns of P are eigenvectors and generalized eigenvectors. Needs exact eigenvalues.

Example: `[P, J] = jordan([2 1; 0 2])`

### expm

`expm(A) or expm(A, t)`

Matrix exponential e^A = I + A + A^2/2! + ..., or e^(A t) with a symbol t: exact from the Jordan form (complex eigenvalues a +- bi give e^(a t) cos(b t) and e^(a t) sin(b t)), numeric for decimal entries. x(t) = expm(A, t)*x0 solves d(x, t) = A x with x(0) = x0.

Example: `expm([0 -1; 1 0], t)`

### kron

`kron(A, B)`

Kronecker (tensor) product: each entry a_ij of A becomes the block a_ij*B.

Example: `kron([1 2; 3 4], eye(2))`

### diag

`diag(v) or diag(A)`

diag of a vector is the diagonal matrix with those entries; diag of a matrix is the row of its diagonal entries.

Example: `diag([1 2 3])`

### lu

`[L, U, P] = lu(A)`

LU factorization with pivoting, P*A = L*U (also [L, U] = lu(A) with A = L*U).

Example: `[L, U, P] = lu([1 2; 3 4])`

### transpose

`A', A.' or transpose(A)`

Transpose of a matrix. A' is the conjugate transpose (complex entries are conjugated, as in MATLAB); A.' and transpose(A) only swap rows and columns. For a derivative use d(y, t).

Example: `[1 2; 3 4]'`

### backslash

`A \ b`

Solves the linear system A*x = b for the column x.

Example: `[2 1; 1 3] \ [3; 5]`

### trace

`trace(A), size(A)`

Sum of the diagonal entries; size gives [rows columns].

Example: `trace([1 2; 3 4])`

Also found as `size`.

### eig

`eig(A), charpoly(A, x)`

Eigenvalues of a square matrix (exact when the characteristic polynomial factors or has degree 2, numeric otherwise), and the characteristic polynomial det(A - x*I). A symmetric matrix of numbers with a decimal entry, or with 5 or more rows, is diagonalized numerically by Jacobi rotations: eigenvalues in increasing order, unit eigenvectors from [V, D] = eig(A).

Example: `eig([2 1; 1 2])`

Also found as `charpoly`.

### dot

`dot(u, v), cross(u, v)`

Dot product of two vectors, and cross product of two vectors of length 3.

Example: `cross([1 0 0], [0 1 0])`

Also found as `cross`.

### norm

`norm(A), norm(A, 1), norm(A, inf), norm(A, fro)`

Matrix norms: 2-norm (largest singular value, the default), 1-norm (largest column sum), inf-norm (largest row sum), Frobenius.

Example: `norm([1 -2; 3 4], 1)`

### eye

`eye(n) or eye(n, m)`

Identity matrix.

Example: `eye(3)`

### ones

`ones(n) or ones(n, m), zeros(n) or zeros(n, m)`

Matrix of ones or zeros.

Example: `ones(2, 3)`

Also found as `zeros`.

### rand

`rand(n) or rand(n, m)`

Random matrix with entries uniform in [0, 1).

Example: `rand(2)`

### range

`a:b or a:step:b`

Row vector from a to b. Exact for exact numbers.

Example: `0:1/4:1`

### linspace

`linspace(a, b) or linspace(a, b, n)`

Row vector of n evenly spaced values from a to b (default 100).

Example: `linspace(0, 1, 5)`

### logspace

`logspace(a, b) or logspace(a, b, n)`

Row vector of n values from 10^a to 10^b (default 50).

Example: `logspace(0, 2, 3)`

### index

`A[i], A[i, j], g[a, b], christoffel(g)[a, b, c]`

Indexing starts at 0. Works on matrices, metrics and tensor components.

Example: `christoffel(metric.generate(polar))[1, 0, 1]`

### elementwise

`A .* B, A ./ B, A .^ B`

Entry-by-entry product, quotient and power of two matrices of the same size, or of a matrix and a number (1 ./ [1 2 4] is [1, 1/2, 1/4]). * / ^ on matrices are the matrix operations; + and - are entry by entry already.

Example: `[1 2 3] .* [4 5 6]`

Also found as `.*`, `./`, `.^`.

### transform

`transform(data, [names], [expr1 expr2 ...])`

Makes a new table from a data table: the names label the columns of data, and each expression (in those names) gives one column of the result, computed row by row. Use it to change units, take logarithms or compute 1/r.

Example: `transform([1 2; 2 4], [x y], [x 1/y])`

### indexing

`A(k), A(i, j)`

Picks from a stored table or list, counting from 1: A(k) is entry k of a list (row or column) or row k of a table, A(i, j) the entry in row i, column j.

Example: `v = [3 5 7]; v(2)`

### rows

`rows(A), cols(A), length(v)`

The number of rows or columns of a table; length is the number of entries of a list (the larger of the two).

Example: `rows(generate([2 3 1]))`

Also found as `cols`.

### rref

`rref(A), rank(A), null(A)`

Row reduction: rref is the reduced row echelon form, rank the number of independent rows, null a basis of the solutions of A*x = 0 (one vector per column; empty when x = 0 is the only solution).

Example: `rref([1 2 3; 4 5 6; 7 8 9])`

Also found as `rank`, `null`.

### eigvec

`eigvec(A), [V, D] = eig(A)`

Eigenvectors, one per column, for exact eigenvalues. [V, D] = eig(A) gives the eigenvectors V and the diagonal matrix D of eigenvalues, so that A = V*D*inv(V).

Example: `[V, D] = eig([2 1; 1 2])`

### column

`column(A, k)`

Column k of a matrix (counting from 1), as a column vector.

Example: `column([1 2; 3 4], 2)`

## Plotting

### plot

`plot(f1, f2, ..., x, from, to) or plot(f, x)`

Plots one or more expressions of x. The range defaults to -10 .. 10.

Example: `plot(sin(x), cos(x), x, 0, 2*pi)`

### animate

`animate(f, x, a, b, t, t0, t1[, frames]), animate(fx, fy, t, t0, t1[, frames]), animate(xs, ys)`

An animated plot with a play button and a time slider: the curve y = f(x, t) as t runs from t0 to t1 (a wave), a point moving along (fx(t), fy(t)) with its path, or a point moving through data such as a numeric solution. 60 frames unless given. On paper (SVG) later frames are drawn faintly under the first. Combine with show, label with legend, labels and title.

Example: `animate(sin(x - t), x, 0, 10, t, 0, 2*pi)`

### parametric

`parametric(fx, fy, t, from, to)`

Plots the curve (fx(t), fy(t)), with equal scales on both axes.

Example: `parametric(cos(3*t), sin(2*t), t, 0, 2*pi)`

### plot3d

`plot3d(f, x, a, b, y, c, d)`

Plots the surface z = f(x, y) over a <= x <= b, c <= y <= d.

Example: `plot3d(sin(x)*cos(y), x, -pi, pi, y, -pi, pi)`

### surface

`surface(fx, fy, fz, u, a, b, v, c, d)`

Plots a parametric surface.

Example: `surface(sin(u)*cos(v), sin(u)*sin(v), cos(u), u, 0, pi, v, 0, 2*pi)`

### parametric3d

`parametric3d(fx, fy, fz, t, from, to)`

Plots a curve in space.

Example: `parametric3d(cos(t), sin(t), t/(2*pi), t, 0, 6*pi)`

### polarplot

`polarplot(r, phi, from, to)`

Plots the curve at distance r(phi) from the origin, for angles phi from..to.

Example: `polarplot(1 + cos(phi), phi, 0, 2*pi)`

### window

`window(p, x0, x1, y0, y1)`

A copy of the 2D plot p that shows exactly x0..x1 across and y0..y1 up; curves leaving the window are cut off.

Example: `window(plot(1/(1 - x), x, 0, 2), 0, 2, -10, 10)`

### show

`show(p1, p2, ...)`

Overlays plots: all 2D, or all 3D. Plots are values, so p = plot(...) stores one.

Example: `show(plot(sin(x), x, 0, pi), plot(cos(x), x, 0, pi))`

### title

`title(p, "text"), xlabel(p, "text"), ylabel(p, "text"), zlabel(p, "text"), labels(p, "x", "y" (, "z"))`

A copy of plot p with a title or axis labels.

Example: `labels(plot(sin(t), t, 0, 10), "time t", "position")`

Also found as `xlabel`, `ylabel`, `zlabel`, `labels`.

### legend

`legend(p, "first", "second", ...)`

A copy of plot p with new names for its curves in the legend.

Example: `legend(show(plot(sin(x), x, 0, pi), plot(cos(x), x, 0, pi)), "sine", "cosine")`

### implicitplot

`implicitplot(F, x, x0, x1, y, y0, y1)`

Draws the curve F(x, y) = 0, or an equation such as x^2 + y^2 == 1, in the window x0..x1, y0..y1 (equal scales). A list [F1, F2, ...] draws several curves.

Example: `implicitplot(x^2/4 + y^2 == 1, x, -3, 3, y, -2, 2)`

### scatter

`scatter(data, [x y]) or scatter(data, [names...], [X Y]) (, sigma)`

Plots observations as points. data has one row per observation; the names label its columns. With two columns the first goes across and the second up; otherwise give [X Y], two expressions in the column names, such as [log(a) log(T)] for a log-log plot. A last argument with one uncertainty per row (for the last column) draws error bars. Combine with show(scatter(...), plot(...)) to draw a fitted curve through the data.

Example: `scatter([1 1; 2 4; 3 9], [x y])`

### mandelbrot

`mandelbrot(re0, re1, im0, im1, n), julia(c, re0, re1, im0, im1, n)`

The points of a grid n points wide whose orbit under z -> z^2 + c stays within |z| <= 2 for 200 steps, as rows [re im] for scatter. mandelbrot varies c (starting from z = 0); julia fixes c and varies the start z.

Example: `scatter(mandelbrot(-2.2, 0.8, -1.2, 1.2, 180), [re im])`

Also found as `julia`.

### contour

`contour(f, x, from, to, y, from, to) or contour(f, x, a, b, y, c, d, levels)`

Level curves of f(x, y), colored from blue (low) to red (high). levels is a count (12) or a list of values ([0 1 4 9]). Combine with other plots using show.

Example: `contour(x^2 - y^2, x, -2, 2, y, -2, 2)`

### fieldplot

`fieldplot([P Q], x, from, to, y, from, to) or with a last argument n (arrows per side, default 15)`

Arrows of a vector field in the plane; lengths relative to the longest arrow. vectorfield is the same.

Example: `fieldplot([-y, x], x, -2, 2, y, -2, 2)`

Also found as `vectorfield`.

## Language

### for

`for k = values ... end, for x in values ... end`

Runs the statements up to end once for each value: each number of a range (for k = 1:10, for t = 0:0.1:1; walked one at a time, so for k = 1:10^7 builds no list), each entry of a row or column vector, each row of a matrix, each item of a list, each character of a string, or each field name of a record. Statements inside show their results unless they end in ;. break leaves the loop, continue goes to the next value. On one line, separate the parts with commas: for k = 1:3, print(k), end.

Example: `s = 0; for k = 1:100, s = s + k^2; end; s`

Also found as `break`, `continue`, `end`, `in`.

### while

`while condition ... end`

Runs the statements up to end as long as the condition is true; break leaves the loop, continue goes back to the test. Stops with an error after 100 million rounds.

Example: `n = 27; steps = 0; while n ~= 1, n = if(mod(n, 2) == 0, n/2, 3*n + 1); steps = steps + 1; end; steps`

### function

`function name(a, b) ... end, function out = name(a, b) ... end`

Defines a function of several statements. It has its own variables: only the arguments come in, and nothing it sets is seen outside. return value ends it with that value; with function out = name(...), the value of out at the end is the result. A function that gives no value can still be called as a statement (to print, for example). For a one-line formula, f(x) := ... is shorter.

Example: `function r = hyp(a, b), r = sqrt(a^2 + b^2); end; hyp(3, 4)`

Also found as `return`.

### print

`print(a, b, ...), disp(a)`

Shows its arguments on one line, separated by spaces; strings without quotes. Useful inside loops and functions. disp is the same.

Example: `for k = 1:3, print("k =", k, "k^2 =", k^2), end`

Also found as `disp`.

### strings

`"text", s + t, str(x), num(s), length(s), s(k), upper, lower, trim, split, join, replace, contains, format`

Text in double quotes. + joins text with anything ("x = " + 3 is "x = 3"). str(x) is x as text, num(s) reads a number from text. s(k) is the k-th character (from 1), s[k] counts from 0. upper(s), lower(s), trim(s) (spaces at the ends removed); split(s) at spaces or split(s, sep), giving a list; join(list, sep); replace(s, old, new); contains(s, part); format("{} + {} = {}", 1, 2, 3) puts values in place of each {}.

Example: `split("a,b,c", ",")`

Also found as `str`, `num`, `upper`, `lower`, `trim`, `split`, `join`, `replace`, `contains`, `format`.

### list

`{a, b, c}, list(v), L(k), L[k], append(L, x), concat(L1, L2), map(f, L), filter(f, L), reverse(L), length(L)`

A list holds values of any kind, also other lists and matrices. L(k) is item k counting from 1, L[k] counts from 0, and L(end) is the last item. L(k) = x changes an item (and extends the list when k is just past the end). list(v) turns a vector or a string into a list. append adds items at the end, concat joins lists (or strings), map(f, L) applies a function by name to each item, filter(f, L) keeps the items where f is true, contains(L, x) tests membership. for x in L ... end walks it.

Example: `sq(x) := x^2; map(sq, {1, 2, 3})`

Also found as `append`, `concat`, `map`, `filter`, `reverse`.

### record

`{name: value, ...}, r.name, r.name = value, fields(r), has(r, "name")`

A record holds named values. Make one with {mass: 3, name: "ball"} or field by field: p.mass = 3 creates p when it does not exist yet. Records can hold records: p.pos.x = 1. fields(r) lists the names, has(r, "name") tests for one.

Example: `p = {mass: 2, v: 3}; p.mass*p.v^2/2`

Also found as `fields`, `has`.

### assignment

`x(k) = value, A(i, j) = value, A[i, j] = value, x(end + 1) = value`

Changes one entry of a vector, matrix or list: x(k) counts from 1, x[k] from 0. A vector grows when k is past its end (missing entries become 0), so x = []; for k = 1:5, x(k) = k^2; end builds a vector. In positions, end is the last one: x(end), x[end], x(end + 1) = v appends. A(k) = row replaces row k of a matrix.

Example: `x = []; for k = 1:5, x(k) = k^2; end; x`

## Complex numbers

### real

`real(z), imag(z), conj(z), abs(z), arg(z)`

Real part, imaginary part, complex conjugate, modulus |z| and argument (angle) of z. Variables count as real.

Example: `arg(1 + i)`

Also found as `imag`, `conj`, `arg`.

### rect

`rect(z), polar(z)`

rect writes z as a + b*i; polar gives [r, phi] with z = r*exp(i*phi).

Example: `polar(1 + i)`

Also found as `polar`.

## Number theory

### isprime

`isprime(n), nextprime(n), prevprime(n)`

Whether n is prime (Miller-Rabin: certain below 3.3e24, a 20-base probable-prime test above), the next prime after n and the last one before it. Any size.

Example: `isprime(2^521 - 1)`

Also found as `nextprime`, `prevprime`.

### ifactor

`ifactor(n), factor(n), factors(n)`

Prime factorization of a whole number, as rows [prime, exponent]: 360 gives [2, 3; 3, 2; 5, 1], and the explanation shows 2^3 * 3^2 * 5. Uses trial division, then Pollard's p - 1 and rho. factor(n) also splits numbers a^n +- b^n algebraically first (see help(factor) and algfactor).

Example: `ifactor(2^64 + 1)`

Also found as `factors`.

### cyclotomic

`cyclotomic(n), cyclotomic(n, v)`

The cyclotomic polynomial Phi_n(x): the product of x - z over the primitive n-th roots of unity z. It has integer coefficients, is irreducible, and x^n - 1 is the product of Phi_d(x) over the divisors d of n. cyclotomic(n, v) puts v in place of x: a number (cyclotomic(8, 10) = 10001) or an expression (cyclotomic(4, 2*x^2)).

Example: `cyclotomic(12)`

### algfactor

`algfactor(n), algfactor(n, effort), algfactor(n, f), algfactor(n, f, effort)`

Searches for an algebraic form of the whole number n and factors along it, one row per piece with its size, its prime factors and the composite parts that did not split. It tests, in order: a form typed as a^k +- b^k; n = a^k + 1 or a^k - 1 for every base a (by k-th roots); n = a^k +- b^k for b up to 12*effort; Sophie Germain's a^4 + 4b^4; n dividing b^k +- 1 (a cofactor from a factor table) for b up to 12*effort and k up to 3000*effort; and finally n = f(x) for every polynomial family in families() (built in, plus any added with addfamily); then the same families with a second variable, n = y^d f(x/y) for y up to 50*effort (so x^6 + 27 also finds x^6 + 27y^6); the list includes 81 Aurifeuillean-type families g(c x^2) and g(c x^3) found by search (like x^6 + 8x^2 - 1 = (x^3 + 2x^2 + 2x - 1)(x^3 - 2x^2 + 2x + 1)), and every family is first checked modulo small numbers so most are ruled out without big arithmetic; then Fermat's method (n = a^2 - b^2 with a near sqrt(n), 20000*effort steps), which splits n at once when two factors are close; and last, for every degree d and every base x near the d-th root of n (and of n/c for small leading digits c), it writes n in base x with balanced digits, and when only a few digits are nonzero and all are at most 8*effort, it factors that polynomial. With a polynomial f in one variable, only n = f(x) is tested. Pieces are values of cyclotomic polynomials (split again by Aurifeuillean factors) or of the factors of f. effort (default 1, about a second; up to 10000) also gives Pollard rho and p - 1 that many times more work on each piece. The result has one row per factor found, like factor: [p, e], with a composite part that did not split in a row of its own.

Example: `algfactor((2^1062 + 1)/5)`

### addfamily

`addfamily(f), families()`

addfamily adds a polynomial family f(x) with whole-number coefficients (for example one found with factorsearch) to the list that algfactor tries on every number, for the rest of the session. families() shows the list: the built-in families and the added ones.

Example: `addfamily(x^8 + x^4 + 1)`

### factorsearch

`factorsearch(expr, k, a, b) (, step)`

Hunts for factorizations in a family of polynomials: for k = a, a+1, ..., b, puts k into expr, factors the result over the integers and lists the k for which it splits, with the factors. expr must become a polynomial in one variable, or a homogeneous one in two. r[j] is the j-th factorization found.

Example: `factorsearch(x^4 + k, k, 1, 100)`

### pm1

`pm1(n), pm1(n, B)`

Pollard's p - 1 method with bound B (default 100000): computes 3^E mod n for E = lcm(1, ..., B). A prime p of n with every prime power of p - 1 at most B has 3^E = 1 mod p, because (Z/p)* has order p - 1, so gcd(3^E - 1, n) finds p. It says which case happened and shows p - 1. The result is a factorization like factor's: rows [p, e] with every prime of n. After the method splits n, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `pm1(2^67 - 1)`

### ecm

`ecm(n), ecm(n, curves), ecm(n, curves, B1)`

Lenstra's elliptic curve method: like p - 1, but in the group of points of a random elliptic curve mod p, whose order varies between p + 1 - 2 sqrt(p) and p + 1 + 2 sqrt(p) from curve to curve. Each curve is a new chance for a smooth group order. Defaults: 50 curves, B1 = 11000 (good for factors of about 20 digits; 50000 for 25, 250000 for 30). algfactor and factor also try a few curves after rho, more with a larger effort. The result is a factorization like factor's: rows [p, e] with every prime of n. After the method splits n, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `ecm(nextprime(7*10^15)*nextprime(10^45))`

### squfof

`squfof(n)`

Shanks' square forms factorization (SQUFOF): walks the continued fraction of sqrt(k n) until a square appears among the denominators, then walks back from the square form to a factor. It takes about n^(1/4) steps, so it is for n up to about 30 digits. The result is a factorization like factor's: rows [p, e] with every prime of n. After the method splits n, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `squfof(1000000000039 * 10000000000037)`

### classgroup

`classgroup(D)`

The class group of the negative discriminant D: one reduced form (a, b, c), meaning a x^2 + b x y + c y^2 with b^2 - 4ac = D and |b| <= a <= c, for each class, with its order under composition. Forms of order 2 are called ambiguous; for D = -4N they are exactly the factorizations of N.

Example: `classgroup(-4*391)`

### reduceform

`reduceform([a, b, c]), composeforms(f, g), formpow(f, k)`

Binary quadratic forms a x^2 + b x y + c y^2 written [a, b, c]: the reduced form in the same class, Gauss composition of two forms of the same discriminant (the group law of the class group), and the k-th power of a form.

Example: `composeforms([2, 1, 3], [2, 1, 3])`

Also found as `composeforms`, `formpow`.

### classfactor

`classfactor(n), classfactor(n, B)`

Factoring with the class group (after Schnorr and Lenstra): raises a form of discriminant -4kn to the product of the odd prime powers up to B (default 50000), then squares until the identity appears; the form just before has order 2, so it is ambiguous and splits n. Works when the odd part of the class number is B-smooth; for n up to about 25 digits. The result is a factorization like factor's: rows [p, e] with every prime of n. After the method splits n, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `classfactor(nextprime(10^8)*nextprime(3*10^8))`

### polyselect

`polyselect(n), polyselect(n, d), polyselect(n, d, generations)`

Explores polynomial selection for the number field sieve: pairs f(x) of degree d (default by size: 3 below 60 digits, 4 below 110, 5 below 220, else 6) and g(x) = x - m with f(m) = n. Starts from the base-m expansion of n, runs a root sieve over rotations f + (j x + k)(x - m) to lower Murphy's alpha (more roots mod small primes), then an evolutionary search over the leading coefficient, the shift of m and the rotation, scored by Murphy's E (Dickman's rho of both sides over the sieve region). Shows lognorm, alpha and E for each step and the best pair, with the check f(m) - n = 0. A teaching tool: real selectors (Kleinjung's algorithm in CADO-NFS and msieve) search far more leading coefficients and also translate f.

Example: `polyselect(nextprime(10^59)*nextprime(10^60))`

### digits

`digits(n)`

The number of decimal digits of the whole number n.

Example: `digits(2^521 - 1)`

### primes

`primes(n), primes(a, b), prime(k), primepi(n)`

The primes up to n (or from a to b) by the sieve of Eratosthenes, the k-th prime, and the number of primes up to n.

Example: `primes(30)`

Also found as `prime`, `primepi`.

### divisors

`divisors(n), sigma(n), totient(n)`

All divisors of n, their sum, and Euler's phi: how many of 1..n share no factor with n (eulerphi is the same).

Example: `divisors(28)`

Also found as `sigma`, `totient`, `eulerphi`.

### powmod

`powmod(a, b, n), modinv(a, n)`

a^b mod n computed by repeated squaring (fast for huge b), and the inverse of a mod n (the x with a*x = 1 mod n).

Example: `powmod(2, 1000, 1001)`

Also found as `modinv`.

### multorder

`multorder(a, n), primroot(n)`

The smallest k > 0 with a^k = 1 mod n, and the smallest primitive root mod n (a number whose powers give every unit).

Example: `multorder(2, 13)`

Also found as `primroot`.

### crt

`crt([r1, r2, ...], [m1, m2, ...])`

Chinese remainder theorem: the smallest x >= 0 with x = r_i mod m_i (moduli without common factors).

Example: `crt([2, 3, 2], [3, 5, 7])`

### pollard

`pollard(n), isqrt(n)`

One factor of n found by Pollard's rho method, and the integer square root (largest k with k^2 <= n).

Example: `pollard(8051)`

Also found as `isqrt`.

### letters

`letters(text), word(list)`

letters turns the letters of a text into numbers A = 0, B = 1, ..., Z = 25 (other characters are skipped); word turns numbers back into letters, taking them mod 26. For shift ciphers.

Example: `word(mod(letters("HELLO") + 3, 26))`

Also found as `word`.

### encode

`encode(text), decode(n)`

encode turns a text into one whole number (its bytes in base 256); decode turns the number back into the text. For RSA examples.

Example: `decode(encode("Hi Galileo"))`

Also found as `decode`.

## Factoring lab

### semiprime

`semiprime(digits), semiprime(digits, seed), semifactors(digits, seed)`

A balanced semiprime N = p q with exactly that many digits, p and q primes of the same size drawn at random from the seed (default 0); the same seed always gives the same number. semifactors gives the factorization like factor: [p, 1; q, 1] with p < q. These are the test numbers of the factoring lab.

Example: `semiprime(30, 7)`

Also found as `semifactors`.

### labrun

`labrun(method, N), labrun(method, N, seconds)`

Runs one factoring method on N with a time limit (default 5 s) and reports the factor and the work it took. Work is counted in multiplications modulo N, the same on every computer. A method is a built-in name in quotes: "trial", "fermat", "hart", "rho", "pm1", "squfof", "ecm", "qs"; a lab program like "iter(x^2 + 1, x - y, 2)" or "rel(rti, 64)" (see help(algosearch)); or the name of your own function f(n) := ... that returns a factor (its work is counted in evaluation steps, so compare user functions by time). The result is a factorization like factor's: rows [p, e] with every prime of N. After the method splits N, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `labrun("qs", semiprime(40, 1))`

### scoreboard

`scoreboard(methods, digits, trials, seconds)`

Runs methods (one, or a row like ["rho" "qs"]) on balanced semiprimes of the given sizes (a row like 12:4:40, the default), trials numbers per size (default 3), at most seconds per number (default 1). A method stops at the first size where it solves nothing. Shows a table of solved counts, median work and time, and fits how the cost grows: N^a (exponential in the digits), L(1/2) (quadratic sieve) or L(1/3) (number field sieve). Natively the fit uses time, in the browser work units. scoreplot(s) draws it.

Example: `scoreboard(["rho" "squfof" "qs"], 16:4:32)`

### scoreplot

`scoreplot(s), scoreplot(s1, s2, ...)`

Plots scoreboard results: log10 of the cost against the number of digits, one series of points per method, plus reference curves for N^(1/2), N^(1/4), L(1/2, 1) and L(1/3, 1.92) drawn through the first point (only where they stay near the data). The fitted growth laws are listed in the scoreboard table.

Example: `scoreplot(scoreboard("rho", 12:2:20))`

### algosearch

`algosearch(seconds), algosearch(seconds, mode), algosearch(seconds, mode, seed), algosearch(seconds, mode, seed, digits)`

Searches for new factoring procedures for about that many seconds (default 5). Programs are written in a small language: iter(f, h, x0) iterates x -> f(x, y, i) from x0, with y the value of x at the last power-of-two step, and checks gcd(h, N) (Pollard rho is iter(x^2 + 1, x - y, 2), p - 1 is iter(pow(x, i), x - 1, 2)); rel(s, fb) makes s at step i, checks whether s^2 mod N is a square, and with fb factor-base primes combines smooth values by linear algebra mod 2 (Fermat is rel(rt + i, 0), Hart rel(rti, 0), Dixon rel(rt + i, 64)). Names: x, y, i, rt = ceil(sqrt N), rti = ceil(sqrt(i N)), B = floor(N^(1/3)), d0, d1 (digits of N in base B), small integers; operations + - * ^k, pow(a, i), luc(a) (Lucas V_i, Williams p + 1), inv(a) (a failed inverse mod N reveals a factor). A third kind, grp(world, e, r), raises an element of a group mod N to the power e(i) at step i and checks gcd with N: worlds mul (order p - 1, so grp(mul, i, 0) is Pollard p - 1), quad(d) (a + b sqrt(d), p + 1), cubic(c) (polynomials mod t^3 - c, Bach-Shallit), mat (2 x 2 matrices) and ec(a) (elliptic curves); with r > 0 it starts over with a new element and parameter every r steps, so grp(ec(3), i, 100) is ECM. mode "evolve" (default) mutates and recombines the best programs, "random" samples at random, "exhaustive" tries every small program in order of size. Programs are scored by total work on 12 semiprimes of 12 to 18 digits (or of the sizes in digits, e.g. [20 24 28], after a 12-digit screening; slow), then the best are re-run on other numbers two digits larger next to Pollard rho, with a paired p-value: gains that only show on the training numbers are luck. r[k] is program k as text.

Example: `algosearch(10)`

### leaderboard

`leaderboard()`

The best programs found by every algosearch in this session, with how they compare to Pollard rho on numbers no search trained on.

Example: `algosearch(1); leaderboard()`

### gnfs

`gnfs(N), gnfs(N, seconds), gnfs(N, f, m), gnfs(N, f, m, seconds)`

Factors N with the number field sieve and prints every step: the polynomial f with f(m) divisible by N (base m, or a special form when N divides b^k +- 1: that is the special number field sieve), the factor bases, the line sieve over pairs (a, b) where both a - b m and the norm b^d f(a/b) are smooth, the linear algebra mod 2 (with quadratic characters), the square root in the number field (Hensel lifting at a prime where f splits) and gcd(X - Y, N). Give your own f (a polynomial in x, made monic if needed) and m to try a polynomial. Default limit 60 s. This is a teaching version: below about 60 digits the quadratic sieve is faster. The result is a factorization like factor's: rows [p, e] with every prime of N. After the method splits N, a composite part is factored on (as factor does, then by this method again) until all parts are prime; a part that does not split keeps a row of its own. [] when no factor is found.

Example: `gnfs(2^128 + 1)`

Also found as `nfs`.

### nfspoly

`nfspoly(N), nfspoly(N, degree)`

Polynomials for the number field sieve on N with their scores: the estimated size in bits of the two norms over a typical sieve region plus Murphy's alpha (how often values of f are divisible by small primes). Lower scores sieve faster. Lists the special forms found for N and the best base-m polynomials of degree 3 to 5 (or of the given degree). r[j] is the coefficient vector of the j-th polynomial, constant term first.

Example: `nfspoly(2^128 + 1)`

### leakhunt

`leakhunt(digits, samples, seed)`

Looks for information about p hidden in cheap features of N = p q: residues of N, Jacobi symbols, leading digits, digits in other bases, the continued fraction of sqrt(N) and, up to 6 digits, class numbers h(-4N). Each feature is tested against properties of the smaller prime p (p mod 4, p mod 3, a middle bit, ...) with a chi-squared test on half of the samples (default 16 digits, 4000 samples), corrected for the number of tests (Benjamini-Hochberg) and re-tested on the other half; a naive Bayes predictor tries all features together. Control properties (p = q mod 3, the second bit of p) are known to leak and show that the test works.

Example: `leakhunt(12, 2000)`

## Groups

### perm

`perm("(1 2 3)(4 5)", n), cycles(p)`

A permutation of 1..n written in one-line notation, p = [p(1), p(2), ...], from cycle notation; cycles(p) goes back.

Example: `cycles([2, 3, 1, 5, 4])`

Also found as `cycles`.

### compose

`compose(p, q), pinverse(p)`

p after q: first q, then p, so compose(p, q)[i] = p[q[i]]. pinverse undoes p.

Example: `compose([2, 3, 1], [2, 1, 3])`

Also found as `pinverse`.

### porder

`porder(p), psign(p)`

Order of a permutation (smallest k with p^k = identity: the lcm of its cycle lengths) and its sign (+1 even, -1 odd).

Example: `porder([2, 3, 1, 5, 4])`

Also found as `psign`.

### generate

`generate(g1, g2, ...), cayley(G)`

All permutations made by composing the generators, one per row (the identity first); cayley(G) is the group table: entry (i, j) is the row number of G_i after G_j.

Example: `cayley(generate([2, 3, 1]))`

Also found as `cayley`.

### sym

`sym(n), alt(n), cyclic(n), dihedral(n), units(n), group("Q8"), group(p1, p2, ...)`

Groups as values: the symmetric and alternating groups on n points, the cyclic group of order n, the symmetries of a regular n-gon (order 2n), the units of Z/n, the quaternion group Q8 and Klein four-group V4 by name, or the group generated by permutations. A group prints as the call that rebuilds it.

Example: `G = sym(4); order(G)`

### order

`order(G), elements(G), gens(G), orders(G), cayley(G)`

order(G) is the number of elements of G. elements(G) lists them (one permutation per row), gens(G) its generators, orders(G) gives [order, count] for the orders of its elements, and cayley(G) its group table.

Example: `orders(group("Q8"))`

### subgroup

`subgroup(G, p1, ...), center(G), derived(G), sylow(G, p)`

Subgroups: generated by elements of G, the center (what commutes with everything), the commutator subgroup [G, G], and a Sylow p-subgroup (of the largest p-power order dividing |G|).

Example: `order(sylow(alt(5), 5))`

### cosets

`cosets(G, H), isnormal(G, H), quotient(G, N), classes(G)`

Left cosets gH (one representative per row), whether H is normal in G, the quotient group G/N, and the conjugacy classes (one representative per row; the note gives the class equation).

Example: `quotient(sym(4), subgroup(sym(4), [2, 1, 4, 3], [3, 4, 1, 2]))`

### kernel

`kernel(G, H, f1, ...), image(G, H, f1, ...), ishom(G, H, f1, ...)`

A homomorphism G -> H given by the images f1, f2, ... of the generators of G (in the order of gens(G)): its kernel, its image, or whether such a homomorphism exists.

Example: `order(kernel(sym(3), cyclic(2), [2, 1], [1, 2]))`

### isomorphic

`isomorphic(G, H), abelianinv(G), cayleygraph(G)`

Whether two groups (up to order 2000) are isomorphic (the note gives a map); the invariant factors [d1, d2, ...] of G/[G, G], so G/[G, G] is Z/d1 x Z/d2 x ...; and a picture of G, one color of arrows per generator. Groups can also be given by a presentation: group("<a, b | a^4, b^2, (a*b)^2>") (coset enumeration).

Example: `isomorphic(group("<a, b | a^4, b^2, (a*b)^2>"), dihedral(4))`

### isabelian

`isabelian(G), issolvable(G), issimple(G), dprod(G, H)`

Whether G is abelian, solvable (its derived series reaches the trivial group) or simple (no normal subgroups but 1 and G); dprod is the direct product.

Example: `issolvable(sym(5))`

## Rings and fields

### gf

`gf(q), zmod(n), gf(q, x), zmod(n, x), F = gf(9); F(a + 1)`

The finite field with q = p^k elements and the ring of integers mod n, as values. An element is made by gf(q, x), zmod(n, x) or F(x); in gf(p^k) with k > 1 elements are polynomials in a, where a is a root of modulus(F). Elements add, subtract, multiply, divide and take powers (negative powers are inverses); numbers mix in. characteristic(F), modulus(F), order(F), elements(F), isfield(F), lift(x).

Example: `F = gf(9); F(a + 1)^2`

Also found as `characteristic`, `modulus`, `lift`, `isfield`.

### generator

`generator(F), order(x), dlog(h, g)`

A generator of the units of F (every nonzero element is a power of it), the multiplicative order of an element, and the discrete logarithm: the x with g^x = h (Pohlig–Hellman with baby-step giant-step).

Example: `F = gf(101); dlog(F(3), generator(F))`

### groebner

`groebner([f1, f2, ...], [x, y, ...]), polyrem(f, [f1, ...], [x, ...]), inideal(f, [f1, ...], [x, ...])`

The reduced Gröbner basis of the ideal generated by f1, f2, ... (lex order with x > y > ...; add "grlex" or "grevlex" for a degree order, and a prime p to work mod p), the remainder of f modulo the ideal, and whether f is in it. In lex order the basis is triangular, which is how systems of polynomial equations are solved by elimination; a basis [1] means no common solution.

Example: `groebner([x^2 + y^2 - 1, x - y], [x, y])`

### numfield

`numfield(f), quotient(f, x), quotient(f, x, p), K = numfield(x^2 - 2); K(x + 1)`

The number field Q[x]/(f) for f irreducible over Q, and the quotient ring K[x]/(f) with K = Q, or Z/p when a prime p is given. Elements are polynomials in x reduced mod f and support + - * / ^ (division fails on a zero divisor, naming the common factor). norm(a), trace(a) and minpoly(a) give the field norm, the trace and the minimal polynomial; modulus, isfield, characteristic, order and lift work as for gf.

Example: `K = numfield(x^3 - 2); minpoly(K(x^2 + x))`

Also found as `minpoly`.

### galois

`galois(f) or galois(f, x)`

The Galois group over Q of an irreducible polynomial of degree up to 5, as a group of permutations of its roots (order, issolvable and the other group commands apply). Degree 3 and 4 use the discriminant and the resolvent cubic; degree 5 adds how f factors mod many primes. The note says whether the roots can be written with radicals.

Example: `issolvable(galois(x^5 - x - 1))`

### quat

`quat(a, b, c, d), quat(angle, [x, y, z]), conj(q), norm(q), parts(q), rotate(q, v), rotmatrix(q), su2(q)`

Quaternions a + b i + c j + d k (components may be symbolic), with + - * / and whole powers; i j = k but j i = -k. quat(angle, axis) is the unit quaternion of a rotation; rotate(q, v) computes q v q^-1, rotmatrix(q) its 3x3 rotation matrix and su2(q) the matching 2x2 complex matrix: unit quaternions are SU(2), and q and -q give the same rotation (the double cover SU(2) -> SO(3)).

Example: `rotate(quat(pi/2, [0, 0, 1]), [1, 0, 0])`

### su

`su(n), so(n), sl(n), bracket(A, B), coords(X, basis), adjoint(X, basis), killing(basis)`

Bases of the Lie algebras su(n), so(n) and sl(n) (n up to 6) as lists of matrices; the bracket A B - B A (also for quaternions); the coordinates of a matrix in a basis; the matrix of ad(X) = [X, .]; and the Killing form trace(ad x ad y), nondegenerate exactly for semisimple algebras.

Example: `B = su(2); bracket(B(1), B(2)) == B(3)`

## Statistics

### mean

`mean(x), median(x), mode(x), var(x), std(x), summary(x), quantile(x, p)`

Descriptive statistics of a list of numbers (a row or column). var and std divide by n - 1 (sample variance). summary gives [min, Q1, median, Q3, max]; quantile interpolates linearly between sorted values (p may be a list). Whole numbers and fractions give exact answers.

Example: `summary([2 4 4 4 5 5 7 9])`

Also found as `median`, `mode`, `var`, `std`, `summary`, `quantile`.

### entropy

`entropy(p), entropy(p, 2), entropy(rho)`

The Shannon entropy -sum p*log(p) of probabilities p = [p1 p2 ...], in nats (natural log), or in bits with entropy(p, 2). It is 0 when one outcome is certain and log(n) for n equally likely outcomes. For a square density matrix rho it is the von Neumann entropy -tr(rho*log(rho)), from the eigenvalues of rho.

Example: `entropy([1/2 1/4 1/4], 2)`

### tally

`tally(x), sort(x), numel(x), cumsum(x)`

tally lists the distinct values (first row) and how often each occurs (second row); sort sorts; numel counts; cumsum gives running totals. Comparisons such as x < 2 work entry by entry, giving 1 or 0, so sum(x < 2) counts.

Example: `tally([1 3 3 2 3 1])`

Also found as `sort`, `numel`, `cumsum`.

### cov

`cov(x, y), corr(x, y), linfit(xs, ys) or linfit(xs, ys, x)`

Sample covariance, the correlation coefficient r, and the least-squares line a + b x through the points (xs, ys), with r squared in the note.

Example: `linfit([1 2 3 4], [2 4 5 8])`

Also found as `corr`, `linfit`.

### seed

`seed(n); randn(n) or randn(n, m); randint(a, b, n); randbin(n, p, count); randpois(lambda, count)`

Random samples: seed(n) makes them repeatable. randn gives standard normal numbers, randint whole numbers from a to b, randbin binomial counts (successes in n trials), randpois Poisson counts. A single count gives a row; two give a matrix.

Example: `seed(1); randint(1, 6, 10)`

Also found as `randn`, `randint`, `randbin`, `randpois`.

### normpdf

`normpdf(x, mu, sigma), normcdf(x, mu, sigma), norminv(p, mu, sigma); binopdf(k, n, p), binocdf; poisspdf(k, lambda), poisscdf; tpdf(x, df), tcdf, tinv(p, df); chi2pdf(x, df), chi2cdf, chi2inv`

Probability distributions: pdf is the density or probability P(X = k), cdf is P(X <= x), inv is the inverse of the cdf. mu = 0 and sigma = 1 when left out. Densities with a symbol for x are formulas you can plot and integrate; binomial and Poisson probabilities are exact for exact inputs.

Example: `binopdf(3, 10, 1/2)`

Also found as `normcdf`, `norminv`, `binopdf`, `binocdf`, `poisspdf`, `poisscdf`, `tpdf`, `tcdf`, `tinv`, `chi2pdf`, `chi2cdf`, `chi2inv`.

### ci

`ci(data) or ci(data, level); ttest(data, mu0) or ttest(x, y); chi2test(observed, expected) or chi2test(table)`

Inference: the t confidence interval for a mean (level 0.95 by default); the two-sided p-value of a one-sample or two-sample (Welch) t-test; the p-value of a chi-square test of fit (expected counts or probabilities) or of independence in a table of counts. The note gives the test statistic and degrees of freedom.

Example: `chi2test([18 22 20 25 15], [20 20 20 20 20])`

Also found as `ttest`, `chi2test`.

### hist

`hist(data), hist(data, bins), hist(data, [edges]), with a last "density"; bar(heights) or bar(xs, heights)`

Histogram of a list of numbers (bins equal-width bins, or the given edges); "density" scales the area to 1 to compare with a density curve using show. bar(xs, heights) draws bars of the given heights, for example the probabilities of a discrete distribution; bar(xs, heights, width) sets the bar width, to put two sets of bars side by side.

Example: `show(hist(randn(1, 1000), 20, "density"), plot(normpdf(x), x, -4, 4))`

Also found as `histogram`, `bar`.

## Complex analysis

### laurent

`laurent(f, z, a, n)`

The Laurent series of f around z = a up to (z − a)^n, with all negative powers. The note says whether a is a regular point, a removable singularity, a pole (and its order) or an essential singularity, and gives the residue. Works for rational functions, exp, log, powers and the trigonometric and hyperbolic functions.

Example: `laurent(exp(z)/z^3, z, 0, 2)`

### residue

`residue(f, z, a); poles(f, z)`

residue: the coefficient of 1/(z − a) in the Laurent series of f around a, exact. poles: one row [pole, order, residue] for each pole of a rational function. By the residue theorem, the integral of f around a closed curve is 2πi times the sum of the residues inside.

Example: `poles(1/(z^4 + 1), z)`

Also found as `poles`.

### cint

`cint(f, z, path, t, t0, t1)`

The contour integral of f(z) dz along the path z = path(t), t from t0 to t1, computed numerically: the integral of f(z(t)) z'(t) dt.

Example: `cint(1/z, z, exp(i*t), t, 0, 2*pi)`

### cplot

`cplot(f, z, x0, x1, y0, y1); cmap(f, z, x0, x1, y0, y1)`

cplot: domain coloring of f over the rectangle x0 ≤ Re z ≤ x1, y0 ≤ Im z ≤ y1; the hue shows arg f(z), the brightness |f(z)|, so zeros are dark and poles light. cmap: the images under f of the horizontal and vertical grid lines of the rectangle.

Example: `cplot((z^2 - 1)/(z^2 + 1), z, -2, 2, -2, 2)`

Also found as `cmap`.

## Differential equations

### seriessolve

`seriessolve(ode, y, x, n) or seriessolve(ode, y, x, n, [y(0), d(y, 0), ...])`

Power series solution y = a0 + a1 x + ... + an x^n about the ordinary point x = 0 of a linear equation (polynomial coefficients, or any with a Taylor series). Without start values a0 = y(0) and a1 = y'(0) stay free.

Example: `seriessolve(d(y, x, 2) == x*y, y, x, 8)`

### laplace

`laplace(f, t, s) or laplace(f)`

Laplace transform F(s) = ∫₀^∞ f(t) e^(-s t) dt from the table and its rules: powers, exponentials, sines and cosines, sinh, cosh, shifts by e^(a t), products with t^n, heaviside(t - a) and dirac(t - a), and derivatives y(t), d(y, t), d(y, t, 2) with y(0) and d(y, 0). With 7 or 8 arguments, laplace(g, x, a, b, y, c, d) solves Laplace's equation instead (see poisson).

Example: `laplace(exp(-t)*cos(2*t), t, s)`

### ilaplace

`ilaplace(F, s, t) or ilaplace(F)`

Inverse Laplace transform of a rational function of s, by partial fractions, with delays e^(-a s) turned into heaviside(t - a).

Example: `ilaplace(1/(s*(s + 1)), s, t)`

### partfrac

`partfrac(f, x)`

Partial fraction decomposition of a rational function over the rationals.

Example: `partfrac((x + 3)/(x^2 - 1), x)`

Also found as `apart`.

### fourier

`fourier(f, x, L, N) or fourier(f, x, L, N, "sine") or fourier(f, x, L, N, "cosine")`

Fourier series of f on [-L, L] up to the N-th harmonic: a0/2 + Σ a_n cos(nπx/L) + b_n sin(nπx/L). With "sine" or "cosine", the half-range series on [0, L]. f may be piecewise with heaviside, sign and abs; the note gives the general a_n and b_n.

Example: `fourier(sign(x), x, pi, 5)`

### heaviside

`heaviside(x), dirac(x), sign(x)`

The unit step (0 for x < 0, 1 for x > 0, 1/2 at 0), the Dirac delta (for integrals and Laplace transforms: ∫ h(x) dirac(x - a) dx = h(a)), and the sign function. Definite integrals split at their jumps.

Example: `int(heaviside(x - 1)*x, x, 0, 3)`

Also found as `dirac`, `sign`.

### assume

`assume(a > 0), assume(a < 0), assume(n, integer), assume(n, integer, positive); forget(a); assumptions`

Facts about symbols that limits, integrals and simplification use: e^(-a t) -> 0 as t -> inf when a > 0, sqrt(a^2) = a, sin(n pi) = 0 and cos(n pi) = (-1)^n for an integer n. forget(a) removes them, forget() removes all, assumptions lists them.

Example: `assume(a > 0); int(exp(-a*x), x, 0, inf)`

Also found as `forget`, `unassume`, `assumptions`.

## Differential geometry

### metric

`metric([x1 x2 ...], [g11 g12 ...; ...]) or metric.generate(name)`

A metric with the given coordinates and components g_ab. Coordinates stay symbolic even if a variable of that name has a value. metric.generate gives a ready-made one: minkowski, euclidean, polar, spherical, two-sphere, three-sphere, torus, hyperbolic, schwarzschild.

Example: `g = metric([r phi], [1 0; 0 r^2])`

Also found as `metric.generate`.

### inducedmetric

`inducedmetric([x y z], [u v])`

The metric induced by flat space on the surface (or curve, or hypersurface) x(u, v), y(u, v), z(u, v): the first fundamental form.

Example: `inducedmetric([sin(u)*cos(v) sin(u)*sin(v) cos(u)], [u v])`

### inverse

`inverse(g)`

Inverse metric g^ab.

Example: `inverse(metric.generate(polar))`

### christoffel

`christoffel(g), christoffelfirst(g)`

Christoffel symbols of the second kind Gamma^a_bc (christoffelsecond is the same), or of the first kind Gamma_abc. Pick one with christoffel(g)[a, b, c].

Example: `christoffel(metric.generate(two-sphere))`

Also found as `christoffelfirst`, `christoffelsecond`.

### riemannsecond

`riemannsecond(g), riemannfirst(g)`

Riemann curvature tensor R^a_bcd (second) or R_abcd (first).

Example: `riemannsecond(metric.generate(two-sphere))`

Also found as `riemannfirst`, `riemann`.

### riccitensor

`riccitensor(g), ricciscalar(g)`

Ricci tensor R_ab and Ricci scalar R.

Example: `ricciscalar(metric.generate(two-sphere))`

Also found as `ricciscalar`.

### einsteintensor

`einsteintensor(g), einsteinscalar(g)`

Einstein tensor G_ab = R_ab - R g_ab / 2, and its trace.

Example: `einsteintensor(metric.generate(schwarzschild))`

Also found as `einsteinscalar`.

### weyltensor

`weyltensor(g), schoutentensor(g)`

Weyl conformal tensor C_abcd and Schouten tensor P_ab (3 or more dimensions).

Example: `weyltensor(metric.generate(three-sphere))`

Also found as `schoutentensor`.

### gausscurvature

`gausscurvature(g)`

Gaussian curvature of a 2D metric.

Example: `gausscurvature(metric.generate(two-sphere))`

### kretschmann

`kretschmann(g)`

Kretschmann scalar R_abcd R^abcd.

Example: `kretschmann(metric.generate(schwarzschild))`

### geodesics

`geodesics(g)`

The geodesic equations of a metric, with ' for d/ds.

Example: `geodesics(metric.generate(two-sphere))`

### geodesic

`geodesic(g, x0, v0, s1 (, [X Y (Z)] (, steps)))`

Integrates geodesics from position x0 with velocity v0 (one row per geodesic) up to parameter s1 and plots them, mapped through X, Y (, Z), expressions in the coordinates. Other variables in the metric need values first.

Example: `geodesic(metric.generate(two-sphere), [pi/2 0], [1 1], 6, [sin(theta)*cos(phi) sin(theta)*sin(phi) cos(theta)])`

## Discovery

### discover

`discover(data, [x y]) or discover(data, [x y], [term1 term2 ...]) (, sigma)`

Searches for the simplest formula that explains the last variable from the others (symbolic regression). (1) data: a matrix with one row per observation and one column per variable; (2) the names of the columns, in order; the quantity to explain comes last, or is the one written as a derivative: [t d(x, t, 2)] explains the second derivative of x, estimated from neighbouring rows (the first column is then the independent variable). (3) optionally a row of candidate terms; discover then looks for the fewest terms whose weighted sum fits (the SINDy idea). Without terms it tries power laws with simple exponents (y = C*x^(3/2)) and short sums of powers of the inputs. (4) optionally one uncertainty per row: the fit then weighs each row by 1/sigma^2 and reports chi2/dof. The result lists candidates from simple to complex with their misfit (relative error, or chi2/dof with uncertainties) and marks the best one: the simplest formula that predicts each row from the others (leave-one-out) at most twice as badly as any, with at least two rows per fitted constant (with uncertainties: the simplest whose chi2/dof is close to the lowest). Constants close to simple numbers such as 1, 3/2 or 4*pi^2 are recognised. d[k] picks candidate k, best(d) the best one.

Example: `discover([1 1; 2 2.83; 3 5.2; 4 8], [a T])`

### check

`check(law, data, [x y]) or check(law, data, [x y], sigma)`

Tests a formula for the last variable against observations. Names in the law that are not columns are constants to fit (k in k/p^2): they are fitted by least squares and reported with their uncertainty. The result shows each row's observed and predicted value and the difference; with uncertainties (one per row) the difference is in standard deviations and the law is called consistent when chi2/dof <= 3 and every point is within 3 sigma. Give a row of laws, [k/p, k/p^2], to compare several, or the result of discover to test its best formula on new data: check(d, newdata).

Example: `check(a^(3/2), [1 1; 4 8; 9 27], [a T])`

### best

`best(d)`

The formula discover marked as best, as an expression you can use further.

Example: `best(discover([1 1; 2 4; 3 9], [x y]))`

## Multivariable calculus

### grad

`grad(f, [x y z]) or grad(f, [r theta phi], "spherical")`

Gradient: the row of partial derivatives, pointing where f increases fastest. Coordinate systems: "cartesian" (default), "polar" [r theta], "cylindrical" [r theta z], "spherical" [r theta phi] with theta measured from the z axis; the components are then along the unit vectors.

Example: `grad(x^2*y, [x y])`

Also found as `gradient`.

### div

`div([P Q R], [x y z]) or div(F, [r theta phi], "spherical")`

Divergence of a vector field: the net outflow per unit volume, dP/dx + dQ/dy + dR/dz in Cartesian coordinates. Same coordinate systems as grad.

Example: `div([x*y, y*z, z*x], [x y z])`

Also found as `divergence`.

### curl

`curl([P Q R], [x y z]) or curl([P Q], [x y])`

Curl of a vector field: the row [dR/dy - dQ/dz, dP/dz - dR/dx, dQ/dx - dP/dy] in 3D, the number dQ/dx - dP/dy in the plane. Same coordinate systems as grad.

Example: `curl([-y, x, 0], [x y z])`

### laplacian

`laplacian(f, [x y z]) or laplacian(f, [r theta phi], "spherical")`

The Laplacian div(grad f): the sum of the second partial derivatives in Cartesian coordinates.

Example: `laplacian(1/r, [r theta phi], "spherical")`

### jacobian

`jacobian([f g], [x y])`

Jacobian matrix: row i holds the partial derivatives of the i-th function. Its determinant is the area scale factor of a change of variables.

Example: `jacobian([r*cos(t), r*sin(t)], [r t])`

### hessian

`hessian(f, [x y])`

Hessian matrix of second partial derivatives.

Example: `hessian(x^3 - 3*x*y + y^3, [x y])`

### potential

`potential([P Q R], [x y z])`

A function phi with grad(phi) = F, or [] when the field is not conservative (its curl is not zero).

Example: `potential([2*x*y, x^2 + 3*y^2], [x y])`

### lineint

`lineint(F, [x y], [x(t) y(t)], t, a, b)`

Line integral along the path r(t), a <= t <= b. For a vector field F: the work, the integral of F(r(t)) . r'(t) dt. For a scalar f: the integral of f ds with ds = |r'(t)| dt (f = 1 gives the length).

Example: `lineint([-y, x], [x y], [cos(t), sin(t)], t, 0, 2*pi)`

### flux

`flux(F, [x y z], [x(u,v) y(u,v) z(u,v)], u, a, b, v, c, d), surfint(f, ...)`

Flux of a vector field through a parametrized surface, the double integral of F . (r_u x r_v) du dv; surfint integrates a scalar f dS (f = 1 gives the area).

Example: `flux([x, y, z], [x y z], [sin(u)*cos(v), sin(u)*sin(v), cos(u)], u, 0, pi, v, 0, 2*pi)`

Also found as `surfint`.

### critical

`critical(f, [x y])`

Critical points: the real solutions of grad f = 0, one per row (exact for polynomial systems). The explanation classifies each with the second-derivative test.

Example: `critical(x^3 - 3*x*y + y^3, [x y])`

### classify

`classify(f, [x y], [x0 y0])`

Second-derivative test at a point: "min", "max", "saddle" or "inconclusive", from the signs of the Hessian's eigenvalues.

Example: `classify(x^2 - y^2, [x y], [0 0])`

### minimize

`minimize(f, x, a, b), minimize(f, [x y]), minimize(f, [x y], [x0 y0]); maximize the same`

Where f is smallest: on an interval by the closed interval method (exact); in several variables the best local minimum among the critical points; or numerically (BFGS) from a starting point. maximize works the same way.

Example: `minimize(x^3 - 3*x, x, -3, 2)`

Also found as `maximize`.

### lagrange

`lagrange(f, g, [x y]) or lagrange(f, [g1 g2], [x y z])`

Lagrange multipliers: the points where grad f = lambda grad g on the constraint g = 0 (an equation g1 == g2 works too), one per row. The explanation gives lambda and f at each point.

Example: `lagrange(x*y, x^2 + y^2 == 2, [x y])`

## Special functions

### gamma

`gamma(x), beta(a, b), digamma(x), polygamma(n, x)`

The gamma function, the integral of t^(x-1) e^(-t) from 0 to inf: gamma(n) = (n-1)!, gamma(1/2) = sqrt(pi), exact at whole and half-whole numbers. beta(a, b) = gamma(a) gamma(b) / gamma(a+b); digamma is the derivative of log(gamma(x)), polygamma(n, x) its n-th derivative.

Example: `gamma(7/2)`

Also found as `beta`, `digamma`, `polygamma`.

### erf

`erf(x), erfc(x), erfi(x)`

Error function, 2/sqrt(pi) times the integral of e^(-t^2) from 0 to x; erfc = 1 - erf; erfi(x) = -i erf(ix). Integrals of e^(polynomial of degree 2) come out with them.

Example: `integrate(exp(-x^2), x, 0, inf)`

Also found as `erfc`, `erfi`.

### zeta

`zeta(s)`

Riemann zeta function, the sum of 1/k^s for s > 1, continued to all s except 1. Exact at even numbers (zeta(2) = pi^2/6) and at 0 and negative whole numbers (zeta(-1) = -1/12).

Example: `zeta(4)`

### polylog

`polylog(s, z)`

Polylogarithm Li_s(z), the sum of z^k/k^s over k = 1, 2, 3, ... for |z| <= 1, continued to the rest of the plane. Li_1(z) = -log(1 - z); for whole s <= 0 a rational function of z; Li_s(1) = zeta(s). Numeric for complex z (whole s >= 1; s = 2 everywhere). sum turns series like x^k/k^2 and k^2 x^k into it; polylog(2, z) is the dilogarithm.

Example: `polylog(2, 1/2)`

### besselj

`besselj(n, x), bessely(n, x), besseli(n, x)`

Bessel functions of whole order n: J_n and Y_n solve x^2 y'' + x y' + (x^2 - n^2) y = 0 (J_n is the one finite at 0); I_n is the modified Bessel function.

Example: `plot(besselj(0, x), besselj(1, x), x, 0, 20)`

Also found as `bessely`, `besseli`.

### legendre

`legendre(n, x), hermite(n, x), laguerre(n, x), chebyshev(n, x), chebyshevu(n, x)`

Classical orthogonal polynomials of degree n: Legendre P_n, Hermite H_n (physicists'), Laguerre L_n, Chebyshev T_n and U_n.

Example: `legendre(3, x)`

Also found as `hermite`, `laguerre`, `chebyshev`, `chebyshevu`.

## Units

### convert

`convert(q, unit), convert(q)`

Converts a quantity to other units of the same dimension: convert(100 km/h, m/s), convert(2 kg*3 m/s^2, N). The target is written as units (km/h, J/(kg*K)) or as a string. Without a target, SI base units (m, kg, s, A, K, mol). An exact value that is not a whole number comes out as a decimal.

Example: `convert(60 mph, km/h)`

### value

`value(q), value(q, unit)`

The number of a quantity without its units: in SI base units (value(3 km) is 3000), or in the units given (value(3 km, mi)). Use it to plot or compute with plain numbers: value(v, m/s). Works entry by entry on a matrix.

Example: `value(100 km/h, m/s)`

### unit

`unit(x, "m/s")`

x with units: unit(3, "m/s") is 3 m/s. A number, a matrix or parentheses followed by a unit name (3 m/s, [1 2] m, (a + b) kg) means the same; quantities multiply, divide, add (in the units of the first term) and compare, and adding different dimensions is an error.

Example: `unit(5, "J/(kg*K)")`

### dimension

`dimension(q)`

The dimension of a quantity in words: dimension(3 m/s) is "length/time".

Example: `dimension(const.G)`

### const

`const.c, const.G, ...`

Physical constants with units (CODATA 2022): c (speed of light), G (gravitation), g (standard gravity), h, hbar (Planck), k or kB (Boltzmann), NA (Avogadro), R (gas constant), e (elementary charge), me, mp (electron and proton mass), eps0, mu0 (vacuum), sigma (Stefan-Boltzmann), Mearth, Rearth, Msun, Rsun, Mmoon.

Example: `const.hbar*const.c`

## Signals

### fft

`fft(x), ifft(X)`

The discrete Fourier transform of a row or column of numbers: X_k = sum of x_n e^(-2 pi i k n/N), for k = 0, ..., N - 1 (fast for N a power of 2). ifft inverts it. For samples dt apart, entry k is the frequency k/(N dt) (freqs gives them all); abs(fft(x)) is the spectrum.

Example: `fft([1 0 -1 0])`

### freqs

`freqs(n, dt)`

The frequencies of the entries of fft for n samples taken dt apart: 0, 1/(n dt), 2/(n dt), ..., then the negative frequencies, as numpy's fftfreq.

Example: `freqs(8, 0.5)`

## Graphs

### graph

`graph(edges), graph(edges, n), graph(edges, {names}), digraph(edges)`

A graph (network) as a value: one edge per row, [1, 2; 2, 3; 3, 1], with a third column for weights (distances, costs, capacities): ["A", "B", 4; "B", "C", 2]. Vertices are the numbers 1..n (graph(edges, n) adds isolated ones) or names in quotes. digraph makes the edges one-way arrows. A graph prints as the call that rebuilds it and shows as a drawing in notebooks. order(G) counts vertices, size(G) edges.

Example: `G = graph([1, 2; 2, 3; 3, 1; 3, 4])`

Also found as `digraph`.

### complete

`complete(n), complete(m, n), cycle(n), path(n), star(n), wheel(n), grid(m, n), hypercube(d), petersen(), randgraph(n, p)`

Ready-made graphs: every pair joined (K_n), the complete bipartite K_(m,n), a ring, a line, a hub with n spokes, a hub inside a ring, a lattice of m rows of n, the d-dimensional cube (2^d vertices), the Petersen graph, and a random graph (each pair joined with probability p, or randgraph(n, m) with m edges; seed(k) first to repeat it).

Example: `chromatic(petersen())`

Also found as `cycle`, `path`, `star`, `wheel`, `grid`, `hypercube`, `petersen`, `randgraph`.

### vertices

`vertices(G), edges(G), degree(G), degree(G, v), neighbors(G, v), adjacency(G), laplacian(G), subgraph(G, {vs}), complement(G)`

Looking at a graph: the vertex list, the edge rows (with weights), the degree of every vertex (as a row) or of one, the neighbours of v, the adjacency matrix A (A^k counts walks of length k), the Laplacian L = D - A (its zero eigenvalues count components; a cofactor counts spanning trees), the graph on some vertices with the edges between them, and the graph of the missing edges.

Example: `laplacian(cycle(4))`

Also found as `edges`, `neighbors`, `adjacency`, `complement`.

### components

`isconnected(G), components(G), isbipartite(G), istree(G), bfs(G, s), dfs(G, s), toposort(G)`

Structure: whether every vertex can reach every other, the connected components (strongly connected ones for a digraph), whether the vertices split in two sides with every edge across (no odd cycle), whether G is a tree, the breadth-first and depth-first orders from s, and an order of a digraph's vertices with every arrow pointing forward (a schedule for tasks with prerequisites).

Example: `components(graph([1, 2; 3, 4], 5))`

Also found as `isconnected`, `isbipartite`, `istree`, `bfs`, `dfs`, `toposort`.

### shortest

`shortest(G, s, t), dist(G, s, t), dist(G), diameter(G)`

Shortest paths: the vertices of a shortest path from s to t (fewest edges, or least total weight), its length, the matrix of all distances (inf where there is no path), and the largest distance. Dijkstra's algorithm for weights of 0 or more, Bellman-Ford when a digraph has negative weights.

Example: `shortest(grid(3, 3), 1, 9)`

Also found as `dist`, `diameter`.

### mst

`mst(G), spanningtrees(G), maxflow(G, s, t), mincut(G, s, t)`

A minimum spanning tree: the cheapest set of edges that connects every vertex (Kruskal); spanningtrees counts all the spanning trees (Kirchhoff's matrix-tree theorem). maxflow is the most that can flow from s to t when each edge's weight is its capacity (Edmonds-Karp; the note shows the flow on each edge), and mincut the cheapest set of edges whose removal separates s from t; the two numbers are equal (max-flow min-cut theorem).

Example: `maxflow(digraph([1, 2, 3; 1, 3, 2; 2, 3, 1; 2, 4, 2; 3, 4, 3]), 1, 4)`

Also found as `maxflow`, `spanningtrees`, `mincut`.

### matching

`matching(G), coloring(G), chromatic(G), chromaticpoly(G, k)`

matching: the largest set of edges with no shared vertex, one pair per row (Edmonds' blossom algorithm, any graph). coloring: a color 1, 2, ... for each vertex (as a row) so that joined vertices differ, with as few colors as possible; chromatic is that number; chromaticpoly is the number of such colorings with k colors, a polynomial in k (deletion-contraction, up to 45 edges).

Example: `chromaticpoly(cycle(4), k)`

Also found as `coloring`, `chromatic`, `chromaticpoly`.

### eulerian

`eulerian(G), hamiltonian(G)`

Tours: a walk that uses every edge exactly once (an Euler circuit or trail; it exists when the graph is connected and at most two vertices have odd degree), and a cycle through every vertex exactly once (Hamiltonian; up to 22 vertices). Each gives the vertices in order, or false when there is none.

Example: `eulerian(complete(5))`

Also found as `hamiltonian`.

### centrality

`centrality(G), centrality(G, "closeness"), centrality(G, "degree"), pagerank(G), pagerank(G, d)`

How central each vertex is, as a row: betweenness (how many shortest paths between other vertices pass through it), closeness (1 over the average distance to the others), degree, or PageRank (the share of time a random surfer spends there, following a random arrow with probability d = 0.85 and jumping anywhere otherwise).

Example: `centrality(path(5))`

Also found as `pagerank`.

### draw

`draw(G, path), draw(G, H), draw(G, edges), draw(G, coloring(G))`

A drawing of G with something marked: a path or cycle such as shortest(G, s, t) or hamiltonian(G), a graph on the same vertices such as mst(G), edge rows such as matching(G) or mincut(G, s, t), or a coloring from coloring(G). Graphs that come from cycle, grid, petersen and the like keep their own layout; others are drawn as trees, circles or with a spring layout.

Example: `G = grid(3, 4); draw(G, shortest(G, 1, 12))`

## Random processes

### stationary

`stationary(P), absorption(P), hitting(P, targets)`

Markov chains with transition matrix P (P(i, j) is the chance of going from state i to state j; rows add up to 1), exactly in fractions. stationary(P) is the long-run share of time in each state (pi P = pi; one row per closed class when there are several). absorption(P): for chains with absorbing states (P(i, i) = 1), row i holds the chances of ending in each absorbing state from state i, and the note gives the expected number of steps. hitting(P, targets): the expected number of steps to first reach one of the target states, from each state (inf where it may never happen). Powers P^n give the n-step chances.

Example: `stationary([1/2, 1/2, 0; 1/4, 1/2, 1/4; 0, 1/3, 2/3])`

Also found as `absorption`, `hitting`.

### randchain

`randchain(P, start, n), randwalk(n), randwalk(n, d), arrivals(rate, T), brownian(T, n), brownian(T, n, paths)`

Random paths (seed(k) first to repeat them): n steps of a Markov chain from a start state; a walk of n steps of +1 or -1 from 0 (or on the d-dimensional grid, one row per position, d up to 3); the arrival times of a Poisson process up to time T; and Brownian motion at the times 0, T/n, ..., T (several paths: one per row).

Example: `seed(1); randwalk(10)`

Also found as `randwalk`, `arrivals`, `brownian`.

### sde

`sde(f, g, x0, T, n), sde(f, g, x0, T, n, paths)`

Solves the stochastic differential equation dx = f(x, t) dt + g(x, t) dW from x(0) = x0 up to time T in n steps (the Euler-Maruyama method): a row of n + 1 values at the times linspace(0, T, n + 1), or one row per path. f and g are expressions in x and t, for example sde(0.1*x, 0.2*x, 100, 1, 250) for a stock price (geometric Brownian motion).

Example: `seed(1); sde(-x, 1, 2, 1, 10)`

### ising

`ising(n, T, sweeps)`

The Ising model of a magnet: spins +1 or -1 on an n by n grid that wraps around, each wanting to agree with its four neighbours, at temperature T, simulated by the Metropolis method for the given number of sweeps (n^2 flip attempts each). The result is a record: m (the average |magnetization| per spin), e (the average energy per spin), both over the second half of the sweeps, and spins (the final grid). Below Onsager's critical temperature 2.269 the spins line up.

Example: `seed(1); r = ising(10, 1.5, 100); r.m`

### bscall

`bscall(S, K, r, sigma, T), bsput(S, K, r, sigma, T)`

Black-Scholes prices of a European call and put option: stock price S, strike price K, interest rate r, volatility sigma (per year) and T years to expiry. The note gives d1, d2 and put-call parity.

Example: `bscall(100, 100, 0.05, 0.2, 1)`

Also found as `bsput`.

## Partial differential equations

### heat

`heat(f, x, a, b, T) or heat(f, x, a, b, T, {k: ..., left: ..., right: ..., source: ..., n: 100, frames: 50})`

Solves the heat equation u_t = k u_xx on a <= x <= b up to time T, starting from u = f(x), by Crank-Nicolson. The ends hold u = 0 unless left or right give a value, an expression in t, or "insulated" (no heat flows through); source adds a heat source in x and t. Returns a record {x, t, u} with one row of u per snapshot; show it with pdeplot.

Example: `s = heat(sin(pi*x), x, 0, 1, 0.1); pdeplot(s, "lines")`

### wave

`wave(f, g, x, a, b, T) or wave(f, g, x, a, b, T, {c: ..., left: ..., right: ..., damping: ..., n: 200, frames: 50})`

Solves the wave equation u_tt = c^2 u_xx on a <= x <= b up to time T, starting from u = f(x) and u_t = g(x), by the leapfrog scheme (Courant number 0.9 or less). The ends are held at 0 unless left or right give a value, an expression in t, or "free"; damping adds - damping u_t. Returns a record {x, t, u}; show it with pdeplot.

Example: `s = wave(exp(-100*(x - 1/2)^2), 0, x, 0, 1, 1); pdeplot(s)`

### poisson

`poisson(f, g, x, a, b, y, c, d) or laplace(g, x, a, b, y, c, d), with a last record {left: ..., right: ..., bottom: ..., top: ..., n: 40}`

Solves u_xx + u_yy = f (poisson) or = 0 (laplace with 7 or 8 arguments) on the rectangle a <= x <= b, c <= y <= d with u = g(x, y) on its edge, by the five-point formula and successive over-relaxation. left and right (expressions in y) or bottom and top (in x) set single edges. Returns a record {x, y, u} with u(j, i) the value at (x(i), y(j)); show it with pdeplot.

Example: `s = laplace(0, x, 0, 1, y, 0, 1, {top: sin(pi*x)}); pdeplot(s)`

### pdeplot

`pdeplot(s) or pdeplot(s, "animate" | "lines" | "map" | "surface")`

Shows a result of heat, wave, laplace or poisson: an animation in time (the default for heat and wave), snapshots side by side, a color map (the default for laplace and poisson, with level curves) or a surface.

Example: `pdeplot(heat(x*(1 - x), x, 0, 1, 0.2), "map")`

## Calculus of variations

### eulerlagrange

`eulerlagrange(L, y, x) or eulerlagrange(L, [q1 q2], t)`

The Euler–Lagrange equation d/dx(dL/dy') = dL/dy of a Lagrangian L in x, y and y': a path that makes the integral of L smallest (or stationary) between fixed end points satisfies it. Written as d(y, x, 2) == ... when it can be solved for the second derivative. Several unknowns give one equation each (Lagrangian mechanics). The note names conserved quantities: dL/dq' when L does not contain q, and energy(L, ...) when L does not contain x.

Example: `eulerlagrange(sqrt(1 + d(y, x)^2), y, x)`

### energy

`energy(L, y, x) or energy(L, [q1 q2], t)`

The sum of q' dL/dq' over the unknowns minus L. When L does not contain x (or t), it is constant along every solution of the Euler–Lagrange equations: the total energy in mechanics, Beltrami's first integral in geometry.

Example: `energy(m*d(x, t)^2/2 - k*x^2/2, x, t)`

### action

`action(L, y, x, f, a, b)`

The value of the functional: the integral of L(x, y, y') from x = a to b along the path y = f(x), exactly when possible, otherwise numerically. For several unknowns give a row of unknowns and a row of paths.

Example: `action(sqrt(1 + d(y, x)^2), y, x, x^2, 0, 1)`

### extremal

`extremal(L, y, x, a, b, ya, yb) or with a last argument n (pieces, default 100)`

Finds numerically the path y(x) from (a, ya) to (b, yb) that makes the integral of L(x, y, y') smallest: the path is cut into n straight pieces (crowded near the ends) and Newton's method lowers the integral. Returns a record {x, y, J} with J the value of the integral.

Example: `s = extremal(sqrt((1 + d(y, x)^2)/y), y, x, 0, pi, 0, 2); s.J`

### motion

`motion(L, q, t, q0, v0, T) or motion(L, [q1 q2], t, [..], [..], T) with a last argument steps (2000)`

Solves the equations of motion of a Lagrangian L(q, q', t) numerically (Runge–Kutta) from positions q0 and velocities v0 up to time T. Returns a record {t, q, v}: one row per coordinate. The note reports how well the energy was kept.

Example: `s = motion(d(x, t)^2/2 - x^2/2, x, t, 1, 0, 2*pi); s.q`
