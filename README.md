# Galileo
Galileo is the genesis of a symbolic and numerical math tool written in Scala; a Computer Algebra System (CAS).
It's similar to matlab or octave, but has many features of maple and mathematica as well in the sense that it supports variables and symbolic calculations.


Differentiating features:
* Support for tensor operations and differential geometry operations (Christoffel symbols, Ricci and Riemann tensors)
* Powerful simplification rules (accessible using the `simplify` command)
* Symbolic matrix manipulation (you can invert a matrix containing symbols)
* Support for logic operations
* Basic framework for rules driven proofs is available (e.g. `prove(1/x==x^(-1))`)

## Installation and running
Galileo uses the 'sbt' build system. After cloning the repository, running
```
sbt run
```
will launch the galileo interactive shell.

Galileo also provides a de-facto Scala API for symbolic math, the best way to explore its use is by looking at the test scripts.

## Basics
### Working with numbers
```
galileo>5
5
galileo>5+6
11
```
By default, galileo does not evaluate fractions. The 'eval' command can be used to force the evaluation.
```
galileo>5/4
5/4
galileo>eval(5/4)
1.25
```

### Working with variables
```
galileo>x+7
x+7
galileo>x=3
3
galileo>x+7
10
```

### Complex numbers
```
galileo>2+3*i
3*j+2
galileo>j*j
-1
```

## Manipulation of expressions
The commands 'simplify', 'factor' and 'expand' can be used to manipulate expressions.

## Logic operations
```
galileo>true && false
false
galileo>or(false,true)
true
```

## Basic calculus
```
galileo>deriv(x^2,x)
2*x 
```

## Matrices and tensors
### Matrix manipulation

```
galileo>A=[a b;c d]
a    b
c    d
galileo>[L,U,P]=lu(A)
L	=
1.0	0.0	
c/a	1.0	

U	=
a	b	
0.0	(-1.0)*b*c/a+d	

P	=
1.0	0.0
0.0	1.0
galileo>L*U
a	b
c	d
galileo>P*A
a	b
c	d
```
### Tensor manipulation

