In the galileo shell, you can interact with mathematical expresssions.
The examples below illustrate the currently available features.

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
The imaginary unit is available as either i or j, with i*i=-1
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
You can take the derivative using the 'deriv' command.
```
galileo>deriv(x^2,x)
2.0*x 
```

## Matrices and tensors
### Matrix manipulation

The following commands are available:
* `lu(A)` LU factorization for matrix `A`
* `inv(A)` Inverse of matrix `A`
* `rand(n)` Random `n`x`n` matrix; elements chosen from uniform distribution
* `rand(n,m)` Random `n`x`m` matrix; elements chosen from uniform distribution 
* `eye(n)` `n`x`n` Identity matrix
* `ones(n)` `n`x`n` matrix filled with all ones
* `linspace`
* `logspace`

```
galileo>A=[a b;c d]
a    b
c    d
galile0>A[0]
a    b
galileo>A[0,1]
b
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
galileo>A=[3 4;5 b]
3.0	4.0
5.0	b
galileo>inv(A)
(-1.0)*b/((-3.0)*b+20.0)	3.0*b/(5.0*((-3.0)*b+20.0))+1.0/5.0
5.0/((-3.0)*b+20.0)	(-15.0)/(5.0*((-3.0)*b+20.0))
galileo> simplify(ans)
(-1.0)*b/((-3.0)*b+20.0)	3.0*b/((-15.0)*b+100.0)+1.0/5.0
5.0/((-3.0)*b+20.0)	(-15.0)/((-15.0)*b+100.0)
```
### Tensor manipulation
```
galileo> m=metric.generate(three-sphere)
Metric(Tensor(List(TensorIndex(Lower,3), TensorIndex(Lower,3)),List(r^2.0, 0.0, 0.0, 0.0, r^2.0*sin(psi)^2.0, 0.0, 0.0, 0.0, r^2.0*sin(psi)^2.0*sin(theta)^2.0)),List(psi, theta, phi ))
galileo> simplify(einsteintensor(m))
Tensor(List(TensorIndex(Lower,3), TensorIndex(Lower,3)),List(-1.0, 0.0, 0.0, 0.0, (-1.0)*sin(psi)^2.0, 0.0, 0.0, 0.0, sin(psi)^2.0*sin(theta)^2.0+(-1.0)*cos(psi)^2.0*sin(theta)^2.0+(-3.0)*sin(psi)^2.0*sin(theta)^2.0+sin(theta)^2.0))
```
