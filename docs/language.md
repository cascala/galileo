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

The 'simplify' command simplifies an expression:
```
m=metric.generate(three-sphere)
et=einsteintensor(m)
```
The Einstein tensor for any metric contains combinations of second order derivatives of the metric expression, which can be quite complex before simplification:
```
Tensor(List(TensorIndex(Lower,3), TensorIndex(Lower,3)),List(((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0-2.0*r^2.0*sin(psi)^2.0)*r^2.0*sin(psi)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)/(4.0*r^4.0*sin(psi)^4.0)+((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0*sin(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-1.0)*((((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0-2.0*r^2.0*sin(psi)^2.0)*r^2.0*sin(psi)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)/(4.0*r^4.0*sin(psi)^4.0)+((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0*sin(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/r^2.0+(2.0*((-2.0)*r^2.0*cos(psi)^2.0+2.0*r^2.0*sin(psi)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^2.0)+(-1.0)*(2.0*(2.0*r^2.0*sin(psi)^2.0*cos(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/(r^2.0*sin(psi)^2.0)+(2.0*((-2.0)*r^2.0*cos(psi)^2.0*sin(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+2.0*((-2.0)*r^2.0*sin(psi)^2.0*cos(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0))/(r^2.0*sin(psi)^2.0*sin(theta)^2.0))*r^2.0/2.0, 4.0*r^4.0*sin(psi)^2.0*cos(psi)*sin(psi)*cos(theta)*sin(theta)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^2.0*sin(theta)^2.0*cos(psi)*sin(psi)*cos(theta)*sin(theta)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0), 0.0, 4.0*r^4.0*sin(psi)^2.0*cos(psi)*sin(psi)*cos(theta)*sin(theta)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^2.0*sin(theta)^2.0*cos(psi)*sin(psi)*cos(theta)*sin(theta)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0), 2.0*((-2.0)*r^2.0*cos(psi)^2.0+2.0*r^2.0*sin(psi)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^2.0)+(-1.0)*(2.0*(2.0*r^2.0*sin(psi)^2.0*cos(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-1.0)*r^2.0*sin(psi)^2.0*((((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0-2.0*r^2.0*sin(psi)^2.0)*r^2.0*sin(psi)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)/(4.0*r^4.0*sin(psi)^4.0)+((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0*sin(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/r^2.0+(2.0*((-2.0)*r^2.0*cos(psi)^2.0+2.0*r^2.0*sin(psi)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^2.0)+(-1.0)*(2.0*(2.0*r^2.0*sin(psi)^2.0*cos(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/(r^2.0*sin(psi)^2.0)+(2.0*((-2.0)*r^2.0*cos(psi)^2.0*sin(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+2.0*((-2.0)*r^2.0*sin(psi)^2.0*cos(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0))/(r^2.0*sin(psi)^2.0*sin(theta)^2.0))/2.0, 0.0, 0.0, 0.0, 2.0*((-2.0)*r^2.0*cos(psi)^2.0*sin(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+2.0*((-2.0)*r^2.0*sin(psi)^2.0*cos(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-1.0)*r^2.0*sin(psi)^2.0*((((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0-2.0*r^2.0*sin(psi)^2.0)*r^2.0*sin(psi)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0)/(4.0*r^4.0*sin(psi)^4.0)+((-1.0)*(2.0*(2.0*r^2.0*cos(psi)^2.0*sin(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)-4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/r^2.0+(2.0*((-2.0)*r^2.0*cos(psi)^2.0+2.0*r^2.0*sin(psi)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^2.0)+(-1.0)*(2.0*(2.0*r^2.0*sin(psi)^2.0*cos(theta)^2.0-2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0*sin(theta)^2.0-8.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0)/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^4.0))/(r^2.0*sin(psi)^2.0)+(2.0*((-2.0)*r^2.0*cos(psi)^2.0*sin(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0/(4.0*r^4.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+2.0*((-2.0)*r^2.0*sin(psi)^2.0*cos(theta)^2.0+2.0*r^2.0*sin(psi)^2.0*sin(theta)^2.0)*r^2.0*sin(psi)^2.0/(4.0*r^4.0*sin(psi)^4.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+(-4.0)*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+(-4.0)*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0)+4.0*r^4.0*cos(psi)^2.0*sin(psi)^2.0*sin(theta)^4.0/(4.0*r^4.0*sin(psi)^2.0*sin(theta)^2.0)+4.0*r^4.0*sin(psi)^4.0*cos(theta)^2.0*sin(theta)^2.0/(4.0*r^4.0*sin(psi)^4.0*sin(theta)^2.0))/(r^2.0*sin(psi)^2.0*sin(theta)^2.0))*sin(theta)^2.0/2.0))
```
The 'simplify' command reduces the complexity of this expression:
```
simplify(einsteintensor(metric.generate(three-sphere)))
```
returns a mere
```
Tensor(List(TensorIndex(Lower,3), TensorIndex(Lower,3)),List(-1.0, 0.0, 0.0, 0.0, (-1.0)*sin(psi)^2.0, 0.0, 0.0, 0.0, (-4.0)*sin(psi)^2.0*sin(theta)^2.0/4.0))
```

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

## Plotting
`plot` samples one or more expressions over a range of a variable. In the REPL it draws a text chart; in a Jupyter notebook (iGalileo) it renders as an SVG image.
```
galileo>plot(sin(x), x, 0, 2*pi)
galileo>plot(sin(x), cos(x), x^2/10, x, -pi, pi)
galileo>f = x^3 - 2*x
galileo>plot(f, deriv(f, x), x)
```
The range is optional and defaults to -10 .. 10. User-defined variables and constants (pi, e) are substituted, and poles (such as `1/x` or `tan(x)`) are clipped rather than stretching the chart.

`parametric` plots a curve given by two expressions of one parameter:
```
galileo>parametric(cos(3*t), sin(2*t), t, 0, 2*pi)
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
ds^2 = r^2.0*dpsi^2 + r^2.0*sin(psi)^2.0*dtheta^2 + r^2.0*sin(psi)^2.0*sin(theta)^2.0*dphi^2
galileo> einsteintensor(m)
Einstein tensor (G, coordinates psi, theta, phi), non-zero components:
  G_psi,psi = -1.0
  G_theta,theta = (-1.0)*sin(psi)^2.0
  G_phi,phi = (-1.0)*sin(psi)^2.0*sin(theta)^2.0
```

## Differential geometry
Metrics can be written down directly, taken from a list of ready-made ones, or induced on a parametrised surface.
Curvature results are simplified automatically and listed as their non-zero, independent components
(in a Jupyter notebook they are typeset with LaTeX).

| Command | Result |
|---|---|
| `metric([r phi], [1 0; 0 r^2])` | metric g_ab with the given coordinates and components (shown as a line element ds^2) |
| `metric.generate(name)` | ready-made metric: `minkowski`, `euclidean`, `polar`, `spherical`, `two-sphere`, `three-sphere`, `torus`, `hyperbolic`, `schwarzschild` |
| `inducedmetric([x y z], [u v])` | first fundamental form of the surface (x(u,v), y(u,v), z(u,v)) |
| `inverse(g)` | inverse metric g^ab |
| `christoffel(g)` | Christoffel symbols Gamma^a_bc (also `christoffelfirst`, `christoffelsecond`) |
| `riemannsecond(g)`, `riemannfirst(g)` | Riemann tensor R^a_bcd, R_abcd |
| `riccitensor(g)`, `ricciscalar(g)` | Ricci tensor and scalar |
| `einsteintensor(g)`, `weyltensor(g)` | Einstein and Weyl tensors |
| `gausscurvature(g)` | Gaussian curvature of a 2D metric |
| `kretschmann(g)` | Kretschmann scalar R_abcd R^abcd |
| `geodesics(g)` | the geodesic equations |
| `geodesic(g, x0, v0, s1, map)` | numerically integrated geodesic(s) from position x0 with velocity v0 (several rows of v0 give several geodesics), plotted through `map` (2 or 3 expressions in the coordinates) |
| `odeplot([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1, map)` | numerical solution of dy/dt = f(t, y), plotted through `map` (2D or 3D) |
| `odesolve([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1)` | the values y(t1) |

Matrix elements are separated by spaces or commas; use commas when an element starts with a minus sign (`[v, -x]`), because `[v -x]` means `[v-x]`.

```
galileo> g = metric.generate(two-sphere)
ds^2 = r^2.0*dtheta^2 + r^2.0*sin(theta)^2.0*dphi^2
galileo> christoffel(g)
Christoffel symbols of the second kind (Gamma, coordinates theta, phi), non-zero components:
  Gamma^theta_phi,phi = (-1.0)*cos(theta)*sin(theta)
  Gamma^phi_theta,phi = cos(theta)/sin(theta)
galileo> gausscurvature(g)
1.0/r^2.0
galileo> gausscurvature(inducedmetric([(R+a*cos(u))*cos(v) (R+a*cos(u))*sin(v) a*sin(u)], [u v]))
cos(u)/(a*(R+a*cos(u)))
galileo> s = metric.generate(schwarzschild)
galileo> riccitensor(s)
Ricci tensor (R, coordinates t, r, theta, phi): all components are zero
galileo> kretschmann(s)
12.0*r_s^2.0/r^6.0
galileo> r = 1
galileo> geodesic(g, [pi/2 0], [1 1], 6, [sin(theta)*cos(phi) sin(theta)*sin(phi) cos(theta)])
```
The last command draws a great circle on the unit sphere (a 3D graphic in Jupyter).
See the [iGalileo differential geometry tour](https://github.com/cascala/igalileo/blob/master/examples/differential-geometry-tour.ipynb) for 3D graphics and physics examples, including the perihelion precession of Mercury.

