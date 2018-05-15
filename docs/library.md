# Galileo library

The Galileo math library currently has the following mathematical objects:
* Number
* Constant
* Variable
* Sum, Product and Fraction
* Exp and Log
* Trigonometric functions: CosF1, SinF1, TanF1 and their inverse: ACosF1, ASinF1, ATanF1
* Matrix, with methods `solve`, LU factorization (implemented in method `_lup`)
* Specific matrix implementations for sparse matrices and matrices with special structures:
  * LowerTriangularMatrix
  * UpperTriangularMatrix
  * OnesMatrix
  * EyeMatrix
  * DiagMatrix
* Tensor, supporting contraction of lower/upper indices

All of these objects implement the generic _Expr_ trait.

Sample projects that use the Galileo library are available here:
* [Java sample](https://github.com/cascala/galileo-sample-java)
* [Kotlin sample](https://github.com/cascala/galileo-sample-kotlin)
