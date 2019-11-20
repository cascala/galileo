# Galileo
Galileo is the genesis of a symbolic and numerical math tool written in Scala; a Computer Algebra System (CAS).
It's similar to matlab or octave, but has many features of maple and mathematica as well in the sense that it supports variables and symbolic calculations.

Differentiating features:
* Support for tensor operations and differential geometry operations (Christoffel symbols, Ricci and Riemann tensors)
* Powerful simplification rules (accessible using the `simplify` command)
* Symbolic matrix manipulation (you can invert a matrix containing symbols)
* Support for logic operations
* Basic framework for rules driven proofs is available (e.g. `prove(1/x==x^(-1))`)

Galileo is available as both
* a [standalone, interpreted language](docs/language.md), with its own REPL, and, 
* a [library](docs/library.md) for use in JAVA or other JVM languages (Scala, Kotlin)

## Usage as a standalone tool (REPL)

### Running a pre-built Docker image
A Docker image of Galileo is available on Docker hub, and can be run as follows (on Mac or Linux):
```
docker run -it --rm cascala/galileo:0.1.3
```

### Building from source

Galileo can also be built from its source files using the 'sbt' build system:
```
git clone https://github.com/cascala/galileo.git
sbt run
```

## Usage as a library
You can use Galileo as a mathematical library in any JVM language (Java, Kotlin, Scala...).
For details, see:
[http://search.maven.org/#artifactdetails%7Ccom.github.cascala%7Cgalileo_2.11%7C0.1%7Cjar](http://search.maven.org/#artifactdetails%7Ccom.github.cascala%7Cgalileo_2.11%7C0.1%7Cjar)

For Maven, one can use:
```
<dependency>
    <groupId>com.github.cascala</groupId>
    <artifactId>galileo_2.11</artifactId>
    <version>0.1.1</version>
</dependency>
```

For sbt, one can use:
```
libraryDependencies += "com.github.cascala" %% "galileo" % "0.1.1"
```


## Detailed documentation
Detailed documentation is available for the 
* Galileo [language](docs/language.md), and, 
* Galileo [library](docs/library.md). 
