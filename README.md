[![GitHub issues](https://img.shields.io/github/issues/cascala/galileo.svg)](https://github.com/cascala/galileo/issues)

# Galileo

Galileo is the genesis of a symbolic and numerical math tool written in Scala; a Computer Algebra System (CAS).
It's similar to matlab or octave, but has many features of maple and mathematica as well in the sense that it supports variables and symbolic calculations.

Differentiating features:
* Differential geometry: metrics (custom, ready-made or induced on surfaces), Christoffel symbols, Riemann, Ricci, Einstein and Weyl tensors, Gaussian and Kretschmann curvature, geodesic equations, and numerical geodesics and ODEs with 2D/3D plots
* Powerful simplification rules (accessible using the `simplify` command)
* Symbolic matrix manipulation (you can invert a matrix containing symbols)
* Support for logic operations
* Basic framework for rules driven proofs is available (e.g. `prove(1/x==x^(-1))`)
* Plotting of expressions and parametric curves (`plot(sin(x), cos(x), x, 0, 2*pi)`): text charts in the REPL, SVG in Jupyter notebooks via [iGalileo](https://github.com/cascala/igalileo)

Galileo is available as both
* a [standalone, interpreted language](docs/language.md), with its own REPL, and, 
* a [library](docs/library.md) for use in JAVA or other JVM languages (Scala, Kotlin)

## Usage as a standalone tool (REPL)

### Running a pre-built Docker image
A Docker image of Galileo is available on Docker hub, and can be run as follows (on Mac or Linux):
```
$ docker run -it --rm cascala/galileo
```
To build the image yourself: `docker build -t cascala/galileo .`

### Building from source

Galileo can also be built from its source files using the [sbt](https://www.scala-sbt.org/) build system (JDK 17 or later):
```
$ git clone https://github.com/cascala/galileo.git
$ cd galileo
$ sbt test     # run the test suite
$ sbt run      # start the REPL
$ sbt assembly # build a standalone jar: java -jar target/scala-2.13/Galileo-assembly-0.2.0.jar
```

## Usage as a library
You can use Galileo as a mathematical library in any JVM language (Java, Kotlin, Scala...).
Version 0.1.3 is on Maven Central; 0.2.0 (with plotting) can be installed locally with `sbt publishLocal`. For details, see:
[https://search.maven.org/search?q=g:com.github.cascala](https://search.maven.org/search?q=g:com.github.cascala)

For Maven, one can use:
```
<dependency>
    <groupId>com.github.cascala</groupId>
    <artifactId>galileo_2.13</artifactId>
    <version>0.1.3</version>
</dependency>
```

For sbt, one can use:
```
libraryDependencies += "com.github.cascala" %% "galileo" % "0.1.3"
```


## Detailed documentation
Detailed documentation is available for the 
* Galileo [language](docs/language.md), and, 
* Galileo [library](docs/library.md). 
