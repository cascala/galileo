[![GitHub issues](https://img.shields.io/github/issues/cascala/galileo.svg)](https://github.com/cascala/galileo/issues)

# Galileo

> **Galileo now lives on in Rust:** [cascala/galileo-rs](https://github.com/cascala/galileo-rs). The `cascala/galileo` and `cascala/igalileo` Docker images from 0.3.0 on use the Rust engine; tags 0.2.1 and earlier are this Scala version.

**▶ [Try Galileo in your browser](https://cascala.github.io/igalileo/)**: nothing to install. Open the [playground](https://cascala.github.io/igalileo/playground/), start the [first lesson](https://cascala.github.io/igalileo/playground/#lesson/foundations/01-numbers), or use [notebooks in JupyterLite](https://cascala.github.io/igalileo/lab/lab/index.html).

Galileo is a computer algebra system (CAS) written in Scala. It works with numbers like Matlab or Octave, and with symbols like Maple or Mathematica: variables can stay unknown, and expressions are simplified, differentiated and solved symbolically.

What sets it apart:
* **Tensors and differential geometry:** metrics (custom, ready-made, or induced on a surface), Christoffel symbols, the Riemann, Ricci, Einstein and Weyl tensors, Gaussian and Kretschmann curvature, and geodesics, with results simplified and typeset
* **Differential equations:** numerical geodesics and ODE solutions, drawn in 2D or 3D (`geodesic`, `odeplot`, `odesolve`)
* **Simplification rules**, through the `simplify` command
* **Symbolic matrices:** you can invert a matrix that contains symbols
* **Plotting:** 2D curves, parametric curves, 3D surfaces and space curves (`plot`, `parametric`, `plot3d`, `surface`, `parametric3d`, `show`)
* Logic operations, and a basic framework for rule-driven proofs (e.g. `prove(1/x==x^(-1))`)

Galileo comes as
* an [interpreted language](docs/language.md) with its own REPL,
* a [Jupyter kernel, iGalileo](https://github.com/cascala/igalileo), where plots are interactive, and
* a [library](docs/library.md) for Java and other JVM languages (Scala, Kotlin).

Type `help` for a list of commands, or `help(name)` for one of them.

```
galileo> f = x^3 - 2*x
x^3.0-2.0*x
galileo> deriv(f, x)
3.0*x^2.0-2.0
galileo> simplify(sin(y)^2 + cos(y)^2)
1.0
galileo> plot(f, deriv(f, x), x, -2, 2)
```

## Notebooks (recommended)

For notebooks with interactive plots, use [iGalileo](https://github.com/cascala/igalileo). It needs only Docker, and runs natively on Intel and Apple Silicon:

```
docker run --rm -p 8888:8888 cascala/igalileo
```

Open the `http://127.0.0.1:8888/lab?token=...` link it prints, then open the tour notebook.

## The REPL

### With Docker

```
docker run -it --rm cascala/galileo
```

The `cascala/galileo` image runs on Intel (amd64) and Apple Silicon (arm64). In the REPL, plots are drawn as text charts.

### From source

You need JDK 17 or later and [sbt](https://www.scala-sbt.org/).

```
git clone https://github.com/cascala/galileo.git
cd galileo
sbt test       # run the test suite
sbt run        # start the REPL
sbt assembly   # standalone jar: java -jar target/scala-2.13/Galileo-assembly-0.2.1.jar
```

## As a library

Galileo works from any JVM language. Version 0.1.3 is on [Maven Central](https://search.maven.org/search?q=g:com.github.cascala):

```
libraryDependencies += "com.github.cascala" %% "galileo" % "0.1.3"
```

```
<dependency>
    <groupId>com.github.cascala</groupId>
    <artifactId>galileo_2.13</artifactId>
    <version>0.1.3</version>
</dependency>
```

Version 0.2.1, which adds plotting and the current Scala and sbt versions, isn't published yet. Install it into your local repository with `sbt publishLocal` and depend on `"com.github.cascala" %% "galileo" % "0.2.1"`.

## Docker images

Build the image locally with `docker build -t cascala/galileo .`. To publish both architectures at once, see [docker.md](docker.md).

## Documentation

* [Language reference](docs/language.md): numbers, variables, simplification, calculus, plotting, matrices, tensors and differential geometry
* [Library reference](docs/library.md)
* Example scripts are in [examples/](examples/)

## Citing Galileo

Galileo is created by Joris Esch ([cascala](https://github.com/cascala)). If you use it in teaching, research or writing, please cite it: GitHub's **Cite this repository** button (in the sidebar) gives APA and BibTeX, from [CITATION.cff](CITATION.cff).

## License

Apache 2.0.
