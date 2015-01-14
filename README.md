# OMF (Ontological Modeling Framework) Core Functional API

OMF is a functional API for ontological modeling in the sense that it specifies:
- abstract types
- operations on abstract types

Ontological modeling combines two notions: 
- modeling a system in the sense of the Object Management Group (OMG) modeling specifications (e.g. the Unified Modeling Language (UML) and the Systems Modeling Language (SysML)) 
- describing a system according to a analyzable vocabulary for creating and reasoning about such descriptions in the sense of the World-Wide Web (W3C) Ontology Web Language (OWL) standard.

JPL developed frameworks and techniques to leverage the unique benefits of ontological modeling accross JPL's projects that have embraced the new paradigm of model-based systems engineering (MBSE). Currently, there is no standard for MBSE per se; however, there is a strong need for the analysis of systems models developed in the practice of MBSE. To address this need, JPL's Institutional Model-Centric Initiative (IMCE) developed an integration of the two leading standards for modeling (i.e. OMG's UML and SysML) and for ontologies (i.e. W3C's OWL, version 2). This integration involves several conventions, restrictions and patterns for using these standards to yield an analyzable coherent description of a system: an ontological model of that system. Evolving this integration to address the needs of JPL's MBSE practitioners led to the recognizing the need for a concise specification for ontological modeling; particularly one that does not require technical familiarities with the details of W3C's OWL standard or OMG's UML/SysML specifications. The OMF is precisely this concise specification of ontological modeling according to JPL's experience gained in the IMCE initiative.

The OMG core functional API adopts techniques from the field of functional programming languages, particularly Scala, for specifying the vocabulary of ontological modeling as a set of abstract types (that is, no commitment implied or assumed about any implementation of these abstract types) and a set of functional operations on these abstract types (that is, operations in the mathematical sense of pure functions that compute output values based on input values). This functional paradigm for specifying a domain allows a clean separation between the domain of ontological modeling (i.e., the focus of the OMF Core Functional API) from specific bindings for standards-based technology frameworks such as W3C's OWL, OMG's UML/SysML and, potentially, others. More importantly, the functional nature of this OMF Core API allows decoupling algorithms for analyzing, constructing, auditing, verifying, reasoning about OMF ontological models independently of the particular technology in which these ontological models are represented. 

## Scala

The OMF Scala Core relies extensively on techniques for functional programming in Scala.
This project is built with Java 1.8 and Scala 2.11.4

### Online training:
- [Introduction to Functional Programming in Scala, taught by Martin Odersky on Coursera](https://www.coursera.org/course/progfun) 

### [Books about Scala](http://www.scala-lang.org/documentation/books.html)

- [Scala In Depth, by Joshua D. Suereth](http://www.manning.com/suereth); in particular, see chapters 5,6,7
- [Functional Programming in Scala, by Paul Chusano & Runar Bjarnason](http://manning.com/bjarnason/); in particular, see part 3

## [Simple Build Tool, SBT](http://www.scala-sbt.org/documentation.html)

This project is built with SBT 0.13.7

To install SBT, use a package manager for your system (Linux, MacOSX, Windows,...), see: [http://en.wikipedia.org/wiki/List_of_software_package_management_systems](http://en.wikipedia.org/wiki/List_of_software_package_management_systems)

### [Books about SBT](http://www.scala-lang.org/documentation/books.html)

- [SBT in Action, by Joshua D. Suereth](http://www.manning.com/suereth2/)

### SBT Plugins used in this project

- [sbt-license-plugin](https://github.com/Banno/sbt-license-plugin)

## Building the OMF Scala Core

### Updating the license header

The text of the license header is in the `Build.sbt` file, to force updating all source files, use the follwing command:

```
sbt formatLicenseHeaders
```

### [Scala IDE](http://scala-ide.org), Version 4.0 on Eclipse Luna

The Eclipse `.classpath` file refers to third-party library dependency jar files in `coreLibs/target/pack/lib`
To download these library dependencies, open a shell window and run:

```
sbt coreLibs/pack
```

If the 3rd party libraries are updated in `project/Build.scala/coreLibs`, manually update the Eclipse `.classpath` file.

### Building with SBT

```
sbt coreLibs/pack
sbt publishLocal
```

### Building with Eclipse

Build the Scala project as usual with Eclipse

## Unit Tests

Just as the OMF Scala Core is a parameterized functional API, the unit tests are also parameterized.
This means that the unit tests cannot be executed in the OMF Scala Core project as-is.
However, these unit tests can be executed by binding the type parameter; see the OMF Scala binding projects for details.
