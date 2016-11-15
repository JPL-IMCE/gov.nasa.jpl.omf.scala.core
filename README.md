# OMF (Ontological Modeling Framework) Core Functional API

[![Build Status](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.omf.scala.core.svg?branch=master)](https://travis-ci.org/JPL-IMCE/gov.nasa.jpl.omf.scala.core)
[ ![Download](https://api.bintray.com/packages/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.omf.scala.core/images/download.svg) ](https://bintray.com/jpl-imce/gov.nasa.jpl.imce/gov.nasa.jpl.omf.scala.core/_latestVersion)

OMF is a functional API for ontological modeling in the sense that it specifies:
- abstract types
- operations on abstract types

Ontological modeling combines two notions: 
- modeling a system in the sense of the Object Management Group (OMG) modeling specifications (e.g. the Unified Modeling Language (UML) and the Systems Modeling Language (SysML)) 
- describing a system according to a analyzable vocabulary for creating and reasoning about such descriptions in the sense of the World-Wide Web (W3C) Ontology Web Language (OWL) standard.

JPL developed frameworks and techniques to leverage the unique benefits of ontological modeling accross JPL's projects that have embraced the new paradigm of model-based systems engineering (MBSE). Currently, there is no standard for MBSE per se; however, there is a strong need for the analysis of systems models developed in the practice of MBSE. To address this need, JPL's Institutional Model-Centric Initiative (IMCE) developed an integration of the two leading standards for modeling (i.e. OMG's UML and SysML) and for ontologies (i.e. W3C's OWL, version 2). This integration involves several conventions, restrictions and patterns for using these standards to yield an analyzable coherent description of a system: an ontological model of that system. Evolving this integration to address the needs of JPL's MBSE practitioners led to the recognizing the need for a concise specification for ontological modeling; particularly one that does not require technical familiarities with the details of W3C's OWL standard or OMG's UML/SysML specifications. The OMF is precisely this concise specification of ontological modeling according to JPL's experience gained in the IMCE initiative.

The OMG core functional API adopts techniques from the field of functional programming languages, particularly Scala, for specifying the vocabulary of ontological modeling as a set of abstract types (that is, no commitment implied or assumed about any implementation of these abstract types) and a set of functional operations on these abstract types (that is, operations in the mathematical sense of pure functions that compute output values based on input values). This functional paradigm for specifying a domain allows a clean separation between the domain of ontological modeling (i.e., the focus of the OMF Core Functional API) from specific bindings for standards-based technology frameworks such as W3C's OWL, OMG's UML/SysML and, potentially, others. More importantly, the functional nature of this OMF Core API allows decoupling algorithms for analyzing, constructing, auditing, verifying, reasoning about OMF ontological models independently of the particular technology in which these ontological models are represented. 

## OMF: Schema, Tables & API

- The [OMF Schema specification](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.omf.schema.specification) defines
  the abstract syntax of OMF models via an 
  [Eclipse Xcore](https://wiki.eclipse.org/Xcore) [metamodel](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.omf.schema.specification/blob/master/model/OMFSchema.xcore).
  
  In OMF a model is a terminology box graph; a particular configuration of a model is a terminology instance graph.
  Briefly, a terminology box graph describes classifiers of concepts, relationships & data.
  A terminology instance graph specifies particular individuals classified by definitions in a terminology box graph
   along with values of their data properties.
   
  The abstract syntax vocabulary pertaining to terminology box graphs is summarized in 3 diagrams:
  
  - [OMF Terms](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.omf.schema.specification/blob/master/model/OMF%20TBox.svg)
  - [OMF Term Axioms](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.omf.schema.specification/blob/master/model/OMF%20Term%20Axioms.svg)
  - [OMF Terminology Axioms](https://github.com/JPL-IMCE/gov.nasa.jpl.imce.omf.schema.specification/blob/master/model/OMF%20Terminology%20Axioms.svg)  

- The [OMF Schema & Provenance Tables](https://github.com/JPL-IMCE/jpl.omf.schema.tables)

  This is a polyglot library currently cross-compiled for two kinds of environments:
  - JVM, for Java and/or Scala applications
  - NPM, for JavaScript Node applications
  
  This library provides support for serializing and deserializing OMF models to/from Json representations
  of the 4th normal form OMF schema tables.
  
  For transformations to/from the [OMG Tool Interoperability API](https://github.com/TIWG/org.omg.oti.uml.core),
  this library provides support for serializing and deserializing OMF to OTI Provenance represented as
  Json pairs of OMF & OTI identification keys.
  
- The OMF/Core API (this project) is a strongly typed generic Scala API parameterized by an implementation type.
 
  Note that the OMF/Core API is currently behind w.r.t. the OMF Schema & Tables.
  
 
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

To install SBT, use a package manager for your system (Linux, MacOSX, Windows,...), 
see: [http://en.wikipedia.org/wiki/List_of_software_package_management_systems](http://en.wikipedia.org/wiki/List_of_software_package_management_systems)

### [Books about SBT](http://www.scala-lang.org/documentation/books.html)

- [SBT in Action, by Joshua D. Suereth](http://www.manning.com/suereth2/)

### SBT Plugins used in this project

- [sbt-mbee-plugin](https://github.jpl.nasa.gov/secae/sbt.mbee.plugin)

## Building the OMF Scala Core

* with the SBT command line

```
sbt -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)>
```

The SBT prompt will show:

```
omf-scala-core(<GIT branch>)>
```

* with IntelliJ

File > New > Project from Existing Sources...
Then choose SBT (not Eclipse,...)
In SBT, configure the VM parameters to include:

```
-DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)>
```

Then IntelliJ should be able to import the project properly.

### Updating the license header

The text of the license header is in the `build.sbt` file, to force updating all source files, use the follwing command:

```
sbt formatLicenseHeaders
```

### Building with SBT

```
sbt compile
```

### Building with Eclipse

This is deferred: it is unclear how to use Eclipse to develop projects with artifact-based dependencies
where project publish artifacts. Eclipse lacks a flexible artifact-aware build system like SBT.

There are artifact-aware build systems for Eclipse (e.g., Eclipse Maven Integration, Eclipse IvyDE, Eclipse Buckminster)
However, these typically require using a very verbose XML-based Maven POM file; definitely much more cumbersome
and complicated to maintain than an SBT build specification.

Finally, the reason is that very few open-source Scala projects use Eclipse alone.
The vast majority of open-source Scala projects use SBT, in which case support for Eclipse
is addressed via the SBT/Eclipse plugin, see: https://github.com/typesafehub/sbteclipse/wiki

## Unit Tests

Just as the OMF Scala Core is a parameterized functional API, the unit tests are also parameterized.
This means that the unit tests cannot be executed in the OMF Scala Core project as-is.
However, these unit tests can be executed by binding the type parameter; see the OMF Scala binding projects for details.

## Schedule (as of January 16, 2015)

### OMF/Core

	- [ ] Write unit tests for TBox structured datatypes
	- [ ] Write unit tests for ABox graphs

Completion criteria: unit tests should cover all of the OMF API (types & operations)

	- [ ] Select a Scala-aware code coverage tool
	Choices:
    		* http://scoverage.org
    		* Semmle .QL

### OMF/Binding/OWLAPI

	- [ ] Finish implementing the OMF API construction operations (OWLAPIOps: 12)
	- [ ] Finish implementing the OMF API loadTerminologyGraph
	- [ ] Finish implementing the OMF API loadInstanceGraph
	- [ ] Develop additional roundtrip unit tests loading/saving IMCE ontologies

### OMF/Binding/UML & SysML

	- [ ] An OMF model can be represented in several ways in UML/SysML

		- An ontology of UML (as implemented in a particular tool)
		- An ontology of SysML (as implemented in a particular tool, e.g., as a Profile)
		- An ontology of QUDV (as implemented in a particular tool, e.g., as a UML/SysML library)
		- An ontology of ISO 80K (as implemented in a particular tool, e.g., as a UML/SysML library)
		
		From an OMF perspective, all 4 cases above are OMF ModelTerminologyGraphs:
		- The TBox of the OMF model for UML includes OMF concepts (UML metaclasses) and OMF relationships (UML Associations); note that OMF's EntityConceptSubClassAxioms are simply UML Generalizations
		- The TBox of the OMF model for SysML includes OMF concepts (SysML stereotypes) and OMF relationships (UML Associations); note that OMF's EntityConceptSubClassAxioms can be either:
			- A UML Generalization between Stereotypes
			- A UML Extension between a Stereotype & a metaclass
		- The TBox of the OMF model for QUDV includes OMF concepts (SysML Blocks) and OMF relationships (UML associations); OMF's EntityConceptSubClassAxioms are the same as the case for the OMF model of UML
		- The TBox of the OMF model for ISO 80K includes OMF concepts (UML InstanceSpecifications classified by QUDV Blocks) and OMF relationships (UML InstanceSpecifications); OMF's EntityConceptSubClassAxioms are UML InstanceSpecifications classified by QUDV's Association for modeling generalization relationships between QUDV Units or QUDV QuantityKinds.
