# OMF (Ontological Modeling Framework) Core Functional API

OMF is a functional API for ontological modeling in the sense that it specifies:
- abstract types
- operations on abstract types

Ontological modeling combines two notions: 
- modeling a system in the sense of the Object Management Group (OMG) modeling specifications (e.g. the Unified Modeling Language (UML) and the Systems Modeling Language (SysML)) 
- describing a system according to a analyzable vocabulary for creating and reasoning about such descriptions in the sense of the World-Wide Web (W3C) Ontology Web Language (OWL) standard.

JPL developed frameworks and techniques to leverage the unique benefits of ontological modeling accross JPL's projects that have embraced the new paradigm of model-based systems engineering (MBSE). Currently, there is no standard for MBSE per se; however, there is a strong need for the analysis of systems models developed in the practice of MBSE. To address this need, JPL's Institutional Model-Centric Initiative (IMCE) developed an integration of the two leading standards for modeling (i.e. OMG's UML and SysML) and for ontologies (i.e. W3C's OWL, version 2). This integration involves several conventions, restrictions and patterns for using these standards to yield an analyzable coherent description of a system: an ontological model of that system. Evolving this integration to address the needs of JPL's MBSE practitioners led to the recognizing the need for a concise specification for ontological modeling; particularly one that does not require technical familiarities with the details of W3C's OWL standard or OMG's UML/SysML specifications. The OMF is precisely this concise specification of ontological modeling according to JPL's experience gained in the IMCE initiative.

The OMG core functional API adopts techniques from the field of functional programming languages, particularly Scala, for specifying the vocabulary of ontological modeling as a set of abstract types (that is, no commitment implied or assumed about any implementation of these abstract types) and a set of functional operations on these abstract types (that is, operations in the mathematical sense of pure functions that compute output values based on input values). This functional paradigm for specifying a domain allows a clean separation between the domain of ontological modeling (i.e., the focus of the OMF Core Functional API) from specific bindings for standards-based technology frameworks such as W3C's OWL, OMG's UML/SysML and, potentially, others. More importantly, the functional nature of this OMF Core API allows decoupling algorithms for analyzing, constructing, auditing, verifying, reasoning about OMF ontological models independently of the particular technology in which these ontological models are represented. 

## Fundamental Principles in OMF

1) Except for datatypes & data properties, OMF has only 3 kinds of classes (entities in OMF):

- Aspects
- Concepts
- Directed, binary relations

2) These classes can be organized in hierarchical taxonomies according to the following nominal subtyping rules:

- An aspect may specialize other aspects as long as the specialization hierarchy is acyclic.
- A concept may specialize other concepts and/or aspects as long as the specialization hierarchy is acyclic.
- A relation may specialize other relations and/or aspects as long as the specialization hierarchy is acyclic.

3) Each of the 3 kinds of entities designates a category of objects whose existence is asserted in terms of unique identifiers.

This means that except for the unique identifier, every object that is an instance of some kind of entity is completely opaque in OMF.
This is important because implementations need the flexibity to introduce whatever structure is necessary for the purposes of representing and managing OMF models in a particular platform (UML/SysML, OWL, XML Schema, database, etc...)

## Critique of OMF

1) OMF vs. OO

OMF is heavily inspired from the Alloy language, whose semantics is elegantly and succinctly summarized in just 1 page in Daniel Jackson's book.
That is, just like in Alloy, an OMF entity is fundamentally a relationship; there's no structure in the sense of object-oriented programming (e.g., class with attributes).
In terms of Alloy, an OMF relationship is a pattern involving 3 Alloy relations:
- an Alloy unary signature relation for the OMF relation (ie. a predicate about which instances in the world are OMF relations)
- an Alloy binary relation for the OMF relation "source"
- an Alloy binary relation for the OMF relation "target"

OMF Aspects and OMF Concepts map to Alloy unary signature relations. 

In conventional OO, there would not be any relation per se; rather, 
relations would be implicit in the definition of class attributes.

What's the difference between an Alloy-relational model OMF and an OO class/attribute model of OMF?

The difference is that an Alloy-relational model of OMF conforms to the 5th normal form of relational database theory
whereas an OO class/attribute model of OMF would violate the 4th normal form of relational database theory.
See [A Simple Guide to Five Normal Forms in Relational Database Theory](http://www.bkent.net/Doc/simple5.htm)

In terms of Java syntax, the Alloy-relational model of OMF would look like this:

```java

class Aspect {
}

class Concept {
}

class Relation {
  source: Entity
  target: Entity
}
```

This model is in 5th normal form because every table (i.e., Class) is at most a binary relationship.

In the OO class/attribute model, there would not be any OMF per se; rather, a domain would be modeled in terms of classes (i.e., domain concepts)
and the relations in the domain would be modeled in terms of class attribute properties.

This means that an OO class/attribute model for a particular domain could have domain-specific classes with 3 or more attribute properties: a violation of the 5th normal form. Of course, one could counter-argue that the OO class/attribute model for that domain could be refactored into 5th normal form. In theory, yes. In practice, it is rarely done. 


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
