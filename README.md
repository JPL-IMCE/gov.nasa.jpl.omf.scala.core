# OMF (Ontological Modeling Framework) Core Functional API

OMF is a functional API for ontological modeling in the sense that it specifies:
- abstract types
- operations on abstract types

Ontological modeling combines two notions: 
- modeling a system in the sense of the Object Management Group (OMG) modeling specifications (e.g. the Unified Modeling Language (UML) and the Systems Modeling Language (SysML)) 
- describing a system according to a analyzable vocabulary for creating and reasoning about such descriptions in the sense of the World-Wide Web (W3C) Ontology Web Language (OWL) standard.

JPL developed frameworks and techniques to leverage the unique benefits of ontological modeling accross JPL's projects that have embraced the new paradigm of model-based systems engineering (MBSE). Currently, there is no standard for MBSE per se; however, there is a strong need for the analysis of systems models developed in the practice of MBSE. To address this need, JPL's Institutional Model-Centric Initiative (IMCE) developed an integration of the two leading standards for modeling (i.e. OMG's UML and SysML) and for ontologies (i.e. W3C's OWL, version 2). This integration involves several conventions, restrictions and patterns for using these standards to yield an analyzable coherent description of a system: an ontological model of that system. Evolving this integration to address the needs of JPL's MBSE practitioners led to the recognizing the need for a concise specification for ontological modeling; particularly one that does not require technical familiarities with the details of W3C's OWL standard or OMG's UML/SysML specifications. The OMF is precisely this concise specification of ontological modeling according to JPL's experience gained in the IMCE initiative.

The OMG core functional API adopts techniques from the field of functional programming languages, particularly Scala, for specifying the vocabulary of ontological modeling as a set of abstract types (that is, no commitment implied or assumed about any implementation of these abstract types) and a set of functional operations on these abstract types (that is, operations in the mathematical sense of pure functions that compute output values based on input values). This functional paradigm for specifying a domain allows a clean separation between the domain of ontological modeling (i.e., the focus of the OMF Core Functional API) from specific bindings for standards-based technology frameworks such as W3C's OWL, OMG's UML/SysML and, potentially, others. More importantly, the functional nature of this OMF Core API allows decoupling algorithms for analyzing, constructing, auditing, verifying, reasoning about OMF ontological models independently of the particular technology in which these ontological models are represented. 

## SBT

### Updating license header

```
sbt formatLicenseHeaders
```

### Eclipse

The Eclipse `.classpath` file refers to third-party library dependency jar files in `coreLibs/target/pack/lib`
To download these library dependencies, open a shell window and run:

```
sbt coreLibs/pack
```

### Building with SBT

```
sbt coreLibs/pack
sbt publishLocal
```


