# Fundamental Principles in OMF

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


