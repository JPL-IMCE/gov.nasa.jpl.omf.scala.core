# Critique of OMF

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

In the OO class/attribute model, there would not be any OMF per se; rather, a domain would be modeled 
in terms of classes (i.e., domain concepts) and the relations in the domain would be modeled 
in terms of class attribute properties.

This means that an OO class/attribute model for a particular domain could have domain-specific classes 
with 3 or more attribute properties: a violation of the 5th normal form. Of course, one could counter-argue 
that the OO class/attribute model for that domain could be refactored into 5th normal form. 
In theory, yes. In practice, it is rarely done. 


