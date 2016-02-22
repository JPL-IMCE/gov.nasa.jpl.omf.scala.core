# OMF Datatypes

The current OMF design for scalar datatypes needs to be revised.

Currently, OMF scalar datatypes reflect XML Schema 1.1 Datatypes
(see: https://www.w3.org/TR/xmlschema11-2/#typesystem)

However, OWL2 uses a different vocabulary that does not map easily
to that of XML Schema 1.1 Datatypes:

    DataRange :=
        Datatype |
        DataIntersectionOf |
        DataUnionOf |
        DataComplementOf |
        DataOneOf |
        DatatypeRestriction

    Datatype := IRI

    DataIntersectionOf := 'DataIntersectionOf' '(' DataRange DataRange { DataRange } ')'

    DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'

    DataComplementOf := 'DataComplementOf' '(' DataRange ')'

    DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'

    DatatypeRestriction := 'DatatypeRestriction' '(' Datatype constrainingFacet restrictionValue { constrainingFacet restrictionValue } ')'
    constrainingFacet := IRI
    restrictionValue := Literal

The problem is that there is no simple mapping between OWL2's intersection, union & complement
and the restriction-based constructs available in XML Schema 1.1 Datatypes.

For example, mapping OWL2 `DataUnionOf(dt1, ... dtN)` would require either
- creating a new XML Schema 1.1 datatype, `dTop`, and redefining `dt1,...., dtN` as restrictions of it; or
- resolving `dTop` as the common restriction parent of `dt1, ..., dtN`.