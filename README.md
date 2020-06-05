# A model checker for CTL over constraint semirings
This project is a prototype of a model checker for CTL (computation tree logic) over constraint semirings.
Typical model checking is Boolean, i.e. it answers either *yes* or *no*, and can only handle properties defined over Boolean propositions.

To reason about quantitive aspects of systems, it is necessary to extend the classic approach.
This project extends the well-known temporal logic CTL by defining it over constraint semirings (c-semirings), which are algebraic structures that can capture quantitive aspects like quality of service.

A c-semiring has a domain and defines an additive and a multiplicative operation which must satisfy some simple properties.
It may also be distributive, in which case the model checking problem for it is decidable.

This project is based on the work presented by Lafuente and Montanari in the paper "Quantitative µ-calculus and CTL defined over constraint semirings".
