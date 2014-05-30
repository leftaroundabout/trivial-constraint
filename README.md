Since GHC 7.4, constraints are first-class: we have the constraint kind, and thus type-classes have a kind such as `* -> Constraint`.

These can be used as parameters to data types. They also can be combined quite nicely,

    type NewConstraint a = (Constraint1 a, Constraint2 a)

however you always need to start with a plain old type class when building constraints.

This library provides a type class that is not really a constraint at all, so you can "start from zero" with building up a custom constraint.
