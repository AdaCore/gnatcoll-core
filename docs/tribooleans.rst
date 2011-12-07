.. _Three_state_logic:

**********************************
**Tribooleans**: Three state logic
**********************************

Through the package `GNATCOLL.Tribooleans`, GNATColl provides
a type that extends the classical `Boolean` type with an
`Indeterminate` value.

There are various cases where such a type is useful. One example we have
is when a user is doing a search (on a database or any set of data), and
can specify some optional boolean criteria ("must the contact be french?").
He can choose to only see french people ("True"), to see no french people
at all ("False"), or to get all contacts ("Indeterminate"). With a classical
boolean, there is no way to cover all these cases.

Of course, there are more advanced use cases for such a type. To support
these cases, the `Tribooleans` package overrides the usual logical
operations `"and"`, `"or"`, `"xor"`, `"not"` and
provides an `Equal` function.

See the specs of the package to see the truth tables associated with those
operators.

