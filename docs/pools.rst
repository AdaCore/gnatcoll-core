******************************************
**Pools**: Controlling access to resources
******************************************

The package **GNATCOLL.Pools** provides resource pools.

A pool contains a maximum number of resources, which are created on demand.
However, once a resource is no longer needed by the client, it is not freed,
but instead it is released to the pool, which will then return it again the
next time a client requests a resource.

The typical resource is when the creation of the resources is expensive, for
instance a connection to a database or a remote server. The lazy creation then
provides a faster startup time (as well as more flexibility, since there is no
need to allocate dozens of resources if only one will be needed in the end),
and more efficient retrieval through the reuse of resources.

The pool in this package is task safe, and is intended as a global variable
(or field of a global variable) somewhere in your application.

The resources are implemented as reference-counted types (through
`GNATCOLL.Refcount`). As a result, as soon as the client no longer has
a handle on them, they are automatically released to the pool and there is
no risk that the client forgets to do so.

`GNATCOLL.Pools` is a generic package where the formal parameters
describe the type of resources, how to create them on demand, what should
happen when a resource is released, and finally how to free a resource when
the pool itself is freed. See :file:`gnatcoll-pools.ads` for a full and
up-to-date description of these parameters.

