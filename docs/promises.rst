.. highlight:: ada

*************************************
**Promises**: deferring work
*************************************

This package provides a way to synchronize work between some
asynchronous workers (or threads).

Promises are a way to encapsulate a yet unknown value, immediately
return to the caller, and work in the background to actually
execute the work.

For instance, you could have a function that reads some data from
a socket. This takes time, and we do not want to block the application
while retrieving the data (and if there is an error retrieving it,
we certainly want to properly handle it).

The main thread (for instance a graphical user interface) needs to
keep processing events and refresh itself. As soon as the data
becomes available from the socket, we should let this main thread
know so that it can take further action, like post-processing the
data and then displaying it.

A general scheme to do that is to have a callback function that is
called whenever the work is finished. Promises build on that simple
idea so that you can easily chain multiple callbacks to build
more complex actions.

See the extensive documentation in :file:`gnatcoll-promises.ads`
