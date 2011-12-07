****************************************
**Ravenscar**: patterns for multitasking
****************************************

.. index:: ravenscar

GNATColl provides a set of patterns for concurrent programming using
Ravenscar-compliant semantics only. The core goal of the GNATCOLL.Ravenscar
(sub) packages is to ease the development of high-integrity multitasking
applications by factorizing common behavior into instantiable,
Ravenscar-compliant, generic packages. Instances of such generic packages
guarantee predictable timing behavior and thus permit the application of most
common timing analysis techniques.

Tasks
=====

The `GNATCOLL.Ravenscar.Simple_Cyclic_Task` generic package lets
instantiate a cyclic tasks executing the same operation at regular time
intervals; on the other side, the
`GNATCOLL.Ravenscar.Simple_Sporadic_Task` task lets instantiate sporadic
tasks enforcing a minimum inter-release time.

Servers
=======

Servers present a more sophisticated run-time semantics than tasks: for
example, they can fulfill different kind of requests (see multiple queues
servers).  `Gnat.Ravenscar.Sporadic_Server_With_Callback` and
`Gnat.Ravenscar.Timed_Out_Sporadic_Server` are particularly interesting. The
former shows how synchronous inter-task communication can be faked in Ravenscar
(the only form of communication permitted by the profile is through shared
resources): the server receives a request to fulfill, computes the result and
returns it by invoking a call-back. The latter enforces both a minimum and a
maximum inter-release time: the server automatically releases itself and
invokes an appropriate handler if a request is not posted within a given period
of time.

Timers
======

`Gnat.Ravenscar.Timers.One_Shot_Timer` is the Ravenscar implementation of
time-triggered event through Ada 2005 Timing Events.

