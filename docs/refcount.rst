********************************
**Refcount**: Reference counting
********************************

.. highlight:: ada

Memory management is often a difficulty in defining an API. Should we let
the user be responsible for freeing the types when they are no longer needed,
or can we do it automatically on his behalf ?

The latter approach is somewhat more costly in terms of efficiency (since
we need extra house keeping to know when the type is no longer needed), but
provides an easier to use API.

Typically, such an approach is implemented using reference counting: all
references to an object increment a counter. When a reference disappears,
the counter is decremented, and when it finally reaches 0, the object is
destroyed.

.. index:: reference counting

This approach is made convenient in Ada using controlled types. However,
there are a number of issues to take care of to get things exactly right.
In particular, the Ada Reference Manual specifies that `Finalize`
should be idempotent: it could be called several times for a given object,
in particular when exceptions occur.

An additional difficulty is task-safety: incrementing and decrementing the
counter should be task safe, since the controlled object might be referenced
from several task (the fact that other methods on the object are task safe
or not is given by the user application, and cannot be ensures through the
reference counting mecanism).

To make things easier, GNATColl provides the package
`GNATCOLL.Refcount`. This package contains a generic child package.

To use it, you need to create a new tagged type that extends
`GNATCOLL.Refcount.Refcounted`, so that it has a counter. Here is an
example::

    with GNATCOLL.Refcount;  use GNATCOLL.Refcount;

    package My_Pkg is
       type My_Type is new Refcounted with record
          Field1 : ...;   --  Anything
       end record;

       package My_Type_Ptr is new Smart_Pointers (My_Type);
    end My_Pkg;

The code above makes a `Ref` available. This is similar in semantics
to an access type, although it really is a controlled type. Every time you
assign the `Ref`, the counter is incremented. When the `Ref` goes
out of scope, the counter is decremented, and the object is potentially
freed.

Here an example of use of the package::

    declare
       R   : Ref;
       Tmp : My_Type := ...;
    begin
       Set (R, Tmp);           --  Increment counter
       Get (R).Field1 := ...;  --  Access referenced object
    end;

    --  R out of scope, so decrement counter, and free Tmp
  
Although reference counting solves most of the issues with memory management,
it can get tricky: when there is a cycle between two reference counted objects
(one includes a reference to the other, and the other a reference to the
first), their counter can never become 0, and thus they are never freed.

There is in particular when common design where this can severly interfer:
imagine you want to have a `Map`, associating a name with a reference
counted object. Typically, the map would be a cache of some sort. While the
object exists, it should be referenced in the map. So we would like the Map
to store a reference to the object. But that means the object will then
never be freed while the map exists either, and memory usage will only
increase.

.. index:: reference, weak

The solution to this issue is to use `weak references`. These hold
a pointer to an object, but do not increase its counter. As a result,
the object can eventually be freed. At that point, the internal data in
the weak reference is reset to `null`, although the weak reference
object itself is still valid.

Here is an example::

    with GNATCOLL.Refcount.Weakref;
    use GNATCOLL.Refcount.Weakref;

    type My_Type is new Weak_Refcounted with...;

    package Pointers is new Weakref_Pointers (My_Type);

The above code can be used instead of the code in the first example, and
provides the same capability (smart pointers, reference counted types,...).
However, the type `My_Type` is slightly bigger, but can be used to
create weak references::

    WR : Weak_Ref;

    declare
       R   : Ref;
       Tmp : My_Type := ...;
    begin
       Set (R, Tmp);           --  Increment counter
       WR := Get_Weak_Ref (R); --  Get a weak reference

       Get (R).Field1 := ...;  --  Access referenced object
       Get (Get (WR)).Field1 := ...;  --  Access through weak ref
    end;

    --  R out of scope, so decrement counter, and free Tmp

    if Get (WR) /= Null_Ref then  --  access to WR still valid
        --  Always true, since Tmp was freed
    end if;

The example above is very simplified. Imagine, however, that you store
`WR` in a map. Even when `R` is deallocated, the contents of the
map remains accessible without a `Storage_Error` (although using
`Get` will return `Null_Ref`, as above).

For task-safety issues, `Get` on a weak-reference returns a smart
pointer. Therefore, this ensures that the object is never freed while that
smart pointer object. As a result, we recommend the following construct in
your code::

     declare
       R : constant Ref := Get (WR);
     begin
       if R /= Null_Ref then
          --  Get (R) never becomes null while in this block
       end if;
     end;
  

