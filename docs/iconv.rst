*************************************************
**Iconv**: Converting between character encodings
*************************************************

.. index:: iconv
.. index:: charset
.. highlight:: ada

This package provides a binding to the libiconv library. This library
is standard on most Unix systems. When it is not provided by the system,
the GNU libiconv package can be installed instead.

Compiling
=========

Since this is a binding to an external library, it is optional. By
default, GNATCOLL will automatically detect whether the package is
available on the system.

If the library is not found, a dummy version of :file:`gnatcoll-iconv.adb`
is provided, where the input is always returned unmodified. This ensures
that an application can use the API whether or not the library is available,
although of course the behavior will be different in both cases.

If your application depends on having a working libiconv, you can specify
`--with-iconv` or `--with-iconv=PATH` to GNATCOLL's configure. This will
ensure that configure fails if iconv is not found on the system.

On the other hand, you can avoid compiling GNATCOLL.Iconv altogether by
specifying `--without-iconv`. This however does not save much.

Using GNATCOLL.Iconv
====================

Since this is a binding to an external library, GNATCOLL installs a
separate project file for it so that applications must explicitly
declare they will use that package (and so that applications that do
not use this package do not inherit extra external dependencies).

You need to start your project file with::

     with "gnatcoll_iconv";
     project Default is
          ...
     end Default;

API
===

The whole API is documented in :file:`gnatcoll-iconv.ads`. Here is
a simple code sample that converts from iso-8859-1 encoding to UTF8::

    with GNATCOLL.Iconv;   use GNATCOLL.Iconv;
    procedure Main is
       EAcute : constant Character := Character'Val (16#E9#);
       --  in iso-8859-1

       Result : constant String := Iconv
          ("Some string " & EAcute,
           To_Code => UTF8,
           From_Code => ISO_8859_1);
    begin
       null;
    end Main;

A more advanced (and somewhat more efficient) API is available via the
`Iconv` procedure. In that procedure, you control the input and output
buffers, so you will need less overall memory when you are converting
big buffers.
