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
ensure that configure fails if iconv is not found on the system. Specifying
an explicit path is recommended in general. If you specifying an explict
path, and also include the `--disable-shared` switch, GNATCOLL will always
use a static libconv found in the given path.

An alternative is to use `--with-iconv=static` to force the use of a static
library for iconv. This might be needed on some systems if you have link
errors in your application: some versions of libiconv define symbols such
as `__iconv_open` whereas others define `__libiconv_open`, and it is mandatory
to use the include file corresponding to the linked version of libiconv.
For instance, on OSX, the system provides /usr/include/iconv.h and
/usr/lib/libiconv.dylib, but you might also have the GNU version of libiconv
in /opt/local/include/iconv.h and /opt/local/lib/libiconv.dylib.
Unfortunately, these directories might also contain other libraries you
depend and it sometimes happens that /usr/include/iconv.h is used when
building, and /opt/local/lib/libiconv.dylib when linking. These are
incompatible. Using shared libraries solves this issue.

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
