***************************
**VFS**: Manipulating Files
***************************

.. highlight:: ada

Ada was meant from the beginning to be a very portable language, across
architectures. As a result, most of the code you write on one machine has
good chances of working as is on other machines. There remains, however,
some areas that are somewhat system specific. The Ada run-time, the GNAT
specific run-time and GNATColl all try to abstract some of those
operations to help you make your code more portable.

One of these areas is related to the way files are represented and
manipulated. Reading or writing to a file is system independent, and taken
care of by the standard run-time. Other differences between systems include
the way file names are represented (can a given file be accessed through
various casing or not, are directories separated with a backslash or a
forward slash, or some other mean, and a few others). The GNAT run-time does
a good job at providing subprograms that work on most types of filesystems,
but the relevant subprograms are split between several packages and not always
easy to locate. GNATColl groups all these functions into a single
convenient tagged type hierarchy. In addition, it provides the framework for
transparently manipulating files on other machines.

Another difference is specific to the application code: sometimes, a
subprogram needs to manipulate the base name (no directory information) of
a file, whereas sometimes the full file name is needed. It is somewhat hard
to document this in the API, and certainly fills the code with lots of
conversion from full name to base name, and sometimes reverse (which, of
course, might be an expansive computation). To make this easier,
GNATColl provides a type that encapsulates the notion of a file,
and removes the need for the application to indicate whether it needs a
full name, a base name, or any other part of the file name.

Filesystems abstraction
=======================

There exists lots of different filesystems on all machines. These include
such things as FAT, VFAT, NTFS, ext2, VMS,.... However, all these can
be grouped into three families of filesystems:

* windows-based filesystems

  On such filesystems, the full name of a file is split into three parts: the
  name of the drive (c:, d:,...), the directories which are separated by
  a backslash, and the base name. Such filesystems are sometimes inaccurately
  said to be case insensitive: by that, one means that the same file can be
  accessed through various casing. However, a user is generally expecting a
  specific casing when a file name is displayed, and the application should
  strive to preserve that casing (as opposed to, for instance, systematically
  convert the file name to lower cases).

  A special case of a windows-based filesystems is that emulated by the
  cygwin development environment. In this case, the filesystem is seen as if
  it was unix-based (see below), with one special quirk to indicate the drive
  letter (the file name starts with "/cygwin/c/").

* unix-based filesystems

  On such filesystems, directories are separated by forward slashed. File
  names are case sensitive, that is a directory can contain both "foo" and
  "Foo", which is not possible on windows-based filesystems.

* vms filesystem

  This filesystem represents path differently than the other two, using
  brackets to indicate parent directories

A given machine can actually have several file systems in parallel, when
a remote disk is mounted through NFS or samba for instance. There is
generally no easy way to guess that information automatically, and it
generally does not matter since the system will convert from the native file
system to that of the remote host transparently (for instance, if you mount
a windows disk on a unix machine, you access its files through forward slash-
separated directory names).

GNATColl abstracts the differences between these filesystems through
a set of tagged types in the `GNATCOLL.Filesystem` package and its
children. Such a type has primitive operations to manipulate the names of
files (retrieving the base name from a full name for instance), to check
various attributes of the file (is this a directory, a symbolic link, is the
file readable or writable), or to
manipulate the file itself (copying, deleting, reading and writing).
It provides similar operations for directories (creating or deleting paths,
reading the list of files in a directory,...).

It also provides information on the system itself (the list of available drives
on a windows machine for instance).

The root type `Filesystem_Record` is abstract, and is specialized in
various child types. A convenient factory is provided to return the filesystem
appropriate for the local machine (`Get_Local_Filesystem`), but you
might chose to create your own factory in your application if you have
specialized needs (:ref:`Remote_filesystems`).

file names encoding
-------------------

One delicate part when dealing with filesystems is handling files whose
name cannot be described in ASCII. This includes names in asian languages
for instance, or names with accented letters.

There is unfortunately no way, in general, to know what the encoding is for
a filesystem. In fact, there might not even be such an encoding (on linux,
for instance, one can happily create a file with a chinese name and another
one with a french name in the same directory). As a result, GNATColl
always treats file names as a series of bytes, and does not try to assume
any specific encoding for them. This works fine as long as you are
interfacing the system (since the same series of bytes that was returned by
it is also used to access the file later on).

However, this becomes a problem when the time comes to display the name for
the user (for instance in a graphical interface). At that point, you need to
convert the file name to a specific encoding, generally UTF-8 but not
necessarily (it could be ISO-8859-1 in some cases for instance).

Since GNATColl cannot guess whether the file names have a specific
encoding on the file system, or what encoding you might wish in the end, it
lets you take care of the conversion. To do so, you can use either of the
two subprograms `Locale_To_Display` and
`Set_Locale_To_Display_Encoder`

.. _Remote_filesystems:

Remote filesystems
==================

Once the abstract for filesystems exists, it is tempting to use it to
access files on remote machines. There are of course lots of differences
with filesystems on the local machine: their names are manipulated
similarly (although you need to somehow indicate on which host they are
to be found), but any operation of the file itself needs to be done on the
remote host itself, as it can't be done through calls to the system's
standard C library.

Note that when we speak of disks on a remote machine, we indicate disks
that are not accessible locally, for instance through NFS mounts or samba.
In such cases, the files are accessed transparently as if they were local,
and all this is taken care of by the system itself, no special layer is
needed at the application level.

GNATColl provides an extensive framework for manipulating such
remote files. It knows what commands need to be run on the remote host to
perform the operations ("cp" or "copy", "stat" or "dir /a-d",...) and
will happily perform these operations when you try to manipulate such
files.

There are however two operations that your own application needs to take
care of to take full advantage of remote files.

Filesystem factory
------------------

GNATColl cannot know in advance what filesystem is running on the
remote host, so it does not try to guess it. As a result, your application
should have a factory that creates the proper instance of a
`Filesystem_Record` depending on the host. Something like::

  type Filesystem_Type is (Windows, Unix);
  function Filesystem_Factory
    (Typ  : Filesystem_Type;
     Host : String)
    return Filesystem_Access
  is
     FS : Filesystem_Access;
  begin
     if Host = "" then
       case Typ is
         when Unix =>
           FS := new Unix_Filesystem_Record;
         when Windows =>
           FS := new Windows_Filesystem_Record;
       end case;
     else
       case Typ is
         when Unix =>
           FS := new Remote_Unix_Filesystem_Record;
           Setup (Remote_Unix_Filesystem_Record (FS.all),
                  Host      => Host,
                  Transport => ...); *--  see below*
         when Windows =>
           FS := new Remote_Windows_Filesystem_Record;
           Setup (Remote_Windows_Filesystem_Record (FS.all),
                  Host      => Host,
                  Transport => ...);
       end case;
     end if;

     Set_Locale_To_Display_Encoder
       (FS.all, Encode_To_UTF8'Access);
     return FS;
  end Filesystem_Factory;

Transport layer
---------------

There exists lots of protocols to communicate with a remote machine, so as
to be able to perform operations on it. These include protocols such as
`rsh`, `ssh` or `telnet`. In most of these cases, a user
name and password is needed (and will likely be asked to the user).
Furthermore, you might not want to use the same protocol to connect to
different machines.

GNATColl does not try to second guess your intention here. It
performs all its remote operations through a tagged type defined in
`GNATCOLL.Filesystem.Transport`. This type is abstract, and must be
overridden in your application. For instance, GPS has a full support for
choosing which protocol to use on which host, what kind of filesystem is
running on that host, to recognize password queries from the transport
protocol,.... All these can be encapsulated in the transport
protocol.

Once you have created one or more children of
`Filesystem_Transport_Record`, you associate them with your
instance of the filesystem through a call to the `Setup` primitive
operation of the filesystem. See the factory example above.

Virtual files
=============

As we have seen, the filesystem type abstracts all the operations for
manipulating files and their names. There is however another aspect when
dealing with file names in an application: it is often unclear whether a
full name (with directories) is expected, or whether the base name itself
is sufficient. There are also some aspects about a file that can be cached
to improve the efficiency.

For these reasons, GNATColl provides a new type
`GNATCOLL.VFS.Virtual_File` which abstracts the notion of file. It
provides lots of primitive operations to manipulate such files (which
are of course implemented based on the filesystem abstract, so support
files on remote hosts among other advantages), and encapsulate the base
name and the full name of a file so that your API becomes clearer (you
are not expecting just any string, but really a file).

This type is reference counted: it takes care of memory management on
its own, and will free its internal data (file name and cached data)
automatically when the file is no longer needed. This has of course a
slight efficiency cost, due to controlled types, but we have found in
the context of GPS that the added flexibility was well worth it.
