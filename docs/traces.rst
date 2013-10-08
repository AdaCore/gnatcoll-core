.. highlight:: ada

.. _Logging_information:

*******************************
**Traces**: Logging information
*******************************

Most applications need to log various kinds of information: error messages,
information messages or debug messages among others. These logs can be
displayed and stored in a number of places: standard output, a file, the
system logger, an application-specific database table,...

The package :file:`GNATCOLL.Traces` addresses the various needs, except for the
application-specific database, which of course is specific to your business
and needs various custom fields in any case, which cannot be easily provided
through a general interface.

This module is organized around two tagged types (used through access types,
in fact, so the latter are mentioned below as a shortcut):

*Trace_Handle*
  This type defines a handle (similar to a file descriptor in other contexts)
  which is latter used to output messages. An application will generally
  define several handles, which can be enabled or disabled separately, therefore
  limiting the amount of logging.

*Trace_Stream*
  Streams are the ultimate types responsible for the output of the messages.
  One or more handles are associated with each stream. The latter can be a file,
  the standard output, a graphical window, a socket,... New types of streams
  can easily be defined in your application.

.. _Configuring_traces:

Configuring traces
==================

As mentioned above, an application will generally create several
`Trace_Handle` (typically one per module in the application). When
new features are added to the application, the developers will generally
need to add lots of traces to help investigate problems once the application
is installed at a customer's site. The problem here is that each module
might output a lot of information, thus confusing the logs; this also does
not help debugging.

The `GNATCOLL.Traces` package allows the user to configure which handles
should actually generate logs, and which should just be silent and not
generate anything. Depending on the part of the application that needs to
be investigated, one can therefore enable a set of handles or another, to
be able to concentrate on that part of the application.

This configuration is done at two levels:

* either in the source code itself, where some `trace_handle`
  might be disabled or enabled by default. This will be described in more
  details in later sections.

* or in a configuration file which is read at runtime, and overrides
  the defaults set in the source code.

The configuration file is found in one of three places, in the following
order:

* The file name is specified in the source code in the call to
  `Parse_Config_File`.

  .. index:: ADA_DEBUG_FILE

* If no file name was specified in that call, the environment variable
  `ADA_DEBUG_FILE` might point to a configuration file.

  .. index:: .gnatdebug

* If the above two attempts did not find a suitable configuration file,
  the current directory is searched for a file called `.gnatdebug`.
  Finally, the user's home directory will also be searched for that file.

In all cases, the format of the configuration file is the same. Its goal is
to associate the name of a `trace_handle` with the name of a
`trace_stream` on which it should be displayed.

Streams are identified by a name. You can provide additional streams by
creating a new tagged object (:ref:`Defining_custom_stream_types`). Here are
the various possibilities to reference a stream:

*"name"*
  where name is a string made of letters, digits and slash ('/') characters.
  This is the name of a file to which the traces should be redirected. The
  previous contents of the file is discarded. If the name of the file is a
  relative path, it is relative to the location of the configuration file, not
  necessarily to the current directory when the file is parsed. If you used
  ">>" instead of ">" to redirect to that stream, the initial content of the
  file is not overridden, and new traces are appended to the file instead.

*"&1"*
  This syntax is similar to the one used on Unix shells, and indicates that
  the output should be displayed on the standard output for the application.
  If the application is graphical, and in particular on Windows platforms, it
  is possible that there is no standard output!

*"&2"*
  Similar to the previous one, but the output is sent to standard error.

*"&syslog"*
  :ref:`Logging_to_syslog`.

Comments in a configuration file must be on a line of their own, and start
with `--`. Empty lines are ignored. The rest of the lines represent
configurations, as in:

* If a line contains the single character `"+"`, it activates all
  `trace_handle` by default. This means the rest of the configuration
  file should disable those handles that are not needed. The default is that
  all handles are disabled by default, and the configuration file should
  activate the ones it needs. The Ada source code can change the default
  status of each handles, as well

* If the line starts with the character `">"`, followed by a
  stream name (as defined above), this becomes the default stream. All handles
  will be displayed on that stream, unless otherwise specified. If the stream
  does not exist, it defaults to standard output.

* Otherwise, the first token on the line is the name of a handle.
  If that is the only element on the line, the handle is activated, and will
  be displayed on the default stream.

  Otherwise, the next element on the line should be a `"="` sign,
  followed by either `"yes"` or `"no"`, depending on whether the
  handle should resp. be enabled or disabled.

  Finally, the rest of the line can optionally contain the `">"`
  character followed by the name of the stream to which the handle should
  be directed.

  There is are two special cases for the names on this line: they can
  start with either "*." or ".*" to indicate the settings apply to a whole
  set of handles. See the example below.

Here is a short example of a configuration file. It activates all handles
by default, and defines four handles: two of them are directed to the
default stream (standard error), the third one to a file on the disk,
and the last one to the system logger syslog (if your system supports it,
otherwise to the default stream, ie standard error)::

  +
  >&2
  MODULE1
  MODULE2=yes
  SYSLOG=yes >&syslog:local0:info
  FILE=yes >/tmp/file

  --  decorators (see below)
  DEBUG.COLORS=yes

  --  Applies to FIRST.EXCEPTIONS, LAST.EXCEPTIONS,...
  --  and forces them to be displayed on stdout
  *.EXCEPTIONS=yes > stdout

  --  Applies to MODULE1, MODULE1.FIRST,... This can be used to
  --  disable a whole hierarchy of modules.
  --  As always, the latest config overrides earlier ones, so the
  --  module MODULE1.EXCEPTIONS would be disabled as well.

  MODULE1.*=no

.. _Using_the_traces_module:

Using the traces module
=======================

If you need or want to parse an external configuration file as described
in the first section, the code that initializes your application should
contain a call to `GNATCOLL.Traces.Parse_Config_File`. As documented,
this takes in parameter the name of the configuration file to parse. When
none is specified, the algorithm specified in the previous section will be
used to find an appropriate configuration::

  GNATCOLL.Traces.Parse_Config_File;

The code, as written, will end up looking for a file :file:`.gnatdebug` in
the current directory.

The function :code:`Parse_Config_File` must be called to indicate that
you want to activate the traces. It must also end up finding a configuration
file. If it does not, then none of the other functions will ever output
anything. This is to make sure your application does not start printing extra
output just because you happen to use an external library that uses
:code:`GNATCOLL.Traces`. It also ensures that your application will not
try to write to :code:`stdout` unless you think it is appropriate (since
:code:`stdout` might not even exist in fact).

You then need to declare each of the `trace_handle` that your
application will use. The same handle can be declared several times, so
the recommended approach is to declare locally in each package body the
handles it will need, even if several bodies actually need the same
handle. That helps to know which traces to activate when debugging a
package, and limits the dependencies of packages on a shared package
somewhere that would contain the declaration of all shared handles.

.. index:: Function Trace_Handle

Function Trace_Handle Create Name Default Stream Factory Finalize
  This function creates (or return an existing) a `trace_handle` with
  the specified `Name`. Its default activation status can also be
  specified (through `Default`), although the default behavior is to
  get it from the configuration file. If a handle is created several times,
  only the first call that is executed can define the default activation
  status, the following calls will have no effect.

  `Stream` is the name of the stream to which it should be directed.
  Here as well, it is generally better to leave things to the configuration
  file, although in some cases you might want to force a specific behavior.

  `Factory` is used to create your own child types of `trace_handle`
  (:ref:`Log_decorators`).

Here is an example with two package bodies that define their own handles,
which are later used for output::

  package body Pkg1 is
     Me : constant Trace_Handle := Create ("PKG1");
     Log : constant Trace_Handle := Create ("LOG", Stream => "@syslog");
  end Pkg1;

  package body Pkg2 is
     Me : constant Trace_Handle := Create ("PKG2");
     Log : constant Trace_Handle := Create ("LOG", Stream => "@syslog");
  end Pkg2;

Once the handles have been declared, output is a matter of calling the
`GNATCOLL.Traces.Trace` procedure, as in the following sample::

     Trace (Me, "I am here");
  
An additional subprogram can be used to test for assertions (pre-conditions
or post-conditions in your program), and output a message whether the
assertion is met or not::

     Assert (Me, A = B, "A is not equal to B");
  
If the output of the stream is done in color, a failed assertion is
displayed with a red background to make it more obvious.

Logging unexpected exceptions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A special version of `Trace` is provided, which takes an
`Exception_Occurrence` as argument, and prints its message and backtrace
into the corresponding log stream.

This procedure will in general be used for unexcepted exceptions. Since
such exceptions should be handled by developers, it is possible to
configure `GNATCOLL.TRACES` to use special streams for those.

`Trace (Me, E)` will therefore not used `Me` itself as the log handle,
but will create (on the fly, the first time) a new handle with the same
base name and and `.EXCEPTIONS` suffix. Therefore, you could put the
following in your configuration file::

   # Redirect all exceptions to stdout
   *.EXCEPTIONS=yes >& stdout

and then the following code will output the exception trace to stdout::

   procedure Proc is
      Me : Create ("MYMODULE");
   begin
      ...
   exception
      when E : others =>
         Trace (Me, E, Msg => "unexcepted exception:");
   end Proc;

Checking whether the handle is active
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As we noted before, handles can be disabled. In that case, your application
should not spend time preparing the output string, since that would be
wasted time. In particular, using the standard Ada string concatenation
operator requires allocating temporary memory. It is therefore recommended,
when the string to display is complex, to first test whether the handle is
active. This is done with the following code::

  if Active (Me) then
     Trace (Me, A & B & C & D & E);
  end if;

.. _Log_decorators:

Log decorators
==============

.. index:: decorator, log

Speaking of color, a number of decorators are defined by
`GNATCOLL.Traces`. Their goal is not to be used for outputting information,
but to configure what extra information should be output with all log
messages. They are activated through the same configuration file as the
traces, with the same syntax (i.e either `"=yes"` or `"=no"`).

Here is an exhaustive list:

*DEBUG.ABSOLUTE_TIME*
  If this decorator is activated in the configuration file, the absolute time
  when Trace is called is automatically added to the output, when the
  streams supports it (in particular, this has no effect for syslog, which
  already does this on its own).

*DEBUG.MICRO_TIME*
  If active, the time displayed by DEBUG.ABSOLUTE_TIME will use a microseconds
  precision, instead of milliseconds.

*DEBUG.ELAPSED_TIME*
  If this decorator is activated, then the elapsed time since the last call to
  Trace for the same handle is also displayed.

*DEBUG.STACK_TRACE*
  If this decorator is activated, then the stack trace is also displayed. It can
  be converted to a symbolic stack trace through the use of the external
  application `addr2line`, but that would be too costly to do this
  automatically for each message.

*DEBUG.LOCATION*
  If this decorator is activated, the location of the call to Trace is
  automatically displayed. This is a file:line:column information. This
  works even when the executable wasn't compiled with debug information

*DEBUG.ENCLOSING_ENTITY*
  Activate this decorator to automatically display the name of the subprogram
  that contains the call to `Trace`.

*DEBUG.COLORS*
  If this decorator is activated, the messages will use colors for the various
  fields, if the stream supports it (syslog doesn't).

*DEBUG.COUNT*
  This decorator displays two additional numbers on each line: the first is
  the number of times this handle was used so far in the application, the second
  is the total number of traces emitted so far. These numbers can for instance
  be used to set conditional breakpoints on a specific trace (break on
  `gnat.traces.log` or `gnat.traces.trace` and check the value of
  `Handle.Count`. It can also be used to refer to a specific line in some
  comment file.

*DEBUG.FINALIZE_TRACES*
  This handle is activated by default, and indicates whether
  `GNATCOLL.Traces.Finalize` should have any effect. This can be set to False
  when debugging, to ensure that traces are available during the finalization
  of your application.

Here is an example of output where several decorators were activated. In this
example, the output is folded on several lines, but in reality everything is
output on a single line::

   [MODULE] 6/247 User Message (2007-07-03 13:12:53.46)
      (elapsed: 2ms)(loc: gnatcoll-traces.adb:224)
      (entity:GNATCOLL.Traces.Log)
      (callstack: 40FD9902 082FCFDD 082FE8DF )
  
Depending on your application, there are lots of other possible decorators
that could be useful (for instance the current thread, or the name of the
executable when you have several of them,...). Since `GNATCOLL.Traces`
cannot provide all possible decorators, it provides support, through tagged
types, so that you can create your own decorators.

This needs you to override the `Trace_Handle_Record` tagged type. Since
this type is created through calls to `GNATCOLL.Traces.Create`. This is done
by providing an additional `Factory` parameter to `Create`; this is
a function that allocates and returns the new handle.

Then you can override either (or both) of the primitive operations
`Pre_Decorator` and `Post_Decorator`. The following example creates
a new type of handles, and prints a constant string just after the module
name::

  type My_Handle is new Trace_Handle_Record with null record;
  procedure  Pre_Decorator
    (Handle  : in out My_Handle;
     Stream  : in out Trace_Stream_Record'Class;
     Message : String) is
  begin
     Put (Stream, "TEST");
     Pre_Decorator (Trace_Handle_Record (Handle), Stream, Message);
  end**;

  function Factory return Trace_Handle is
  begin
     return new My_Handle;
  end;

  Me : Trace_Handle := Create ("MODULE", Factory => Factory'Access);

As we will see below (:ref:`Dynamically_disabling_features`), you can also
make all or part of your decorators conditional and configurable through
the same configuration file as the trace handles themselves.

.. _Defining_custom_stream_types:

Defining custom stream types
============================

We noted above that several predefined types of streams exist, to output to
a file,
to standard output or to standard error. Depending on your specific needs,
you might want to output to other media. For instance, in a graphical
application, you could have a window that shows the traces (perhaps in
addition to filing them in a file, since otherwise the window would
disappear along with its contents if the application crashes); or you could
write to a socket (or even a CORBA ORB) to communicate with another
application which is charge of monitoring your application.

You do not need the code below if you simply want to have a new stream in
your application (for instance using one for logging Info messages, one for
Error messages, and so on). In this case, the function `Create` is all
you need.

`GNATCOLL.Traces` provides the type `Trace_Stream_Record`, which can
be overridden to redirect the traces to your own streams.

Let's assume for now that you have defined a new type of stream (called
`"mystream"`). To keep the example simple, we will assume this stream
also redirects to a file. For flexibility, however, you want to let the user
configure the file name from the traces configuration file. Here is an
example of a configuration file that sets the default stream to a file
called :file:`foo`, and redirects a specific handle to another file called
:file:`bar`. Note how the same syntax that was used for standard output and
standard error is also reused (ie the stream name starts with the `"&"`
symbol, to avoid confusion with standard file names)::

  >&mystream:foo
  MODULE=yes >&mystream:bar

You need of course to do a bit of coding in Ada to create the stream. This
is done by creating a new child of `Trace_Stream_Record`, and override
the two primitive operations `Put` and `Newline` (at least).
In this implementation, and because `GNATCOLL.Traces.Trace` takes care of
not outputting two messages at the same time, we can just output to the
file as characters are made available. In some other cases, however,
the implementation will need to buffer the characters until the end of
line is seen, and output the line with a single call. See for instance
the implementation of `GNATCOLL.Traces.Syslog`, which needs to do
exactly that::

  type My_Stream is new Trace_Stream_Record with record
     File : access File_Type;
  end record;

  procedure Put
    (Stream : in out My_Stream; Str : String) is
  begin
    Put (Stream.File.all, Str);
  end Put;

  procedure Newline (Stream : in out My_Stream) is
  begin
    New_Line (Stream.File.all);
  end Newline;

The above code did not open the file itself, as you might have noticed,
nor did it register the name `"mystream"` so that it can be used in
the configuration file. All this is done by creating a factory, ie a
function in charge of creating the new stream.

A factory is also a tagged object (so that you can store custom information
in it), with a single primitive operation, `New_Stream`, in charge of
creating and initializing a new stream.
This operation receives
in parameter the argument specified by the user in the configuration file
(after the `":"` character, if any), and must return a newly
allocated stream. This function is also never called twice with the
same argument, since `GNATCOLL.Traces` automatically reuses an existing
stream when one with the same name and arguments already exists::

  type My_Stream_Factory is new Stream_Factory with null record;

  overriding function New_Stream
     (Self : My_Stream_Factory; Args : String) return Trace_Stream
  is
     Str : access My_Stream := new My_Stream;
  begin
     Str.File := new File_Type;
     Open (Str.File, Out_File, Args);
     return Str;
  end Factory;

  Fact : access My_Stream_Factory := new My_Stream_Factory;
  Register_Stream_Factory ("mystream", Fact);
  

.. _Logging_to_syslog:

Logging to syslog
=================

.. index:: syslog
.. index:: gnat.traces.syslog

Among the predefined streams, GNATColl gives access to the system
logger `syslog`. This is a standard utility on all Unix systems, but is
not available on other systems. When you compile GNATColl, you should
specify the switch `--enable-syslog` to configure to activate the
support. If either this switch wasn't specified, or configure could not find
the relevant header files anyway, then support for `syslog` will not
be available. In this case, the package `GNATCOLL.Traces.Syslog` is still
available, but contains a single function that does nothing. If your
configuration files redirect some trace handles to `"syslog"`, they will
instead be redirect to the default stream or to standard output.

Activating support for syslog requires the following call in your application::

  GNATCOLL.Traces.Syslog.Register_Syslog_Stream;

This procedure is always available, whether your system supports or not
syslog, and will simply do nothing if it doesn't support syslog. This means
that you do not need to have conditional code in your application to handle
that, and you can let GNATColl take care of this.

After the above call, trace handles can be redirected to a stream named
`"syslog"`.

The package `GNATCOLL.Traces.Syslog` also contains a low-level interface
to syslog, which, although fully functional, you should probably not use,
since that would make your code system-dependent.

Syslog itself dispatches its output based on two criteria: the
`facility`, which indicates what application emitted the message,
and where it should be filed, and the `level` which indicates the
urgency level of the message. Both of these criteria can be specified in
the `GNATCOLL.Traces` configuration file, as follows::

    MODULE=yes >&syslog:user:error

The above configuration will redirect to a facility called `user`,
with an urgency level `error`. See the enumeration types in
:file:`gnatcoll-traces-syslog.ads` for more information on valid facilities
and levels.

.. _Dynamically_disabling_features:

Dynamically disabling features
==============================

Although the trace handles are primarily meant for outputting messages,
they can be used in another context. The goal is to take advantage of
the external configuration file, without reimplementing a similar
feature in your application. Since the configuration file can be used to
activated or de-activated a handle dynamically, you can then have
conditional sections in your application that depends on that handle,
as in the following example::

  CONDITIONAL=yes
  

and in the Ada code::

  package Pkg is
     Me : constant Trace_Handle := Create ("CONDITIONAL");
  begin
     if Active (Me) then
        ... conditional code
     end if;
  end Pkg;

In particular, this can be used if you write your own decorators, as
explained above.

