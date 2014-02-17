.. highlight:: ada

.. _Xref:

***************************************
**Xref**: Cross-referencing source code
***************************************

When manipulating source code, programmers needs to know where the various
symbols are defined, where they are used, and so on. This is generally
available directly from their IDE and editors. But computing this information
in the first place is tricky, especially for languages that support
overloading of subprograms.

The best tool to resolve these reference is a compiler, since by definition
it understands the program and where each part comes from. Some compilers
like GNAT and gcc can then generate extra output files which contain this
information. For instance, GNAT will generate :file:`.ali` files when it
compiles Ada source code, and gcc will generate :file:`.gli` files when
it compiles C or C++ code and is run with *-fdump-xref* switch.

:file:`GNATCOLL.Xref` can then be used to parse and aggregate all those
files into a single sqlite database, which can be queries conveniently to
answer queries such as "give me the declaration for this entity", "list all
places where this entity is used", "Show all subprograms that could be called
in practice at this dispatching call", "What files does this file depend on",
"Show me the call graph for this application",...

To us this package, some initialization needs to be performed first::

    with GNATCOLL.Xref;     use GNATCOLL.Xref;
    with GNATCOLL.SQL.Sqlite;
    with GNATCOLL.Projects; use GNATCOLL.Projects;   --  1

    procedure Support is
       DB : Xref_Database;
       Tree : Project_Tree;

    begin
       Tree.Load ("prj.gpr");  --  2
       Setup_DB (DB, GNATCOLL.SQL.Sqlite.Setup (Database => "testdb.db"));  -- 3
       Parse_All_LI_Files (DB, Tree, Tree.Root_Project);   --  4
    end Support;
    
GNATCOLL needs to be able to find the :file:`*li` files. For this, it depends
on project files (as supported by :file:`GNATCOLL.Projects`). So the first
thing to do is to parse the project (step 2).

We then need to tell GNATCOLL where the cross-reference information need to be
aggregated. In this example, it will be stored in a sqlite database on the disk.
By using a name ":memory:" instead, we would create a temporary in-memory
database. This is in general faster, but uses more memory and needs to be
recreated every time the program is restarted. We could also decide to store
the information in any other database supported by :file:`GNATCOLL.SQL.Exec`,
for instance PostgreSQL.

Finally, on step 4 we let GNATCOLL parse all the :file:`*li` files that are
relevant for this project. This operation can take a while, depending on the
size of the project. However, if the database already exists on the disk, it
will simply be updated by parsing the files that are not already up-to-date.
When all files are up-to-date, this operation is almost immediate.

At this point, we now have a database that we can start querying. Here are
a few examples, but see the documentation :file:`gnatcoll-xref.ads` for more
types of queries. All these queries have a similar API: they return a
**cursor** which iterates over the result returned by a SQL query. There are
various kinds of cursors, depending on whether they return files, entities,
or references to entities. But they all support the `Has_Element`, `Element`
and `Next` operation, so all loops will look similar::

    pragma Ada_05;   --  use object-dotted-notation
    with GNATCOLL.VFS;   use GNATCOLL.VFS;

    declare
        Entity : Entity_Information;
        Ref    : Entity_Reference;
        File   : Virtual_File;
        Refs   : References_Cursor;
    begin
        File := Tree.Create ("source.ads");   --  5
        Ref := DB.Get_Entity ("Method", File, Line => 2);   --  6
        Entity := Ref.Entity;

        DB.References (Entity, Refs);   --  7
        while Refs.Has_Element loop
           Ref := Refs.Element;
           Put_Line (" at " & Ref.File.Display_Full_Name & ':'
              & Ref.Line'Img & ':' & Ref.Column'Img);
           Refs.Next;
        end loop;
    end;

This example will print all the references to the entity that is referenced
in file source.ads at line 2 (the column is unspecified).

Step 5 gets a handle on the source file. Here, we depend on the project to
find the precise directory in which the source file is found. We can of course
use an absolute file name instead.

Step 6 gets handle on the entity referenced on line 2 in this file. Such an
entity is the starting point for most queries defined in `GNATCOLL.Xref`.

Finally, on step 7 and the loop below we iterate over all references, and
print their location on the standard output.

Let's do a much more complex query: we want to see all references to that
entity, but also places where the entity might be called through a `renames`
statement, or called through a dispatching call via an overriding method
defined on a child tagged type (assuming this is a primitive operation of
a tagged type in the first place). We also want to see all locations
where a method that overrides "Method" is called::

     declare
        Refs : Recursive_References_Cursor;
     begin
        DB.Recursive (Entity, GNATCOLL.Xref.References'Access,
                      From_Overriding => True, From_Overridden => True,
                      From_Renames => True);
        while Refs.Has_Element loop
             ... same as before
             Refs.Next;
        end loop;
     end;

As shown above, the programing pattern is always the same.

GNATCOLL.Xref provides many more subprogram to get information like the list
of fields for a record type (or a C structure), the list of primitive operations
or methods for a tagged object or a class, the call graph for a subprogram,...

It is also able to extract documentation for an entity from the source code, by
looking at the lines of code just before or just after the declaration or the
body of the entity.

*********************
**Xref**: gnatinspect
*********************

As discussed in the previous section, GNATCOLL provides an Ada API to
perform cross-references queries.

There exist a few alternatives when you want to reuse that cross-reference
information from other tools, or command line scripts.

You can of course access the sqlite database directly. Most programming
languages have an interface to sqlite. For instance python does.

But GNATCOLL provides a command line tool dedicated to that purpose, named
**gnatinspect**.

When it is first started on a project, this tool will refresh the xref
database by parsing all the ALI files from the project. This might take
a while (up to several minutes) the first time, unless of course the
xref were already up-to-date because you had loaded the project in GPS
first, or already run gnatinspect.

gnatinspect then displays an interactive prompt that lets you perform
various queries on the database. The full list of queries is available
by typing "help" at the prompt, but this documentation will demonstrate
some of them.

Let's first look at a number of command line switches that might be
useful:

* *--db=ARG*: this switch can be used to specify the name of the database.
  By default, this will be a file named 'gnatinspect.db' in the project's
  object directory. If there is no such directory defined in the project,
  the file is created in the project's directory itself.
  You can however specify any name, including an absolute path, or a path
  relative to the project's object directory.

  An alternative is to specify ':memory:', which creates the database in
  memory. This is of course a temporary database which will disappear when
  gnatinspect exists, and cannot be shared with other tools.

* *--nightlydb=ARG*: this switch can help speed up the initial startup
  of gnatinspect. The idea is that in a lot of cases, the software on which
  a team works is build nightly in a common setup. Running gnatinspect in
  that setup will create or update an xref database.
  Individual developers can then create their own copy of the database by
  starting from the contents of the nightly database (which is pointed to
  by the --nightlydb switch), and then gnatinspect will parse the ALI files
  in the user's setup that are different from the nightly ones.

* *--runtime*: by default, gnatinspect will only parse the ALI files from
  your project (and of course the ones from imported projects). It will not
  however parse the ALI files found in predefined directories, like for
  instance the GNAT runtime. This saves time in general. If you click on
  a call to one of the runtime subprograms in your own code, gnatinspect
  will be able to point you to its declaration. However, you will not have
  access to the body, because the link from declaration to body is found in
  the ALI files of the runtime.

* *--command=ARG*: gnatinspect will update the xref database as usual, then
  execute a command, display its result, and exit. This can be convenient
  when calling gnatinspect from another tool, like Emacs or vi.

* *--file=ARG*: similar to --command, but reads the commands to execute from
  a file. The file can contain comments (starting with '--'. See also the
  --lead switch.

* *--lead=ARG* should be used in coordination with --file, and specify lines
  to ignore from the file. All lines starting with the given prefix will be
  ignored.

* *--basenames*: controls the display of file names in the output. By default,
  gnatinspect outputs full path information.

* *--exit*: if this switch is specified, gnatinspect updates the xref database
  and exits immediately.

* *--project=ARG* or *-P ARG* specifes the name of the project to load. This
  switch is mandatory.

* *-X VAR=VALUE* is used to specify the value of scenario variables used in
  your project. This is similar to the homonym switch in gprbuild.

* *--symlinks* should be specified if your projet uses symbolic links for
  files. This will ensure that the links are fully resolved as stored in the
  database, and thus that when a file is visible through different links, the
  information is appropriately coalesced in the database for that file.

* *--subdirs=ARG* is similar to the homonym switch in gprbuild

* *--tracefile=ARG* is used to point to a file compatible with GNATCOLL.Traces
  that controls the debug information generated by gnatinspect. By default,
  gnatinspect parses a file called '.gnatdebug' in the current directory.

* *--encoding=ARG* is the character encoding used for source and ALI files.
  By default, gnatinspect assumes they are encoded in UTF-8.


Once it has finished parsing the xref information, gnatinspect displays an
interactive prompt, where a number of commands can be used to perform
queries. In a lot of cases, these commands take some file information as
argument (either just the file, or an entity name and the file in which it
is defined).

.. index:: projects; aggregate projects

The file names can be given as either a base name, or relative to the current
directory, or even a full name.  But file names are ambiguous (even when a full
path is specified) when aggregate projects are used. It is valid for a given
file to be part of multiple aggregate projects, and depending on the project we
are considering the result of the xref queries might vary).

To remove the ambiguity, it is possible to specify the project to which the
file belongs. The project is specified either as a project name (which itself
could be ambiguous with aggregate projects), or as a full path.

In all commands below, whenever the parameter specifies ":file", you can
use instead ":file:project" if there are ambiguities.

Here is the full list of commands supported by gnatinspect:

* *decl name:file:line:column* is probably the most useful command. Given a
  reference to an entity, it will indicate where the entity is declared. The
  line and column informations are optional::

       >>> decl Func:file.adb:12
       Func:/some/path/file2.adb:20:9

* *body name:file:line:column* is similar to *decl*, but will return the
  location of the body of the entity. When the entity is an Ada private type,
  its body is in fact the location of the full declaration for that type.

* *refs name:file:line:column* displays all known references to the entity.

* *refs_overriding name:file:line:column* displays all known references to the
  entity or one of its overriding entities

* *doc name:file:line:column* will display documentation for the entity. The
  exact format for the entity might change in future versions of gnatinspect,
  but will in general include the type of entity, the location of its
  declaration, and any comment associated with it in the source code::

      >>> doc Func:file.adb
      procedure declared at /some/path/file2.adb:20:9

      And the comments written below Func in file2.adb

* *fields name:file:line:column* displays the fields of an Ada record type
  or a C struct::

      >>> fields Rec:file.ads:20
      A:/some/path/file.ads:21
      B:/some/path/file.ads:22

* *child_types name:file:line:column* lists all child types for this entity,
  for instance classes that inherit from the entity. This is the opposite of
  *parent_types*.

* *child_types_recursive name:file:line:column* is similar to *child_types*
  but will also list the child types of the children. This query can be used
  to find a whole tagged type hierarchy (or class hierarchy in C++).

* *parent_types name:file:lin:column* returns the parent types for the entity,
  for instance the classes or interfaces from which it derives. See also
  *child_types*.

* *methods name:file:line:column* returns the list of methods (or primitive
  operations) for the entity.

* *method_of name:file:line:column* returns the class or tagged type for
  which the entity is a method.

* *calls name:file:line:column* lists all entities called by the entity. This
  includes all entities defined within the scope of the entity (so for a
  subprogram this will be the list of local variables, but for a package
  this includes all subprograms and nested packages defined within that
  package).

* *callers name:file:line:column* lists all entities that call the entity. This
  information is also available from a call to 'refs', but 'callers' return the
  callers directly, instead of references to the original entity.

* *overrides name:file:line:column* returns the entity that is overridden by
  the entity (generally a method from a parent class).

* *overridden name:file:line:column* returns the list of entities that override
  the parameter (generally methods from children classes).

* *overridden_recursive name:file:line:column* returns the list of entities
  that override the parameter (generally methods from children classes). This is
  recursive.

* *type name:file:line:column* returns the type of the entity (variable or
  constant). For an enumeration literal, this returns the corresponding
  enumeration.

* *component name:file:line:column* returns the component type of the entity
  (for arrays for instance).

* *literals name:file:line:column* returns the valid literal values for an
  enumeration.

* *pointed name:file:line:column* returns the type pointed to by the entity.

* *qname name:file:line:column* returns the fully qualified name for the
  entity.

* *params name:file:line:column* returns the list of parameters for the
  subprogram.

A number of queries are related to the source files of the project:

* *importing filename* lists the files that import the file (via with
  statements in Ada or #include in C for instance)

* *imports filename* lists the files that the file imports (via with statements
  in Ada or #include in C for instance). See also *depends_on*.

* *depends filename* lists the files that the file depends on (recursively
  calling *imports*)
  
* *entities file* lists all entities referenced or declared in the file.


Finally, some commands are not related to entities or source files:

* *refresh* refreshes the contents of the xref database, by parsing all ALI
  files that have been changed.

* *shell* Execute a shell command (an alternative is to use '!' as the
  command).

* *scenario VARIABLE VALUE* changes the value of a scenario variable, and
  reparse the project.

* *time command arguments* executes the command as usual, and report the time
  it took to execute it.

