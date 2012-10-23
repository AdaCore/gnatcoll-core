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

