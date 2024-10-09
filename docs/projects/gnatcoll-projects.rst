.. _Projects:

************************************
**Projects**: manipulating gpr files
************************************

.. highlight:: ada

The package `GNATCOLL.Projects` provides an extensive interface to parse,
manipulate and edit project files (:file:`.gpr` files).

Although the interface is best used using the Ada 2012 notation, it is fully
compatible with Ada 95.

Here is a quick example on how to use the interface, although the spec file
itself contains much more detailed information on all the subprograms related
to the manipulation of project files:

.. code-block:: ada

   with GNATCOLL.Projects; use GNATCOLL.Projects;
   with GNATCOLL.VFS;      use GNATCOLL.VFS;

   procedure Test_Project is
      Tree  : Project_Tree;
      Files : File_Array_Access;
   begin
      Tree.Load (GNATCOLL.VFS.Create (+"path_to_project.gpr"));

      --  List the source files for project and all imported projects

      Files := Tree.Root_Project.Source_Files (Recursive => True);
      for F in Files'Range loop
         Put_Line ("File is: " & Files (F).Display_Full_Name);
      end loop;

      Tree.Unload;
   end Test_Project;

Defining a project with user-defined packages and reading them
==============================================================

.. highlight:: ada

If you want to use `GNATCOLL.Projects` with a GPR file that contains specific
packages and attributes, you must procede in several steps. The following
example will show you how to do it:

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   with GNAT.Strings;

   with GNATCOLL.Projects; use GNATCOLL.Projects;
   with GNATCOLL.VFS;      use GNATCOLL.VFS;

   procedure Test_Project is
      Tree         : Project_Tree;
      Project_File : constant Virtual_File :=
         GNATCOLL.VFS.Create (+"path_to_project.gpr");
   begin
      --  1
      if Register_New_Attribute
           ("String", "Package_Name") /= ""
         or else Register_New_Attribute
           ("List", "Package_Name", Is_List => True) /= ""
         or else Register_New_Attribute
           ("Indexed", "Package_Name", Indexed => True) /= ""
      then
         raise Program_Error;
      end if;

      --  2
      Tree.Load (Root_Project_Path => Project_File,
                 Packages_To_Check => All_Packs);

      declare
         Root : constant Project_Type := Tree.Root_Project;

         --  3
         String_Attribute : constant Attribute_Pkg_String :=
            Build ("Package_Name", "String");
         String_Value : constant String :=
            Root.Attribute_Value (String_Attribute);

         Indexed_Attribute : constant Attribute_Pkg_String :=
            Build ("Package_Name", "Indexed");
         Indexed_Value : constant String :=
            Root.Attribute_Value
              (Indexed_Attribute, Index => "Index");

         List_Attribute : constant Attribute_Pkg_List :=
            Build ("Package_Name", "List");
         List_Value : constant GNAT.Strings.String_List_Access :=
            Root.Attribute_Value (List_Attribute);
      begin
         --  4
         Put_Line ("Package_Name.String: " & String_Value);
         Put_Line ("Package_Name.Indexed (""Index""): "
                   & Indexed_Value);
         Put_Line ("Package_Name.List:");
         for Val of List_Value.all loop
            Put_Line ("  Value: " & Val.all);
         end loop;
      end;
   end Test_Project;

And the corresponding project file:

.. code-block:: ada

   project Path_To_Project is
      package Package_Name is
         for String use "some string"
         for Indexed ("Index") use "other string";
         for List use ("first item", "second item");
      end Package_Name;
   end Path_To_Project;

Step 1: We register all the attributes that we want for a given package.
        If the package does not already exists it is created.

Step 2: We load the project into the projects hierarchy. We tell ``Tree.Load``
        to check all packages otherwise it will not load any packages.

Step 3: We read attributes from the project. An attribute can be an
        ``Attribute_Pkg_String`` (representing a plain string) or an
        ``Attribute_Pkg_List`` (representing a list of strings).

Step 4: We can do something with those values. Here we print the plain string
        and the content of the list.

This program should output:

.. code-block:: text

   Package_Name.String: some string
   Package_Name.Indexed ("Index"): hello world
   Package_Name.List:
     Value: hello
     Value: world
