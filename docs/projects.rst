.. _Projects:

************************************
**Projects**: manipulating gpr files
************************************

.. highlight:: ada

The package `GNATCOLL.Projects` provides an extensive interface to
parse, manipulate and edit project files (:file:`.gpr` files).

Although the interface is best used using the Ada05 notation, it is fully
compatible with Ada95.

Here is a quick example on how to use the interface, although the spec
file itself contains much more detailed information on all the subprograms
related to the manipulation of project files::

  pragma Ada_05;
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

Defining a project with user-defined packages and reading them.
=================================================================

.. highlight:: ada

If you want to use `GNATCOLL.Projects` with a GPR file that contains specific
packages and attributes, you must procede in several steps. The following
example will show you how to do it::

  pragma Ada_05;
  with GNATCOLL.Projects; use GNATCOLL.Projects;
  with GNATCOLL.VFS;      use GNATCOLL.VFS;

  procedure Test_Project is
     Tree  : Project_Tree;
     Virtual_File : VFS; --  We assume it points to a valid file.
  begin
     --  1
     Register_New_Attribute ("String", "Package_Name");
     Register_New_Attribute ("List", "Package_Name", Is_List => True);
     Register_New_Attribute ("Index", "Package_Name", Is_Index => True);

     --  2
     Tree.Load (Root_Project_Path => VFS,
                Packages_To_Check => All_Packs);

      declare --  3
        String_Attribute := constant Attribute_Pkg_String :=
                Build ("string", "package_name")

        Index_Attribute := constant Attribute_Pkg_List :=
                Build ("index", "package_name")

        List_Attribute := constant Attribute_Pkg_List :=
                Build ("list", "package_name")
     begin --  4
        for Val in Tree.Root_Project.Attribute_Value (List_Attribute).all loop
                Put_Line ("Value:" & Val.all);
        end loop;

        declare
           Indexed_Value : constant String := Tree.Root_Project.
              Attribute_Value (Index_Attribute, Index => "Index").all
        begin
           Put_Line ("Indexed_Value:" & Indexed_Value);
        end;
     end;
  end Test_Project;

Step 1: We register all the attributes that we want for a given package.
        If the package does not already exists it is created.

Step 2: We load the project into the projects hierarchy. We tell Tree.Load to
        check all packages otherwise it will not load any packages.

Step 3: We read the Attributes from the project. An attribute can be an
        Attribute_Pkg_String (representing a plain string) or an
        Attribute_Pkg_List (representing a list or an index).

Step 4: We can do something with those values. Here we print the plain string
        and the content of the list, aswell as an indexed value from the index.

