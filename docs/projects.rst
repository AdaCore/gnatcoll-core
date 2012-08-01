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

