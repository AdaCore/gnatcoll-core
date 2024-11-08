with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Test_Assert;

function Test return Integer is
   Env  : Project_Environment_Access;
   Tree : Project_Tree;
   LIL  : Library_Info_List;
   LI   : Library_Info;
begin
   Initialize (Env);
   Tree.Load (Env => Env, Root_Project_Path => Create (+"p.gpr"));

   Tree.Root_Project.Library_Files (List => LIL, ALI_Ext => "^.*\.gli$");
   Test_Assert.Assert
     (Integer (LIL.Length), 1, "unexpected number of library files");

   LI := LIL.First_Element;
   Test_Assert.Assert
     (+LI.Library_File.Base_Name,
      "foo.cpp.gli",
      "unexpected LI file name");
   Test_Assert.Assert
     (+LI.Source.File.Base_Name,
      "foo.cpp",
      "unexpected source file associated to foo.cpp.gli");

   LIL.Clear;
   Tree.Unload;
   Free (Env);
   return Test_Assert.Report;
end Test;
