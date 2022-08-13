with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects.Aux;

with Test_Assert;

function Test return Integer is
   PT     : Project_Tree;
   Env    : Project_Environment_Access;
begin
   Initialize (Env);
   Env.Set_Automatic_Config_File;
   PT.Load (Create ("simple.gpr"), Env);

   declare
      Predef_Sources : constant File_Array := Env.Predefined_Source_Files;
   begin
      Test_Assert.Assert
        (Predef_Sources'Length > 0,
         "check if runtime is found");
   end;

   GNATCOLL.Projects.Aux.Delete_All_Temp_Files (PT.Root_Project);

   PT.Unload;
   Free (Env);
   return Test_Assert.Report;

end Test;
