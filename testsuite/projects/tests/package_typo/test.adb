with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPR.Opt;
with Test_Assert;

function Test return Integer is
   Env     : Project_Environment_Access;
   Tree    : Project_Tree;
   Err_Idx : Positive := 1;

   procedure Report_Error (Message : String);

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Message : String) is
   begin
      case Err_Idx is
         when 1 =>
            Test_Assert.Assert
              (Message,
               "common.gpr:3:12: warning: ""link"" is not a known package"
               & " name" & ASCII.LF);

         when 2 =>
            Test_Assert.Assert
              (Message,
               "common.gpr:3:12: warning: possible misspelling of ""linker"
               & '"' & ASCII.LF);

         when others =>
            Test_Assert.Assert (False, Message);
      end case;

      Err_Idx := Err_Idx + 1;
   end Report_Error;

begin
   Initialize (Env);
   GPR.Opt.Quiet_Output := False; -- To see warnings
   Tree.Load
     (Create ("typo.gpr"), Env, Errors => Report_Error'Unrestricted_Access);

   Test_Assert.Assert (Err_Idx = 3);

   Free (Env);

   return Test_Assert.Report;
end Test;
