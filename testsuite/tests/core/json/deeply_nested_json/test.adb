with GNATCOLL.JSON; use GNATCOLL.JSON;
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function Read_File_Into_JSON (Fn : String) return JSON_Value;

   function Read_File_Into_JSON (Fn : String) return JSON_Value
   is
      Result : Read_Result;
   begin
      Result := Read_File (Fn);

      if not Result.Success then
         --  ??? We should close the file here, but the subprogram is likely
         --  to terminate anyway, so this is not crucial.
         raise Invalid_JSON_Stream with Format_Parsing_Error (Result.Error);
      end if;
      return Result.Value;
   end Read_File_Into_JSON;

   File : JSON_Value;
begin
   File := Read_File_Into_JSON ("bad.json");
   A.Assert (File /= JSON_Null);
   return A.Report;
end Test;
