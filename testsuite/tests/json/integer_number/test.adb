with GNATCOLL.JSON;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package JSON renames GNATCOLL.JSON;

   use type JSON.JSON_Value_Type;

   Content : constant String := "14";
begin
   declare
      Value   : constant JSON.JSON_Value := JSON.Read
         (Strm     => Content,
          Filename => "<data>");
      Result  : Integer;
   begin
      A.Assert (True, "passed");
      A.Assert (JSON.Kind (Value) = JSON.JSON_Int_Type,
                "check if type is JSON_Int_Type (got " &
                JSON.Kind (Value)'Img & ")");
      Result := JSON.Get (Value);
      A.Assert (Result = 14, "check that result is equal to 14");
   end;
   return A.Report;
end Test;
