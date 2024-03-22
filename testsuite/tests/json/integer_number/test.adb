with GNATCOLL.JSON;
with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package JSON renames GNATCOLL.JSON;

   use type JSON.JSON_Value_Type;

begin
   declare
      Value   : constant JSON.JSON_Value := JSON.Read
         (Strm     => "14",
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

   declare
      Value   : constant JSON.JSON_Value := JSON.Read
         (Strm     => "9000000000000000000",
          Filename => "<data>");
      Result  : Long_Long_Integer;
   begin
      A.Assert (True, "passed");
      A.Assert (JSON.Kind (Value) = JSON.JSON_Int_Type,
                "check if type is JSON_Int_Type (got " &
                JSON.Kind (Value)'Img & ")");
      Result := JSON.Get (Value);
      A.Assert (Result = 9000000000000000000,
                "check that result is equal to 9000000000000000000");
   end;

   return A.Report;
end Test;
