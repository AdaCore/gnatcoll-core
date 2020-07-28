with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON;    use GNATCOLL.JSON;
with GNATCOLL.Strings; use GNATCOLL.Strings;

with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;

   function Less (Left, Right : JSON_Value) return Boolean;
   --  Arbitrary and incomplete comparison function to test sorting facilities

   procedure Check_Image (Val : JSON_Value; Expected_Image : String);
   --  Assert that the compact image for Val is Expected_Image

   procedure Check_Error (Result : Read_Result; Expected_Error : String);
   --  Assert that Result comes from an unsuccessful read and compare its error
   --  information formatted by Format_Parsing_Error to Expected_Error.

   -----------------
   -- Check_Image --
   -----------------

   procedure Check_Image (Val : JSON_Value; Expected_Image : String) is
      Img : constant String := Val.Write;
   begin
      A.Assert (Img = Expected_Image, "Check image: " & Img);
   end Check_Image;

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error (Result : Read_Result; Expected_Error : String) is
      Label : constant String := "Checking error: " & Expected_Error;
   begin
      A.Assert (not Result.Success, Label & " (parsing error)");

      declare
         Error : constant String := Format_Parsing_Error (Result.Error);
      begin
         A.Assert (Error = Expected_Error, Label & " (error message)");
      end;
   end Check_Error;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : JSON_Value) return Boolean is
   begin
      if Left.Kind /= Right.Kind then
         return Left.Kind < Right.Kind;
      end if;

      case Left.Kind is
         when JSON_Null_Type =>
            return False;
         when JSON_Boolean_Type =>
            return Boolean'(Left.Get) < Right.Get;
         when JSON_Int_Type =>
            return Integer'(Left.Get) < Right.Get;
         when JSON_String_Type =>
            return String'(Left.Get) < String'(Right.Get);
         when others =>
            null;
      end case;

      return (raise Program_Error);
   end Less;

   Int_0       : JSON_Value := Create (Integer'(0));
   Int_1       : constant JSON_Value := Create (Integer'(1));
   Hello_World : constant JSON_Value := Create ("Hello world!");

   Float_0 : constant JSON_Value := Create (Float'(0.0));
   Float_1 : constant JSON_Value := Create (Float'(1.0));

   Float_0_Image : constant String := Float_0.Write;
   Float_1_Image : constant String := Float_1.Write;

   Dummy_Int : Integer;
begin

   --------------------
   -- Array handling --
   --------------------

   declare
      Arr   : JSON_Array := Int_0 & Int_1 & Hello_World;
      Empty : constant JSON_Array := Empty_Array;
   begin
      --  Inspect an empty array

      A.Assert (Is_Empty (Empty_Array));
      A.Assert (Length (Empty_Array) = 0);
      begin
         Dummy_Int := Get (Get (Empty_Array, 1));
         A.Assert (False, "Out-of-bound JSON_Array.Get");
      exception
         when Constraint_Error => null;
      end;

      --  Inspect a 3-elements array

      A.Assert (not Is_Empty (Arr));
      A.Assert (Length (Arr) = 3);
      A.Assert (Get (Arr, 1) = Int_0);
      A.Assert (Get (Arr, 2) = Int_1);
      A.Assert (Get (Arr, 3) = Hello_World);
      begin
         Dummy_Int := Get (Get (Empty_Array, 4));
         A.Assert (False, "Out-of-bound JSON_Array.Get");
      exception
         when Constraint_Error => null;
      end;

      --  Array modifications

      Append (Arr, Int_0);
      A.Assert (Length (Arr) = 4);
      A.Assert (Get (Arr, 4) = Int_0);

      Prepend (Arr, Hello_World);
      A.Assert (Length (Arr) = 5);
      A.Assert (Get (Arr, 1) = Hello_World);

      Set_Element (Arr, 1, Int_1);
      A.Assert (Length (Arr) = 5);
      A.Assert (Get (Arr, 1) = Int_1);

      Clear (Arr);
      A.Assert (Length (Arr) = 0);

      Arr := Hello_World & Int_0 & Int_1 & Int_0;
      Sort (Arr, Less'Access);
      Check_Image (Create (Arr), "[0,0,1,""Hello world!""]");

      --  Array iteration
      declare
         I : Positive := 1;
      begin
         for V of Arr loop
            case I is
            when 1      => A.Assert (V = Int_0);
            when 2      => A.Assert (V = Int_0);
            when 3      => A.Assert (V = Int_1);
            when 4      => A.Assert (V = Hello_World);
            when others => A.Assert (False);
            end case;
            I := I + 1;
         end loop;

         for V of Empty loop
            A.Assert (False);
         end loop;
      end;
   end;

   -------------------------
   -- JSON_Value.Is_Empty --
   -------------------------

   A.Assert (JSON_Null.Is_Empty);
   A.Assert (Create_Object.Is_Empty);
   A.Assert (Create (Empty_Array).Is_Empty);
   A.Assert (not Int_0.Is_Empty);
   A.Assert (not Hello_World.Is_Empty);

   ----------------------------------------------
   -- Serialization/deserialization primitives --
   ----------------------------------------------

   --  Exception-based serialization/deserialization primitives get their own
   --  testing outside of this testcase.

   Check_Image (Read ("{}").Value, "{}");
   Check_Image (Read (To_Unbounded_String ("{}")).Value, "{}");

   Check_Error (Read ("{"), "1:2: empty stream");
   Check_Error (Read (To_Unbounded_String ("{")), "1:2: empty stream");

   -----------------------------
   -- Creation of JSON values --
   -----------------------------

   Check_Image (Create, "null");

   Check_Image (Create (True), "true");

   Check_Image (Create (Integer'(0)), "0");
   Check_Image (Create (Long_Integer'(1_000_000_000)), "1000000000");
   Check_Image (Create (Long_Long_Integer'(10_000_000_000)), "10000000000");

   Check_Image (Create (Float'(1.0)), Float_1_Image);
   Check_Image (Create (Long_Float'(1.0)), Float_1_Image);

   Check_Image (Create ("Hello world!"), """Hello world!""");
   Check_Image (Create (To_Unbounded_String ("Hello world!")),
                """Hello world!""");
   Check_Image (Create (To_XString ("Hello world!")), """Hello world!""");

   Check_Image (Create (Empty_Array), "[]");

   Check_Image (Create_Object, "{}");

   -------------
   -- Sorting --
   -------------

   declare
      Obj : JSON_Value := Create_Object;
      Arr : JSON_Value := Create (Int_1 & Int_0);
   begin
      Obj.Set_Field ("bar", Int_1);
      Obj.Set_Field ("foo", Int_0);
      Check_Image (Obj, "{""bar"":1,""foo"":0}");
      Obj.Sort (Less'Access);
      Check_Image (Obj, "{""foo"":0,""bar"":1}");

      Check_Image (Arr, "[1,0]");
      Arr.Sort (Less'Access);
      Check_Image (Arr, "[0,1]");

      Int_0.Sort (Less'Access);
      Check_Image (Int_0, "0");
   end;

   -----------------------------------------
   -- General modification of JSON values --
   -----------------------------------------

   declare
      Arr    : constant JSON_Value := Create (Empty_Array);
      Cloned : JSON_Value;
   begin
      Check_Image (Arr, "[]");
      Arr.Append (Int_0);
      Check_Image (Arr, "[0]");

      Cloned := Arr.Clone;
      Cloned.Append (Int_1);
      Check_Image (Cloned, "[0,1]");
      Check_Image (Arr, "[0]");
   end;

   ------------------------------
   -- Equality for JSON values --
   ------------------------------

   A.Assert (not (Create (Integer'(0)) = Create ("0")));

   A.Assert (Create = Create);

   A.Assert (Int_0 = Create (Integer'(0)));
   A.Assert (Int_1 /= Create (Integer'(0)));

   A.Assert (Create (True) = Create (True));
   A.Assert (Create (True) /= Create (False));

   A.Assert (Create (Float'(1.0)) = Create (Float'(1.0)));
   A.Assert (Create (Float'(1.0)) /= Create (Float'(2.0)));

   A.Assert (Create ("Hello") = Create (To_XString ("Hello")));
   A.Assert (Create ("Hello") /= Create ("world"));

   declare
      Arr : constant JSON_Value := Create (Int_0 & Int_1);
   begin
      A.Assert (Arr = Arr);
      A.Assert (Arr = Create (Int_0 & Int_1));
      A.Assert (Arr /= Create (Int_0 & Int_1 & Int_0));
      A.Assert (Arr /= Create (Int_1 & Int_0));
   end;

   declare
      Obj_A  : constant JSON_Value := Create_Object;
      Obj_A0 : constant JSON_Value := Create_Object;
      Obj_A1 : constant JSON_Value := Create_Object;
      Obj_B  : constant JSON_Value := Create_Object;
      Obj_AB : constant JSON_Value := Create_Object;
      Obj_BA : constant JSON_Value := Create_Object;
   begin
      Obj_A.Set_Field ("A", Int_0);
      Obj_A0.Set_Field ("A", Int_0);
      Obj_A1.Set_Field ("A", Int_1);
      Obj_B.Set_Field ("B", Int_0);

      Obj_AB.Set_Field ("A", Int_0);
      Obj_AB.Set_Field ("B", Int_0);

      Obj_BA.Set_Field ("B", Int_0);
      Obj_BA.Set_Field ("A", Int_0);

      A.Assert (Obj_A = Obj_A);   --  Same pointer
      A.Assert (Obj_A = Obj_A0);  --  Same contents
      A.Assert (Obj_A /= Obj_A1); --  Different value
      A.Assert (Obj_A /= Obj_B);  --  Different field name
      A.Assert (Obj_A /= Obj_AB); --  Different size
      A.Assert (Obj_AB = Obj_BA); --  Different order
   end;

   ----------------------------------
   -- Modification of JSON objects --
   ----------------------------------

   declare
      Obj : JSON_Value := Create_Object;
   begin
      Check_Image (Obj, "{}");

      Obj.Set_Field ("foo", Int_0);
      Check_Image (Obj, "{""foo"":0}");

      Obj.Set_Field ("bar", Int_1);
      Check_Image (Obj, "{""foo"":0,""bar"":1}");

      Obj.Set_Field ("foo", Int_1);
      Check_Image (Obj, "{""foo"":1,""bar"":1}");

      Obj.Set_Field (To_XString ("foo"), Int_0);
      Check_Image (Obj, "{""foo"":0,""bar"":1}");

      Obj := Create_Object;
      Check_Image (Obj, "{}");

      Obj.Set_Field ("foo", True);
      Check_Image (Obj, "{""foo"":true}");

      Obj.Set_Field ("foo", Integer'(0));
      Check_Image (Obj, "{""foo"":0}");

      Obj.Set_Field ("foo", Long_Integer'(1));
      Check_Image (Obj, "{""foo"":1}");

      Obj.Set_Field ("foo", Float'(0.0));
      Check_Image (Obj, "{""foo"":" & Float_0_Image & "}");

      Obj.Set_Field_Long_Float ("foo", Long_Float'(1.0));
      Check_Image (Obj, "{""foo"":" & Float_1_Image & "}");

      Obj.Set_Field ("foo", "Hello, world!");
      Check_Image (Obj, "{""foo"":""Hello, world!""}");

      Obj.Set_Field ("foo", To_Unbounded_String ("Bye"));
      Check_Image (Obj, "{""foo"":""Bye""}");

      Obj.Set_Field ("foo", Empty_Array);
      Check_Image (Obj, "{""foo"":[]}");

      Obj := Create_Object;
      Obj.Set_Field_If_Not_Empty ("foo", "bar");
      Check_Image (Obj, "{""foo"":""bar""}");
      Obj.Set_Field_If_Not_Empty ("foo", "");
      Check_Image (Obj, "{""foo"":""bar""}");

      Obj := Create_Object;
      Obj.Set_Field_If_Not_Empty ("foo", To_Unbounded_String ("bar"));
      Check_Image (Obj, "{""foo"":""bar""}");
      Obj.Set_Field_If_Not_Empty ("foo", To_Unbounded_String (""));
      Check_Image (Obj, "{""foo"":""bar""}");

      Obj := Create_Object;
      Obj.Set_Field_If_Not_Empty ("foo", Int_0 & Int_1);
      Check_Image (Obj, "{""foo"":[0,1]}");
      Obj.Set_Field_If_Not_Empty ("foo", Empty_Array);
      Check_Image (Obj, "{""foo"":[0,1]}");

      Obj.Set_Field ("bar", Integer'(1));
      Check_Image (Obj, "{""foo"":[0,1],""bar"":1}");
      Obj.Unset_Field ("foo");
      Check_Image (Obj, "{""bar"":1}");
      Obj.Unset_Field ("foo");
      Check_Image (Obj, "{""bar"":1}");
      Obj.Unset_Field ("bar");
      Check_Image (Obj, "{}");
   end;

   -----------------
   -- Conversions --
   -----------------

   A.Assert (Create.Kind = JSON_Null_Type);
   A.Assert (Create (True).Get = True);
   A.Assert (Int_1.Get = Integer'(1));
   A.Assert (Int_1.Get = Long_Integer'(1));
   A.Assert (Int_1.Get = Long_Long_Integer'(1));

   A.Assert (Float_0.Get = Float'(0.0));

   A.Assert (UTF8_String'(Hello_World.Get) = "Hello world!");
   A.Assert (UTF8_Unbounded_String'(Hello_World.Get)
             = To_Unbounded_String ("Hello world!"));
   A.Assert (UTF8_XString'(Hello_World.Get) = To_XString ("Hello world!"));

   A.Assert (Create (Int_0 & Int_1).Get = Int_0 & Int_1);

   declare
      Obj : constant JSON_Value := Create_Object;
   begin
      Obj.Set_Field ("foo", Integer'(1));

      A.Assert (Obj.Has_Field ("foo"));
      A.Assert (not Obj.Has_Field ("bar"));

      Obj.Set_Field ("foo", True);
      A.Assert (Obj.Get ("foo") = True);

      Obj.Set_Field ("foo", Int_1);
      A.Assert (Obj.Get ("foo") = Integer'(1));
      A.Assert (Obj.Get ("foo") = Long_Integer'(1));

      Obj.Set_Field ("foo", Float'(1.0));
      A.Assert (Obj.Get ("foo") = Float'(1.0));
      A.Assert (Obj.Get_Long_Float ("foo") = Long_Float'(1.0));

      Obj.Set_Field ("foo", "Hello world!");
      A.Assert (UTF8_String'(Obj.Get ("foo")) = "Hello world!");
      A.Assert (UTF8_Unbounded_String'(Obj.Get ("foo"))
                = To_Unbounded_String ("Hello world!"));

      Obj.Set_Field ("foo", Empty_Array);
      A.Assert (Obj.Get ("foo") = Empty_Array);
   end;

   ---------------
   -- Iteration --
   ---------------

   declare
      Obj : constant JSON_Value := Create_Object;

      Expected_Trace_1 : constant String :=
         "foo: 0" & ASCII.LF & "bar: 1" & ASCII.LF;
      Expected_Trace_2 : constant String :=
         "foo" & ASCII.LF & "bar" & ASCII.LF;

      type Iteration_Data (With_Value : Boolean) is record
         Result : Unbounded_String;

         case With_Value is
            when False => null;
            when True  => Value_Separator : Character;
         end case;
      end record;

      Data_1 : Iteration_Data :=
        (With_Value => True, Result => <>, Value_Separator => ':');
      Data_2 : Iteration_Data := (With_Value => False, Result => <>);

      procedure Iterate
        (Data : in out Iteration_Data; Name : String; Value : JSON_Value);
      procedure Iterate (Name : String; Value : JSON_Value);

      -------------
      -- Iterate --
      -------------

      procedure Iterate
        (Data : in out Iteration_Data; Name : String; Value : JSON_Value) is
      begin
         Append (Data.Result, Name);
         if Data.With_Value then
            Append (Data.Result,
                    Data.Value_Separator & " " & String'(Value.Write));
         end if;
         Append (Data.Result, ASCII.LF);
      end Iterate;

      procedure Iterate (Name : String; Value : JSON_Value) is
      begin
         Iterate (Data_1, Name, Value);
      end Iterate;

      procedure Map_JSON_Object is new Gen_Map_JSON_Object (Iteration_Data);

   begin
      Obj.Set_Field ("foo", Int_0);
      Obj.Set_Field ("bar", Int_1);

      Data_1.Result := Null_Unbounded_String;
      Obj.Map_JSON_Object (Iterate'Access);
      A.Assert (To_String (Data_1.Result), Expected_Trace_1,
                "Checking trace: " & Expected_Trace_1);

      Data_1.Result := Null_Unbounded_String;
      Map_JSON_Object (Obj, Iterate'Access, Data_1);
      A.Assert (To_String (Data_1.Result), Expected_Trace_1,
                "Checking trace: " & Expected_Trace_1);

      Map_JSON_Object (Obj, Iterate'Access, Data_2);
      A.Assert (To_String (Data_2.Result), Expected_Trace_2,
                "Checking trace: " & Expected_Trace_2);
   end;

   return A.Report;
end Test;
