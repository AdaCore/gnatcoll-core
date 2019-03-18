with GNATCOLL.Scripts;        use GNATCOLL.Scripts;

package body Support is

   procedure On_Hello   (Data : in out Callback_Data'Class; Command : String);
   procedure On_Print_Float
      (Data : in out Callback_Data'Class; Command : String);
   procedure C1_Handler (Data : in out Callback_Data'Class; Command : String);
   procedure Custom_List_Handler
      (Data : in out Callback_Data'Class; Command : String);

   Prop    : Integer := 0;
   Ro_Prop : constant Integer := 0;
   pragma Warnings (Off);
   Wo_Prop : Integer := 0;
   pragma Warnings (On);
   --  Imagine these are fields of a class. We simplify the test case by
   --  using global variables

   --------------------
   -- On_Print_Float --
   --------------------

   procedure On_Print_Float
      (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Param : constant Float := Nth_Arg (Data, 1);
   begin
      Set_Return_Value (Data, Param + 1.0);
   end On_Print_Float;

   --------------
   -- On_Hello --
   --------------

   procedure On_Hello (Data : in out Callback_Data'Class; Command : String) is
      pragma Unreferenced (Command);
   begin
      Set_Return_Value (Data, "Hello " & Nth_Arg (Data, 1, "world") & " !");
   end On_Hello;

   -------------------------
   -- Custom_List_Handler --
   -------------------------

   procedure Custom_List_Handler
      (Data : in out Callback_Data'Class; Command : String)
   is
   begin
      Set_Return_Value (Data, "Executing command '" & Command & "'");
   end Custom_List_Handler;

   ----------------
   -- C1_Handler --
   ----------------

   procedure C1_Handler
      (Data : in out Callback_Data'Class; Command : String)
   is
      --  This could also be kept as a variable somewhere, but fetching it
      --  is relatively cheap
      C1 : constant Class_Type := New_Class (Get_Repository (Data), "C1");

      Inst : Class_Instance;
   begin
      if Command = Constructor_Method then
         Inst := Nth_Arg (Data, 1, C1);
         Set_Data (Inst, C1, Integer'(Nth_Arg (Data, 2)));
         Set_Property (Inst, "id", 2);
         Set_Property (Inst, "name", "the_name");

      elsif Command = "method" then
         Inst := Nth_Arg (Data, 1, C1);
         Set_Return_Value
            (Data, "Method applied to class"
             & Integer'Image (Get_Data (Inst, C1)) & " with param "
             & Nth_Arg (Data, 2));

      elsif Command = "prop" then
         if Number_Of_Arguments (Data) = 1 then
            Set_Return_Value (Data, Prop);
         else
            Prop := Nth_Arg (Data, 2) + 1; --  Offset to make sure we were here
         end if;

      elsif Command = "ro_prop" then
         Set_Return_Value (Data, Ro_Prop);

      elsif Command = "wo_prop" then
         Wo_Prop := Nth_Arg (Data, 2);
      end if;
   end C1_Handler;

   ------------------------
   -- Register_Functions --
   ------------------------

   procedure Register_Functions (Repo : Scripts_Repository) is

      C1 : Class_Type;

      Custom_List : constant Class_Type := New_Class
         (Repo, "MyList", Lookup_Class (Repo, "list"));
   begin
      Register_Command
        (Repo, "hello", 0, 1,
         Handler => On_Hello'Access);
      Register_Command
        (Repo, "print_float", 1, 1,
         Handler => On_Print_Float'Access);

      C1 := New_Class (Repo, "C1");
      Register_Command
        (Repo, Constructor_Method, 1, 1,
         Class   => C1,
         Handler => C1_Handler'Access);
      Register_Command
        (Repo, "method", 0, 1,
         Class   => C1,
         Handler => C1_Handler'Access);

      Register_Property
        (Repo, "prop", Class => C1,
         Setter => C1_Handler'Access,
         Getter => C1_Handler'Access);
      Register_Property
        (Repo, "ro_prop", Class => C1,
         Getter => C1_Handler'Access);
      Register_Property
        (Repo, "wo_prop", Class => C1,
         Setter => C1_Handler'Access);

      Register_Command
        (Repo, "dump", Class => Custom_List,
         Handler => Custom_List_Handler'Access);
   end Register_Functions;

end Support;
