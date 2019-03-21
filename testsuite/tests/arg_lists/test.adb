with Ada.Strings.Unbounded;

with GNAT.Source_Info;
with GNAT.Strings;

with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;

with Test_Assert;

function Test return Integer is

   package A renames Test_Assert;
   package SI renames GNAT.Source_Info;

   type String_Array is
      array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;
   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String
      renames Ada.Strings.Unbounded.To_String;

   function Create_Args
     (Command  : String;
      Args     : String_Array;
      Cmd_Mode : Command_Line_Mode;
      Arg_Mode : Argument_Mode) return Arg_List;
   --  Synthetic constructor for Arg_List

   procedure Test_Parse_String
     (Debug_String, Command, Text : String;
      Location                    : String := SI.Source_Location);
   --  Run Parse_String on Command and Text and check that we get the given
   --  debug string.

   procedure Test_Parse_String
     (Debug_String, Text : String;
      Mode               : Command_Line_Mode;
      Location           : String := SI.Source_Location);
   --  Run Parse_String on Text and Mode and check that we get the given
   --  debug string.

   procedure Test_Argument_List_To_String
     (Expected       : String;
      List           : String_Array;
      Protect_Quotes : Boolean);
   --  Run Argument_List_To_String on List and Protect_Quotes and check that it
   --  returns Expected.

   procedure Test_To_List
     (Expected        : String_Array;
      C               : String_Array;
      Include_Command : Boolean;
      Location        : String := SI.Source_Location);
   --  Run To_List on C and Include_Command and check that it returns an
   --  Argument_List corresponding to Expected.

   function Substitution_Callback
     (Param : String; Mode : Command_Line_Mode) return Arg_List;
   --  Callback for our Substitution tests

   procedure Test_Substitute
     (Debug_String : String;
      Command      : String := "cmd";
      Args         : String_Array;
      Cmd_Mode     : Command_Line_Mode;
      Arg_Mode     : Argument_Mode;
      Location     : String := SI.Source_Location);
   --  Create a command-line with the given Cmd_Mode and Command as the
   --  command.  Append all arguments from Args with the given Arg_Mode. Then,
   --  run Substitute on this command-line with the '%' substitution character
   --  and Substitution_Callback.

   procedure Test_To_Display_String
     (Expected        : String;
      C               : Arg_List;
      Include_Command : Boolean;
      Max_Arg_Length  : Positive := Positive'Last);
   --  Check that To_Display_String (C, Include_Command, Max_Arg_Length)
   --  returns Expected.

   procedure Test_To_Script_String (Expected : String; C : Arg_List);
   --  Check that To_Script_String (C) returns Expected

   -----------------
   -- Create_Args --
   -----------------

   function Create_Args
     (Command  : String;
      Args     : String_Array;
      Cmd_Mode : Command_Line_Mode;
      Arg_Mode : Argument_Mode) return Arg_List
   is
      Result : Arg_List := Parse_String (Command, Cmd_Mode);
   begin
      for A of Args loop
         Append_Argument (Result, +A, Arg_Mode);
      end loop;
      return Result;
   end Create_Args;

   -----------------------
   -- Test_Parse_String --
   -----------------------

   procedure Test_Parse_String
     (Debug_String, Command, Text : String;
      Location                    : String := SI.Source_Location)
   is
      Args : constant Arg_List := Parse_String (Command, Text);
   begin
      A.Assert (To_Debug_String (Args), Debug_String, Location => Location);
   end Test_Parse_String;

   -----------------------
   -- Test_Parse_String --
   -----------------------

   procedure Test_Parse_String
     (Debug_String, Text : String;
      Mode               : Command_Line_Mode;
      Location           : String := SI.Source_Location)
   is
      Args : constant Arg_List := Parse_String (Text, Mode);
   begin
      A.Assert (To_Debug_String (Args), Debug_String, Location => Location);
   end Test_Parse_String;

   ----------------------------------
   -- Test_Argument_List_To_String --
   ----------------------------------

   procedure Test_Argument_List_To_String
     (Expected       : String;
      List           : String_Array;
      Protect_Quotes : Boolean)
   is
      use GNAT.Strings;

      SList : String_List_Access := new String_List (List'Range);
   begin
      for I in List'Range loop
         SList (I) := new String'(+List (I));
      end loop;

      A.Assert (Expected, Argument_List_To_String (SList.all, Protect_Quotes));

      GNAT.Strings.Free (SList);
   end Test_Argument_List_To_String;

   ------------------
   -- Test_To_List --
   ------------------

   procedure Test_To_List
     (Expected        : String_Array;
      C               : String_Array;
      Include_Command : Boolean;
      Location        : String := SI.Source_Location)
   is
      use GNAT.Strings;

      Args : Arg_List := Empty_Command_Line;
   begin
      for A of C loop
         Append_Argument (Args, +A, One_Arg);
      end loop;

      declare
         List : constant String_List := To_List (Args, Include_Command);
      begin
         A.Assert (List'Length = Expected'Length,
                   Msg      => "checking length",
                   Location => Location);
         for I in Expected'Range loop
            declare
               Expected_Arg : constant String := +Expected (I);
               Got_Arg      : String_Access :=
                  List (List'First + I - Expected'First);
            begin
               A.Assert (Expected_Arg, Got_Arg.all,
                         Msg      => "Checking argument no. " & I'Image,
                         Location => Location);
               Free (Got_Arg);
            end;
         end loop;
      end;
   end Test_To_List;

   ---------------------------
   -- Substitution_Callback --
   ---------------------------

   function Substitution_Callback
     (Param : String; Mode : Command_Line_Mode) return Arg_List
   is
      pragma Unreferenced (Mode);
      Result : Arg_List;
   begin
      Append_Argument (Result, "<<", One_Arg);
      Append_Argument (Result, Param, One_Arg);
      Append_Argument (Result, ">>", One_Arg);
      return Result;
   end Substitution_Callback;

   ---------------------
   -- Test_Substitute --
   ---------------------

   procedure Test_Substitute
     (Debug_String : String;
      Command      : String := "cmd";
      Args         : String_Array;
      Cmd_Mode     : Command_Line_Mode;
      Arg_Mode     : Argument_Mode;
      Location     : String := SI.Source_Location)
   is
      Cmd_Line : Arg_List := Create_Args (Command, Args, Cmd_Mode, Arg_Mode);
   begin
      Substitute (Cmd_Line, '%', Substitution_Callback'Access);
      A.Assert
        (To_Debug_String (Cmd_Line), Debug_String, Location => Location);
   end Test_Substitute;

   ----------------------------
   -- Test_To_Display_String --
   ----------------------------

   procedure Test_To_Display_String
     (Expected        : String;
      C               : Arg_List;
      Include_Command : Boolean;
      Max_Arg_Length  : Positive := Positive'Last)
   is
   begin
      A.Assert (Expected,
                To_Display_String (C, Include_Command, Max_Arg_Length));
   end Test_To_Display_String;

   ---------------------------
   -- Test_To_Script_String --
   ---------------------------

   procedure Test_To_Script_String (Expected : String; C : Arg_List) is
   begin
      A.Assert (Expected, To_Script_String (C));
   end Test_To_Script_String;

begin
   ------------------------
   -- Get_Command/Create --
   ------------------------

   A.Assert (Get_Command (Create ("foo")), "foo");
   A.Assert (Get_Command (Empty_Command_Line), "");

   -----------------------------
   -- Argument_List_To_String --
   -----------------------------

   Test_Argument_List_To_String
     (Expected       => "foo bar\ baz",
      List           => (+"foo", +"bar baz"),
      Protect_Quotes => True);

   Test_Argument_List_To_String
     (Expected       => "foo bar baz",
      List           => (+"foo", +"bar baz"),
      Protect_Quotes => False);

   ------------------
   -- Parse_String --
   ------------------

   Test_Parse_String
     (Debug_String => "Command: foo"
                      & ASCII.LF & "Arg: bar"
                      & ASCII.LF & "Arg: baz",
      Command      => "foo",
      Text         => "bar baz");

   Test_Parse_String
     (Debug_String => "Command: foo"
                      & ASCII.LF & "Arg: bar"
                      & ASCII.LF & "Arg: baz",
      Text         => "foo bar baz",
      Mode         => Separate_Args);

   Test_Parse_String
     (Debug_String => "Command: foo bar baz",
      Text         => "foo bar baz",
      Mode         => Raw_String);

   Test_Parse_String
     (Debug_String => "Command: ",
      Text         => "",
      Mode         => Raw_String);

   Test_Parse_String
     (Debug_String => "Command: foo"
                      & ASCII.LF & "Arg: a"
                      & ASCII.LF & "Arg: "
                      & ASCII.LF & "Arg: b c"
                      & ASCII.LF & "Arg: d",
      Command      => "foo",
      Text         => "a """" ""b c"" ""d""");

   -----------------------------------------------------
   -- Append_Argument/Args_Length/Nth_ARg/Set_Nth_Arg --
   -----------------------------------------------------

   declare
      Args : Arg_List := Create ("cmd");
   begin
      A.Assert (Args_Length (Args) = 0);
      A.Assert (Nth_Arg (Args, 0), "cmd");

      Append_Argument (Args, "a b", Expandable);
      A.Assert (Args_Length (Args) = 1);
      A.Assert (Nth_Arg (Args, 1), "a b");

      --  Out-of-ounds argument access must raise a Constraint_Error

      declare
         Exception_Raised : Boolean := False;
      begin
         begin
            declare
               Dummy : constant String := Nth_Arg (Args, 2);
            begin
               null;
            end;
         exception
            when Constraint_Error =>
               Exception_Raised := True;
         end;
         A.Assert (Exception_Raised);
      end;

      Set_Nth_Arg (Args, 3, "c");
      A.Assert (Args_Length (Args) = 3);
      A.Assert (Nth_Arg (Args, 1), "a b");
      A.Assert (Nth_Arg (Args, 2), "");
      A.Assert (Nth_Arg (Args, 3), "c");

      A.Assert (+Nth_Arg (Args, 3), "c");
   end;

   ------------------
   -- Substitution --
   ------------------

   Test_Substitute
     (Debug_String => "Command: cmd"
                      & ASCII.LF & "Arg: %a"
                      & ASCII.LF & "Arg: ",
      Args         => (+"%a", +""),
      Cmd_Mode     => Raw_String,
      Arg_Mode     => One_Arg);

   Test_Substitute
     (Debug_String => "Command: << cmd >>"
                      & ASCII.LF & "Arg: %a",
      Command      => "%cmd",
      Args         => (1 => +"%a"),
      Cmd_Mode     => Raw_String,
      Arg_Mode     => One_Arg);

   Test_Substitute
     (Debug_String => "Command: cmd"
                      & ASCII.LF & "Arg: << a >>"
                      & ASCII.LF & "Arg: %"
                      & ASCII.LF & "Arg: ",
      Args         => (+"%a", +"%%", +""),
      Cmd_Mode     => Separate_Args,
      Arg_Mode     => One_Arg);

   Test_Substitute
     (Debug_String => "Command: cmd"
                      & ASCII.LF & "Arg: <<"
                      & ASCII.LF & "Arg: a"
                      & ASCII.LF & "Arg: >>"
                      & ASCII.LF & "Arg: <<"
                      & ASCII.LF & "Arg: %"
                      & ASCII.LF & "Arg: >>",
      Args         => (+"%a", +"%%"),
      Cmd_Mode     => Separate_Args,
      Arg_Mode     => Expandable);

   Test_Substitute
     (Debug_String => "Command: cmd"
                      & ASCII.LF & "Arg: zz<< a >> bzz"
                      & ASCII.LF & "Arg: zz<< a b >>zz",
      Args         => (+"zz%a bzz", +"zz%{a b}zz"),
      Cmd_Mode     => Separate_Args,
      Arg_Mode     => Expandable);

   Test_Substitute
     (Debug_String => "Command: << \\a >>",
      Command      => "%{\a}",
      Args         => (1 .. 0 => <>),
      Cmd_Mode     => Raw_String,
      Arg_Mode     => Expandable);

   Test_Substitute
     (Debug_String => "Command: cmd"
                      & ASCII.LF & "Arg: --option=%",
      Args         => (1 => +"--option=%"),
      Cmd_Mode     => Separate_Args,
      Arg_Mode     => Expandable);

   --  Make sure that Substitute works on an empty command line

   declare
      Args : Arg_List := Empty_Command_Line;
   begin
      Substitute (Args, '%', Substitution_Callback'Access);
      A.Assert (Args = Empty_Command_Line);
   end;

   --  Make sure that substitute does nothing when passed a null callback

   declare
      Args     : constant Arg_List := Parse_String ("cmd", "%a");
      New_Args : Arg_List := Args;
   begin
      Substitute (New_Args, '%', null);
      A.Assert (Args = New_Args);
   end;

   Test_To_List
     (Expected        => (+"b", +"c"),
      C               => (+"a", +"b", +"c"),
      Include_Command => False);

   Test_To_List
     (Expected        => (+"a", +"b", +"c"),
      C               => (+"a", +"b", +"c"),
      Include_Command => True);

   ---------------------------
   -- Conversions to string --
   ---------------------------

   --  To_Display_String

   Test_To_Display_String
     (Expected        => "cmd",
      C               => Parse_String ("cmd", ""),
      Include_Command => True);

   Test_To_Display_String
     (Expected        => "",
      C               => Parse_String ("cmd", ""),
      Include_Command => False);

   Test_To_Display_String
     (Expected        => "cmd arg1 arg2",
      C               => Parse_String ("cmd", "arg1 arg2"),
      Include_Command => True);

   Test_To_Display_String
     (Expected        => "cmd 0123456789",
      C               => Parse_String ("cmd", "0123456789"),
      Include_Command => True,
      Max_Arg_Length  => 10);

   Test_To_Display_String
     (Expected        => "cmd 012345678...",
      C               => Parse_String ("cmd", "0123456789abcdef"),
      Include_Command => True,
      Max_Arg_Length  => 10);

   --  To_String_String

   Test_To_Script_String
     (Expected => "",
      C        => Empty_Command_Line);

   Test_To_Script_String
     (Expected => "cmd",
      C        => Parse_String ("cmd", Raw_String));

   declare
      C : Arg_List := Parse_String ("cmd", Raw_String);
   begin
      Append_Argument (C, "arg", One_Arg);
      Test_To_Script_String
        (Expected => "cmd",
         C        => C);
   end;

   Test_To_Script_String
     (Expected => "cmd arg",
      C        => Parse_String ("cmd", "arg"));

   Test_To_Script_String
     (Expected => "cmd arg\\1 arg\ 2 arg\""3",
      C        => Create_Args (Command  => "cmd",
                               Args     => (+"arg\1", +"arg 2", +"arg""3"),
                               Cmd_Mode => Separate_Args,
                               Arg_Mode => Expandable));

   return A.Report;
end Test;
