------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;               use Ada.Calendar;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Atomic;            use GNATCOLL.Atomic;
with GNATCOLL.Scripts.Impl;      use GNATCOLL.Scripts.Impl;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with Interfaces;                 use Interfaces;
with System.Address_Image;

package body GNATCOLL.Scripts is
   Me : constant Trace_Handle := Create ("SCRIPTS");
   use Classes_Hash;

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   function To_Address is new Ada.Unchecked_Conversion
     (Class_Instance_Record_Access, System.Address);

   procedure Internal_Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Params        : Param_Array_Access := null;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "");
   --  Internal version of Register_Command

   function Get_Data
     (Instance : access Class_Instance_Record'Class; Name : String)
      return User_Data_List;
   --  Return the user data with the given name, or null if there is none

   procedure Unset_Data
      (Instance : access Class_Instance_Record'Class; Name : String);
   --  Remove the user data with the given name

   -----------------------------------
   -- Data stored in class_instance --
   -----------------------------------

   type User_Data_Type is (Strings, Integers, Booleans, Consoles, Floats);

   type Scalar_Properties_Record (Typ : User_Data_Type) is
     new Instance_Property_Record
   with record
      case Typ is
         when Strings =>
            Str : GNAT.Strings.String_Access;
         when Integers =>
            Int : Integer;
         when Floats =>
            Flt : Float;
         when Booleans =>
            Bool : Boolean;
         when Consoles =>
            Console : Virtual_Console;
      end case;
   end record;

   type Scalar_Properties is access all Scalar_Properties_Record'Class;
   overriding procedure Destroy (Prop : in out Scalar_Properties_Record);
   --  See inherited documentation

   -----------------
   -- Subprograms --
   -----------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Scripting_Language_Array, Scripting_Language_List);

   procedure Free_User_Data (Data : in out User_Data_List);
   --  Free the memory used by Data. Data is reset to null, and this doesn't
   --  free other user data in the list.

   procedure Free (Param : in out Param_Descr);
   procedure Free (Params : in out Param_Array_Access);
   --  Free memory

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out Instance_Property_Record) is
      pragma Unreferenced (Prop);
   begin
      null;
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (Param : in out Param_Descr) is
   begin
      Free (Param.Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Params : in out Param_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Param_Array, Param_Array_Access);
   begin
      if Params /= null then
         for P in Params'Range loop
            Free (Params (P));
         end loop;

         Unchecked_Free (Params);
      end if;
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Repo : in out Scripts_Repository) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scripts_Repository_Record'Class, Scripts_Repository);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Command_Descr, Command_Descr_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Property_Descr, Property_Descr_Access);
      C     : Classes_Hash.Cursor;
      Class : Class_Type;
      D, D_Tmp : Command_Descr_Access;
      Prop, P_Tmp : Property_Descr_Access;
   begin
      Trace (Me, "Destroying scripts repository");
      if Repo /= null then
         D := Repo.Commands;
         while D /= null loop
            D_Tmp := D.Next;
            Free (D.Params);
            Unchecked_Free (D);
            D := D_Tmp;
         end loop;

         Prop := Repo.Properties;
         while Prop /= null loop
            P_Tmp := Prop.Next;
            Unchecked_Free (Prop);
            Prop := P_Tmp;
         end loop;

         if Repo.Scripting_Languages /= null then
            for L in Repo.Scripting_Languages'Range loop
               Destroy (Repo.Scripting_Languages (L));
               --  Do not free the language itself, though. Since scripts are
               --  full of controlled types, it might happen that some of them
               --  will be freed later on, and they might still have pointers
               --  to the script itself.
               --  Unchecked_Free (Repo.Scripting_Languages (L));
            end loop;

            Unchecked_Free (Repo.Scripting_Languages);
         end if;

         C := First (Repo.Classes);
         while Has_Element (C) loop
            Class := Element (C);
            Free (Class.Qualified_Name);
            Next (C);
         end loop;

         Unchecked_Free (Repo);
      end if;
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Instance_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Instance_Array, Instance_Array_Access);
   begin
      --  Class_Instance are automatically Finalized by the compiler
      Unchecked_Free (List.List);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (List   : Instance_List;
      Script : access Scripting_Language_Record'Class) return Class_Instance is
   begin
      if List.List /= null then
         for Idx in List.List'Range loop
            exit when List.List (Idx) = No_Class_Instance;
            if Get_Script (List.List (Idx)) = Scripting_Language (Script) then
               return List.List (Idx);
            end if;
         end loop;
      end if;
      return No_Class_Instance;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (List   : in out Instance_List;
      Inst   : Class_Instance)
   is
      Idx : Natural;
   begin
      if List.List = null then
         declare
            Tmp : constant Scripting_Language_Array :=
              Get_Repository (Get_Script (Inst)).Scripting_Languages.all;
         begin
            List.List := new Instance_Array (Tmp'Range);
            List.List.all := (others => No_Class_Instance);
         end;
      end if;

      Idx := List.List'First;
      while Idx <= List.List'Last loop
         if not List.List (Idx).Initialized
            or else Get_Script (List.List (Idx)) = Get_Script (Inst)
         then
            List.List (Idx) := Inst;
            return;
         end if;
         Idx := Idx + 1;
      end loop;
   end Set;

   -----------
   -- First --
   -----------

   function First (Self : Instance_List) return Inst_Cursor is
   begin
      if Self.List = null
         or else not Self.List (Self.List'First).Initialized
      then
         return (Index => Natural'Last);
      else
         return (Index => Self.List'First);
      end if;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next
      (Self : Instance_List; Pos : in out Inst_Cursor) is
   begin
      if Pos.Index >= Self.List'Last
         or else not Self.List (Pos.Index + 1).Initialized
      then
         Pos := (Index => Natural'Last);
      else
         Pos := (Index => Pos.Index + 1);
      end if;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Inst_Cursor) return Boolean is
   begin
      return Position.Index /= Natural'Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
      (Self : Instance_List; Pos : Inst_Cursor) return Class_Instance is
   begin
      return Self.List (Pos.Index);
   end Element;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Callback_Data_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Callback_Data_Array, Callback_Data_List);
   begin
      if List /= null then
         for L in List'Range loop
            if List (L) /= null then
               Free (List (L));
            end if;
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (Repo   : access Scripts_Repository_Record'Class;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access
   is
      Tmp : constant Scripting_Language_Array := Repo.Scripting_Languages.all;
   begin
      if List /= null then
         for T in Tmp'Range loop
            if Tmp (T) = Scripting_Language (Script) then
               return List (T);
            end if;
         end loop;
      end if;
      return null;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Repo   : access Scripts_Repository_Record'Class;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access)
   is
      Tmp : constant Scripting_Language_Array := Repo.Scripting_Languages.all;
   begin
      if List = null then
         List := new Callback_Data_Array (Tmp'Range);
      end if;

      for T in Tmp'Range loop
         if Tmp (T) = Scripting_Language (Script) then
            if List (T) /= null
              and then List (T) /= Data
            then
               Free (List (T));
            end if;

            List (T) := Data;
            exit;
         end if;
      end loop;
   end Set;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Callback_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Callback_Data'Class, Callback_Data_Access);
   begin
      if Data /= null then
         Free (Data.all);
         Unchecked_Free (Data);
      end if;
   end Free;

   ---------------------------------
   -- Register_Scripting_Language --
   ---------------------------------

   procedure Register_Scripting_Language
     (Repo   : access Scripts_Repository_Record'Class;
      Script : access Scripting_Language_Record'Class)
   is
      Tmp : constant Scripting_Language_Array := Repo.Scripting_Languages.all;
   begin
      Unchecked_Free (Repo.Scripting_Languages);
      Repo.Scripting_Languages :=
        new Scripting_Language_Array'(Tmp & Scripting_Language (Script));
   end Register_Scripting_Language;

   -------------------------------
   -- Lookup_Scripting_Language --
   -------------------------------

   function Lookup_Scripting_Language
     (Repo : access Scripts_Repository_Record'Class;
      Name : String) return Scripting_Language
   is
      Tmp : constant Scripting_Language_List := Repo.Scripting_Languages;
      N   : constant String := To_Lower (Name);
   begin
      for T in Tmp'Range loop
         if To_Lower (Get_Name (Tmp (T))) = N then
            return Tmp (T);
         end if;
      end loop;

      return null;
   end Lookup_Scripting_Language;

   -----------------------------
   -- Get_Scripting_Languages --
   -----------------------------

   function Get_Scripting_Languages
     (Repo : access Scripts_Repository_Record'Class)
      return Scripting_Language_Array is
   begin
      return Repo.Scripting_Languages.all;
   end Get_Scripting_Languages;

   --------------------
   -- Block_Commands --
   --------------------

   procedure Block_Commands
     (Repo  : access Scripts_Repository_Record'Class;
      Block : Boolean)
   is
      Tmp : constant Scripting_Language_List := Repo.Scripting_Languages;
   begin
      for T in Tmp'Range loop
         Block_Commands (Tmp (T), Block);
      end loop;
   end Block_Commands;

   -----------
   -- Param --
   -----------

   function Param
     (Name : String; Optional : Boolean := False) return Param_Descr is
   begin
      return Param_Descr'
        (Name     => new String'(Name),
         Optional => Optional);
   end Param;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Params        : Param_Array;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "")
   is
      Min : Natural := Params'Length;
   begin
      for P in Params'Range loop
         if Params (P).Optional then
            Min := Min - 1;
         end if;
      end loop;

      Internal_Register_Command
        (Repo,
         Command       => Command,
         Minimum_Args  => Min,
         Maximum_Args  => Params'Length,
         Params        => new Param_Array'(Params),
         Handler       => Handler,
         Class         => Class,
         Static_Method => Static_Method,
         Language      => Language);
   end Register_Command;

   -------------------------------
   -- Internal_Register_Command --
   -------------------------------

   procedure Internal_Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Params        : Param_Array_Access := null;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "")
   is
      Tmp : constant Scripting_Language_List := Repo.Scripting_Languages;
      Cmd : Command_Descr_Access;
   begin
      if Command = Constructor_Method and then Class = No_Class then
         raise Program_Error
           with "Constructors can only be specified for classes";
      end if;

      if Static_Method and then Class = No_Class then
         raise Program_Error
           with "Static method can only be created for classes";
      end if;

      Cmd := new Command_Descr'
        (Length        => Command'Length,
         Command       => Command,
         Handler       => Handler,
         Class         => Class,
         Params        => Params,
         Static_Method => Static_Method,
         Minimum_Args  => Minimum_Args,
         Maximum_Args  => Maximum_Args,
         Next          => null);

      for T in Tmp'Range loop
         if Language = ""
           or else Get_Name (Tmp (T)) = Language
         then
            Register_Command (Tmp (T), Cmd);
         end if;
      end loop;

      --  Only add it to the list afterward, so that Register_Command is not
      --  tempted to look at Next
      Cmd.Next := Repo.Commands;
      Repo.Commands := Cmd;
   end Internal_Register_Command;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False;
      Language      : String := "")
   is
   begin
      Internal_Register_Command
        (Repo,
         Command       => Command,
         Minimum_Args  => Minimum_Args,
         Maximum_Args  => Maximum_Args,
         Params        => null,
         Handler       => Handler,
         Class         => Class,
         Static_Method => Static_Method,
         Language      => Language);
   end Register_Command;

   ----------------------
   -- Override_Command --
   ----------------------

   procedure Override_Command
     (Repo          : access Scripts_Repository_Record'Class;
      Command       : String;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class)
   is
      Cmd : Command_Descr_Access := Repo.Commands;
   begin
      while Cmd /= null loop
         if Cmd.Command = Command
           and then Cmd.Class = Class
         then
            Cmd.Handler := Handler;
            return;
         end if;

         Cmd := Cmd.Next;
      end loop;

      raise Program_Error with "Command " & Command & " not found";
   end Override_Command;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Repo     : access Scripts_Repository_Record'Class;
      Name     : String;
      Class    : Class_Type;
      Setter   : Module_Command_Function := null;
      Getter   : Module_Command_Function := null)
   is
      Tmp  : constant Scripting_Language_List := Repo.Scripting_Languages;
      Prop : Property_Descr_Access;
   begin
      if Setter = null and then Getter = null then
         raise Program_Error
           with "A property must have at least a getter or a setter";
      end if;

      Prop := new Property_Descr'
        (Length        => Name'Length,
         Name          => Name,
         Class         => Class,
         Getter        => Getter,
         Setter        => Setter,
         Next          => null);

      for T in Tmp'Range loop
         Register_Property (Tmp (T), Prop);
      end loop;

      --  Only add it to the list afterward, so that Register_Command is not
      --  tempted to look at Next
      Prop.Next := Repo.Properties;
      Repo.Properties := Prop;
   end Register_Property;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Repo   : access Scripts_Repository_Record'Class;
      Name   : String;
      Base   : Class_Type := No_Class;
      Module : Module_Type := Default_Module) return Class_Type
   is
      Tmp   : constant Scripting_Language_List := Repo.Scripting_Languages;
      Class : Class_Type;
   begin
      if Tmp = null then
         return No_Class;
      else
         Class := Lookup_Class (Repo, Name, Module);
         if not Class.Exists then
            Class.Exists := True;
            Include (Repo.Classes, Class.Qualified_Name.all, Class);
            for T in Tmp'Range loop
               Register_Class (Tmp (T), Name, Base, Module);
            end loop;
         end if;
         return Class;
      end if;
   end New_Class;

   ------------------
   -- Lookup_Class --
   ------------------

   function Lookup_Class
     (Repo   : access Scripts_Repository_Record;
      Name   : String;
      Module : Module_Type := Default_Module) return Class_Type
   is
      C     : Classes_Hash.Cursor;
      Class : Class_Type;

      function Qualified_Name return String;
      function Qualified_Name return String is
      begin
         if Module = Default_Module then
            return Name;
         else
            return To_String (Module.Name) & '.' & Name;
         end if;
      end Qualified_Name;

      N     : constant String := Qualified_Name;
   begin
      C := Find (Repo.Classes, N);
      if Has_Element (C) then
         return Element (C);
      else
         Class := Class_Type'
           (Qualified_Name => new String'(N), Exists => False);
         Include (Repo.Classes, N, Class);
         return Class;
      end if;
   end Lookup_Class;

   -------------------
   -- Lookup_Module --
   -------------------

   function Lookup_Module
     (Repo           : access Scripts_Repository_Record;
      Qualified_Name : String) return Module_Type
   is
      pragma Unreferenced (Repo);
   begin
      return Module_Type'(Name => To_Unbounded_String (Qualified_Name));
   end Lookup_Module;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Class : Class_Type) return String is
   begin
      if Class.Qualified_Name = null then
         return "";
      elsif Class.Qualified_Name'Length > 2
        and then Class.Qualified_Name
          (Class.Qualified_Name'First .. Class.Qualified_Name'First + 1) = "@."
      then
         return Class.Qualified_Name
           (Class.Qualified_Name'First + 2 .. Class.Qualified_Name'Last);
      else
         return Class.Qualified_Name.all;
      end if;
   end Get_Name;

   -------------------------------
   -- Register_Standard_Classes --
   -------------------------------

   procedure Register_Standard_Classes
     (Repo               : access Scripts_Repository_Record'Class;
      Console_Class_Name : String;
      Logger_Class_Name  : String := "") is
   begin
      Repo.Console_Class := New_Class (Repo, Console_Class_Name);
      Register_Console_Class (Repo, Repo.Console_Class);

      if Logger_Class_Name /= "" then
         Repo.Logger_Class := New_Class (Repo, Logger_Class_Name);
         Register_Logger_Class (Repo, Repo.Logger_Class);
      end if;
   end Register_Standard_Classes;

   -------------------------------
   -- Execute_Command_With_Args --
   -------------------------------

   function Execute_Command_With_Args
     (Script : access Scripting_Language_Record;
      CL     : Arg_List) return String
   is
      pragma Unreferenced (Script, CL);
   begin
      raise Program_Error;
      return "";
   end Execute_Command_With_Args;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : access Scripting_Language_Record;
      CL           : Arg_List;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String is
   begin
      Execute_Command
        (Scripting_Language (Script),
         CL, Console, Hide_Output, Show_Command, Errors.all);
      return "";
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : out Boolean) is
   begin
      Execute_Command
        (Scripting_Language (Script),
         Parse_String
            (Command, Command_Line_Treatment (Scripting_Language (Script))),
         Console, Hide_Output, Show_Command, Errors);
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script      : access Scripting_Language_Record;
      Command     : String;
      Console     : Virtual_Console := null;
      Hide_Output : Boolean := False;
      Errors      : access Boolean) return Boolean is
   begin
      return Execute_Command
         (Scripting_Language (Script),
          Parse_String
             (Command, Command_Line_Treatment (Scripting_Language (Script))),
          Console, Hide_Output, Errors);
   end Execute_Command;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : Scripting_Language;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String is
   begin
      return Execute_Command
        (Script,
         Parse_String (Command, Command_Line_Treatment (Script)),
         Console,
         Hide_Output,
         Show_Command,
         Errors);
   end Execute_Command;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Script : access Scripting_Language_Record) return Boolean
   is
      pragma Unreferenced (Script);
   begin
      return False;
   end Interrupt;

   --------------
   -- Complete --
   --------------

   procedure Complete
     (Script      : access Scripting_Language_Record;
      Input       : String;
      Completions : out String_Lists.List)
   is
      pragma Unreferenced (Script, Input);
   begin
      Completions := String_Lists.Empty_List;
   end Complete;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type is
   begin
      return Subprogram_Type'(Nth_Arg (Callback_Data'Class (Data), N));
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : String) return String is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Filesystem_String)
      return Filesystem_String is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Integer) return Integer is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Float) return Float is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Boolean) return Boolean is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Default    : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N, Class, Allow_Null);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   --------------------
   -- Get_Repository --
   --------------------

   function Get_Repository (Data : Callback_Data) return Scripts_Repository is
   begin
      return Get_Repository (Get_Script (Callback_Data'Class (Data)));
   end Get_Repository;

   ----------
   -- Free --
   ----------

   procedure Free (Subprogram : in out Subprogram_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Subprogram_Record'Class, Subprogram_Type);
   begin
      if Subprogram /= null then
         Free (Subprogram.all);
         Unchecked_Free (Subprogram);
      end if;
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Boolean
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return String
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Class_Instance
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return List_Instance'Class
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class) return Any_Type
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   function Execute
     (Subprogram : access Subprogram_Record'Class;
      Args       : Callback_Data'Class)
      return GNAT.Strings.String_List
   is
      Err : aliased Boolean;
   begin
      return Execute (Subprogram, Args, Err'Access);
   end Execute;

   --------------------
   -- Free_User_Data --
   --------------------

   procedure Free_User_Data (Data : in out User_Data_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (User_Data, User_Data_List);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Instance_Property_Record'Class, Instance_Property);
   begin
      if Data /= null then
         if Data.Prop /= null then
            Destroy (Data.Prop.all);
            Unchecked_Free (Data.Prop);
         end if;
         Unchecked_Free (Data);
      end if;
   end Free_User_Data;

   -------------------------
   -- Free_User_Data_List --
   -------------------------

   procedure Free_User_Data_List (Data : in out User_Data_List) is
      D : User_Data_List;
   begin
      while Data /= null loop
         D := Data;
         Data := Data.Next;
         Free_User_Data (D);
      end loop;
   end Free_User_Data_List;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out Scalar_Properties_Record) is
   begin
      case Prop.Typ is
         when Strings =>
            Free (Prop.Str);

         when Integers | Consoles | Booleans | Floats =>
            null;
      end case;
   end Destroy;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (CI : in out Class_Instance_Data) is
   begin
      if CI.Data /= null then
         Sync_Add_And_Fetch (CI.Data.Refcount'Access, 1);
         Incref (CI.Data);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (CI : in out Class_Instance_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Class_Instance_Record'Class, Class_Instance_Record_Access);

      Data  : Class_Instance_Record_Access := CI.Data;
      Dummy : Integer_32;
   begin
      --  Make Finalize idempotent (RM 7.6.1 (24))

      CI.Data := null;

      --  Data might be null in some rare cases. Most notably, it happens when
      --  GPS is being destroyed: the python module has already been destroyed,
      --  but we still have remaining CI finalized when GNAT finalizes
      --  everything before exit.

      if Data = null then
         return;
      end if;

      Dummy := Sync_Add_And_Fetch (Data.Refcount'Access, -1);
      Decref (Data);

      if Dummy = 0 then
         Unchecked_Free (Data);
      end if;
   end Finalize;

   ---------
   -- "=" --
   ---------

   function "=" (CI1, CI2 : Class_Instance_Data) return Boolean is
   begin
      return CI1.Data = CI2.Data;
   end "=";

   -------------
   -- Get_CIR --
   -------------

   function Get_CIR
     (Inst : Class_Instance) return Class_Instance_Record_Access is
   begin
      if Inst.Initialized then
         return Inst.Data.Data;
      else
         return null;
      end if;
   end Get_CIR;

   --------------------
   -- Print_Refcount --
   --------------------

   function Print_Refcount
     (Instance : access Class_Instance_Record) return String is
   begin
      return "CI=(" & System.Address_Image
        (To_Address (Class_Instance_Record_Access (Instance)))
        & Integer_32'Image (Instance.Refcount) & ")";
   end Print_Refcount;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Self : not null access Class_Instance_Record)
      return access User_Data_List is
   begin
      --  We could not make the operation abstract and private
      raise Program_Error with "Get_User_Data should be overridden";
      return null;
   end Get_User_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Class_Instance_Record'Class; Name : String)
      return User_Data_List
   is
      U : constant access User_Data_List := Instance.Get_User_Data;
      D : User_Data_List;
   begin
      if U /= null then
         D := U.all;
         while D /= null loop
            if D.Name = Name then
               return D;
            end if;
            D := D.Next;
         end loop;
      end if;
      return null;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Name     : String;
      Property : Instance_Property_Record'Class) is
   begin
      Set_Data (Instance.Data.Data, Name, Property);
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : access Class_Instance_Record'Class;
      Name     : String;
      Property : Instance_Property_Record'Class)
   is
      U        : constant access User_Data_List := Instance.Get_User_Data;
   begin
      if U /= null then
         Unset_Data (Instance, Name);
         U.all := new User_Data'
           (Length => Name'Length,
            Name   => Name,
            Next   => U.all,
            Prop   => new Instance_Property_Record'Class'(Property));
      end if;
   end Set_Data;

   ----------------
   -- Unset_Data --
   ----------------

   procedure Unset_Data (Instance : Class_Instance; Name : Class_Type) is
   begin
      Unset_Data (Instance.Data.Data, Get_Name (Name));
   end Unset_Data;

   ----------------
   -- Unset_Data --
   ----------------

   procedure Unset_Data (Instance : Class_Instance; Name : String) is
   begin
      Unset_Data (Instance.Data.Data, Name);
   end Unset_Data;

   ----------------
   -- Unset_Data --
   ----------------

   procedure Unset_Data
      (Instance : access Class_Instance_Record'Class; Name : String)
   is
      U        : constant access User_Data_List := Instance.Get_User_Data;
      D        : User_Data_List;
      Previous : User_Data_List;
   begin
      if U /= null then
         D := U.all;
         while D /= null loop
            if D.Name = Name then
               if Previous = null then
                  U.all := D.Next;
               else
                  Previous.Next := D.Next;
               end if;
               Free_User_Data (D);
               return;
            end if;
            Previous := D;
            D := D.Next;
         end loop;
      end if;
   end Unset_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : String) is
   begin
      Set_Data (Instance, Get_Name (Name), Create_Property (Value));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Integer) is
   begin
      Set_Data (Instance, Get_Name (Name), Create_Property (Value));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Float) is
   begin
      Set_Data (Instance, Get_Name (Name), Create_Property (Value));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Boolean) is
   begin
      Set_Data (Instance, Get_Name (Name), Create_Property (Value));
   end Set_Data;

   ---------------------
   -- Create_Property --
   ---------------------

   function Create_Property
     (Val : Boolean) return Instance_Property_Record'Class is
   begin
      return Scalar_Properties_Record'(Typ => Booleans, Bool => Val);
   end Create_Property;

   ---------------------
   -- Create_Property --
   ---------------------

   function Create_Property
     (Val : Integer) return Instance_Property_Record'Class is
   begin
      return Scalar_Properties_Record'(Typ => Integers, Int => Val);
   end Create_Property;

   ---------------------
   -- Create_Property --
   ---------------------

   function Create_Property
     (Val : Float) return Instance_Property_Record'Class is
   begin
      return Scalar_Properties_Record'(Typ => Floats, Flt => Val);
   end Create_Property;

   ---------------------
   -- Create_Property --
   ---------------------

   function Create_Property
     (Val : String) return Instance_Property_Record'Class is
   begin
      return Scalar_Properties_Record'
        (Typ => Strings, Str => new String'(Val));
   end Create_Property;

   ---------------
   -- As_String --
   ---------------

   function As_String (Prop : Instance_Property_Record'Class) return String is
   begin
      return Scalar_Properties_Record (Prop).Str.all;
   end As_String;

   ----------------
   -- Get_Method --
   ----------------

   function Get_Method
     (Instance : Class_Instance; Name : String) return Subprogram_Type
   is
   begin
      return Get_Method (Get_CIR (Instance), Name);
   end Get_Method;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : access Class_Instance_Record'Class;
      Name     : String) return Instance_Property
   is
      D : constant User_Data_List := Get_Data (Instance, Name);
   begin
      if D = null then
         return null;
      else
         return D.Prop;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance;
      Name     : String) return Instance_Property
   is
      U : User_Data_List := null;
   begin
      if Instance.Initialized then
         U := Get_Data (Get_CIR (Instance), Name);
      end if;

      if U /= null then
         return U.Prop;
      else
         return null;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Integer
   is
      Prop : constant Instance_Property :=
               Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties (Prop).Int;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Float
   is
      Prop : constant Instance_Property :=
               Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties (Prop).Flt;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Boolean
   is
      Prop : constant Instance_Property :=
               Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties (Prop).Bool;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return String
   is
      Prop : constant Instance_Property :=
               Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties (Prop).Str.all;
   end Get_Data;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Integer)
   is
      CIR : constant Class_Instance_Record_Access := Get_CIR (Instance);
   begin
      if CIR /= null then
         Set_Property (CIR, Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Float)
   is
      CIR : constant Class_Instance_Record_Access := Get_CIR (Instance);
   begin
      if CIR /= null then
         Set_Property (CIR, Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : Boolean)
   is
      CIR : constant Class_Instance_Record_Access := Get_CIR (Instance);
   begin
      if CIR /= null then
         Set_Property (CIR, Name, Value);
      end if;
   end Set_Property;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Instance : Class_Instance; Name : String; Value : String)
   is
      CIR : constant Class_Instance_Record_Access := Get_CIR (Instance);
   begin
      if CIR /= null then
         Set_Property (CIR, Name, Value);
      end if;
   end Set_Property;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Instance : Class_Instance) return Scripting_Language is
   begin
      return Instance.Data.Data.Script;
   end Get_Script;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Instance : Class_Instance; Base : Class_Type) return Boolean is
   begin
      return Is_Subclass (Get_CIR (Instance), Get_Name (Base));
   end Is_Subclass;

   function Is_Subclass
     (Instance : Class_Instance; Base : String) return Boolean is
   begin
      return Is_Subclass (Get_CIR (Instance), Base);
   end Is_Subclass;

   -------------------------
   -- Set_Default_Console --
   -------------------------

   procedure Set_Default_Console
     (Script  : access Scripting_Language_Record;
      Console : Virtual_Console) is
   begin
      if Script.Console /= null then
         Set_As_Default_Console (Script.Console, null);
      end if;

      if Console /= null then
         Set_As_Default_Console (Console, Scripting_Language (Script));
      end if;

      Script.Console := Console;
      Display_Prompt (Scripting_Language (Script));
   end Set_Default_Console;

   -------------------------
   -- Get_Default_Console --
   -------------------------

   function Get_Default_Console
     (Script : access Scripting_Language_Record) return Virtual_Console is
   begin
      return Script.Console;
   end Get_Default_Console;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Console  : access Virtual_Console_Record'Class) is
   begin
      --  Note: even if Console is a widget, the call to Set_Data_Primitive
      --  below will automatically take care of proper reference counting, so
      --  that no additional work is needed

      Set_Data_Primitive (Instance, Console);
      Set_Data
        (Instance, "virtualconsole",
         Scalar_Properties_Record'
           (Typ => Consoles, Console => Virtual_Console (Console)));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return Virtual_Console is
      D : constant Instance_Property := Get_Data (Instance, "virtualconsole");
   begin
      if D = null
        or else D.all not in Scalar_Properties_Record'Class
        or else Scalar_Properties (D).Typ /= Consoles
      then
         return null;
      else
         return Scalar_Properties (D).Console;
      end if;
   end Get_Data;

   -----------------------
   -- Get_Console_Class --
   -----------------------

   function Get_Console_Class
     (Repo : access Scripts_Repository_Record'Class) return Class_Type is
   begin
      return Repo.Console_Class;
   end Get_Console_Class;

   ----------
   -- Read --
   ----------

   function Read
     (Console    : access Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean;
      Prompt     : String) return String is
   begin
      if Prompt /= "" then
         Insert_Prompt (Virtual_Console (Console), Prompt);
      end if;

      return Read (Virtual_Console (Console), Size, Whole_Line);
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Console    : access Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      pragma Unreferenced (Console, Size, Whole_Line);
   begin
      return "";
   end Read;

   ----------------------------
   -- Process_Pending_Events --
   ----------------------------

   procedure Process_Pending_Events
     (Console : access Virtual_Console_Record'Class) is
   begin
      --  We mustn't do that if the commands are hidden, for some obscur
      --  reason found in GPS (python-gui.adb:1.25)

      if not Console.Hide_Output then
         if Clock - Console.Refresh_Timeout > Timeout_Threshold then
            Process_Pending_Events_Primitive (Console);
            Console.Refresh_Timeout := Clock;
         end if;
      end if;
   end Process_Pending_Events;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data  : in out Callback_Data'Class;
      N     : Positive;
      Value : Filesystem_String) is
   begin
      Set_Nth_Arg (Data, N, +(Value));
   end Set_Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive) return Filesystem_String is
   begin
      return +Nth_Arg (Data, N);
   end Nth_Arg;

   ----------------------
   -- Set_Return_Value --
   ----------------------

   procedure Set_Return_Value
     (Data : in out Callback_Data'Class; Value : Filesystem_String) is
   begin
      Set_Return_Value (Data, +Value);
   end Set_Return_Value;

   --------------
   -- Load_All --
   --------------

   function Load_All (File : GNATCOLL.VFS.Virtual_File) return Boolean is
      pragma Unreferenced (File);
   begin
      return True;
   end Load_All;

end GNATCOLL.Scripts;
