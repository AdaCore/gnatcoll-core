------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with GNATCOLL.Traces;   use GNATCOLL.Traces;
with System.Assertions;

package body GNATCOLL.Scripts.Impl is

   procedure Console_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles command related to Console class

   procedure Logger_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles command related to Logger class

   type Logger_Properties_Record is new Instance_Property_Record with record
      Handle   : Trace_Handle;
   end record;
   type Logger_Properties is access all Logger_Properties_Record'Class;

   -------------------
   -- From_Instance --
   -------------------

   function From_Instance
     (Script : access Scripting_Language_Record'Class;
      Inst   : access Class_Instance_Record'Class) return Class_Instance
   is
      Result : Class_Instance (Initialized => True);
   begin
      if Inst = null then
         return No_Class_Instance;
      end if;

      --  Do not modify the refcount, it should have been initialized properly
      --  already.
      Inst.Script := Scripting_Language (Script);

      --  Do not use an aggregate to limit the number of calls to
      --  Adjust/Finalize
      Result.Data.Data := Class_Instance_Record_Access (Inst);
      Incref (Result.Data.Data);
      return Result;
   end From_Instance;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String) is
   begin
      if (Console /= null and then Console.Hide_Output)
        or else (Script.Console /= null and then Script.Console.Hide_Output)
      then
         null;

      elsif Console /= null then
         Insert_Text (Console, Txt);

      elsif Script.Console /= null then
         Insert_Text (Script.Console, Txt);
      end if;
   end Insert_Text;

   ------------------
   -- Insert_Error --
   ------------------

   procedure Insert_Error
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String) is
   begin
      if Console /= null then
         Insert_Error (Console, Txt);
      elsif Script.Console /= null then
         Insert_Error (Script.Console, Txt);
      end if;
   end Insert_Error;

   -------------------
   -- Insert_Prompt --
   -------------------

   procedure Insert_Prompt
     (Script  : access Scripting_Language_Record'Class;
      Console : Virtual_Console := null;
      Txt     : String)
   is
   begin
      if Console /= null then
         Insert_Prompt (Console, Txt);
      elsif Script.Console /= null then
         Insert_Prompt (Script.Console, Txt);
      end if;
   end Insert_Prompt;

   -----------------------------
   -- Console_Command_Handler --
   -----------------------------

   procedure Console_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      type Mode_Kinds is (Text, Log, Error);
      Inst     : constant Class_Instance := Nth_Arg (Data, 1, Any_Class);
      Console  : Virtual_Console;
      Mode     : Mode_Kinds := Text;
   begin
      if Command = "write" then
         if Number_Of_Arguments (Data) = 3 then
            begin
               Mode := Mode_Kinds'Value (Nth_Arg (Data, 3));
            exception
               when Constraint_Error =>
                  Set_Error_Msg (Data, "Wrong value for ""mode"" parameter");
                  return;
            end;
         end if;

         Console := Get_Data (Inst);
         if Console /= null then
            case Mode is
               when Text =>
                  Insert_Text (Console, Nth_Arg (Data, 2));

               when Log =>
                  Insert_Log (Console, Nth_Arg (Data, 2));

               when Error =>
                  Insert_Error (Console, Nth_Arg (Data, 2));
            end case;
         else
            Set_Error_Msg (Data, "Console was closed by user");
         end if;

      elsif Command = "clear" then
         Console := Get_Data (Inst);
         if Console /= null then
            Clear (Console);
         else
            Set_Error_Msg (Data, "Console was closed by user");
         end if;

      elsif Command = "flush" then
         null;
         --  Do nothing, only needed for compatibility with Python's
         --  stdout stream

      elsif Command = "isatty" then
         Set_Return_Value (Data, False);

      elsif Command = "read" then
         Console := Get_Data (Inst);
         if Console /= null then
            Set_Return_Value
              (Data,
               Read (Console,
                     Size       => Nth_Arg (Data, 2, Integer'Last),
                     Whole_Line => False));
         else
            Set_Error_Msg (Data, "Console was closed by user");
         end if;

      elsif Command = "readline" then
         Console := Get_Data (Inst);
         if Console /= null then
            Set_Return_Value
              (Data,
               Read (Console,
                     Size       => Nth_Arg (Data, 2, Integer'Last),
                     Whole_Line => True));
         else
            Set_Error_Msg (Data, "Console was closed by user");
         end if;
      end if;
   end Console_Command_Handler;

   --------------------
   -- Logger_Handler --
   --------------------

   procedure Logger_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Logger_Data : constant String := "Logger";
      Inst : constant Class_Instance := Nth_Arg (Data, 1);
      Prop : Instance_Property;
   begin
      if Command = Constructor_Method then
         Set_Data
           (Inst, Logger_Data, Logger_Properties_Record'
              (Handle => Create (Nth_Arg (Data, 2))));

      elsif Command = "log" then
         Prop := Get_Data (Inst, Logger_Data);
         Trace (Logger_Properties (Prop).Handle, Nth_Arg (Data, 2));

      elsif Command = "set_active" then
         Prop := Get_Data (Inst, Logger_Data);
         Set_Active (Logger_Properties (Prop).Handle, Nth_Arg (Data, 2));

      elsif Command = "check" then
         begin
            Prop := Get_Data (Inst, Logger_Data);
            Assert (Logger_Properties (Prop).Handle,
                    Condition          => Nth_Arg (Data, 2),
                    Error_Message      => Nth_Arg (Data, 3),
                    Message_If_Success => Nth_Arg (Data, 4, ""));
         exception
            when System.Assertions.Assert_Failure =>
               Set_Error_Msg (Data, "Assertion error: " & Nth_Arg (Data, 3));
         end;

      elsif Command = "count" then
         Prop := Get_Data (Inst, Logger_Data);
         Set_Return_Value (Data, Count (Logger_Properties (Prop).Handle));
      end if;
   end Logger_Handler;

   ----------------------------
   -- Register_Console_Class --
   ----------------------------

   procedure Register_Console_Class
     (Repo  : access Scripts_Repository_Record'Class;
      Class : Class_Type) is
   begin
      Register_Command
        (Repo, "write",
         Params       => (Param ("text"),
                          Param ("mode", Optional => True)),
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Repo, "clear",
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Repo, "flush",
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Repo, "isatty",
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Repo, "read",
         Params       => (1 => Param ("size", Optional => True)),
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
      Register_Command
        (Repo, "readline",
         Params       => (1 => Param ("size", Optional => True)),
         Class        => Class,
         Handler      => Console_Command_Handler'Access);
   end Register_Console_Class;

   ---------------------------
   -- Register_Logger_Class --
   ---------------------------

   procedure Register_Logger_Class
     (Repo  : access Scripts_Repository_Record'Class;
      Class : Class_Type)
   is
   begin
      Register_Command
        (Repo, Constructor_Method,
         Params => (1 => Param ("name")),
         Class => Class,
         Handler => Logger_Handler'Access);
      Register_Command
        (Repo, "log",
         Params => (1 => Param ("message")),
         Class => Class,
         Handler => Logger_Handler'Access);
      Register_Command
        (Repo, "set_active",
         Params => (1 => Param ("active")),
         Class => Class,
         Handler => Logger_Handler'Access);
      Register_Command
        (Repo, "check",
         Params => (1 => Param ("condition"),
                    2 => Param ("error_message"),
                    3 => Param ("success_message", Optional => True)),
         Class => Class,
         Handler => Logger_Handler'Access);
      Register_Property
        (Repo, "count",
         Class  => Class,
         Getter => Logger_Handler'Access);
   end Register_Logger_Class;

end GNATCOLL.Scripts.Impl;
