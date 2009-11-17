-----------------------------------------------------------------------
--                           G N A T C O L L                         --
--                                                                   --
--                      Copyright (C) 2009, AdaCore                  --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides a type useful to manipulate command lines.

with GNAT.OS_Lib;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package GNATCOLL.Command_Lines is

   type Command_Line_Mode is (Raw_String, Separate_Args);
   --  There are two ways to treat a command line in GPS.
   --    Raw_String: these command lines should never be parsed for arguments
   --                and processing should be minimal.
   --    Separate_Args: these command lines need argument handling.
   --
   --  For instance if an user specifies an action with
   --    <shell lang=python>GPS.Process ("bla")</shell>
   --    <shell>File %*</shell>
   --  then the string 'GPS.Process ("bla")' should be transmitted as-is to
   --  Python, but the string "File %*" needs to be treated for arguments.

   type Argument_Mode is (Expandable, One_Arg);
   --  This type controls the behavior of arguments with respect to expansion.
   --    Expandable means that this argument can be expanded into multiple
   --     arguments.
   --    One_Arg means that this argument will only remain one argument,
   --     even if it gets expanded to separate space-separated strings.

   type Command_Line is private;
   --  A command line.
   --  This contains one command (an executable, typically) and a list of
   --  arguments.

   Empty_Command_Line : constant Command_Line;

   function Get_Command (C : Command_Line) return String;
   --  Return the command contained in C.

   function Create (Command : String) return Command_Line;
   --  Create a command line from command.
   --  This creates a command line which has Command as a command and
   --  no arguments.

   function Parse_String
     (Text : String;
      Mode : Command_Line_Mode) return Command_Line;
   --  Parse Text and return a Command_Line, assuming that Text contains both
   --  the command and the arguments

   function Parse_String (Command : String; Text : String) return Command_Line;
   --  Return a command line, assuming Command contains the command and
   --  Text contains the arguments

   procedure Append_Argument
     (C        : in out Command_Line;
      Argument : String;
      Mode     : Argument_Mode);
   --  Append Argument to the list of arguments in C

   function Args_Length (C : Command_Line) return Integer;
   --  Return the length of the arguments. The command is not included in this
   --  count.
   --  Return 0 if there is only a command and no arguments.
   --  Return -1 if the command is empty.

   function Nth_Arg (C : Command_Line; N : Natural) return String;
   --  Return the Nth argument. Nth_Arg (0) returns the command.

   procedure Set_Nth_Arg (C : in out Command_Line; N : Positive; Arg : String);
   --  Set the Nth arg.
   --  If there are not enough args, create them.

   ------------------
   -- Substitution --
   ------------------

   type Substitution_Function is access
     function (Param : String; Mode : Command_Line_Mode) return Command_Line;

   procedure Substitute
     (CL       : in out Command_Line;
      Char     : Character;
      Callback : Substitution_Function);
   --  Substitute all parameters that start with Char using the mechanisms
   --  specified in Callback.

   function To_List
     (C               : Command_Line;
      Include_Command : Boolean) return GNAT.OS_Lib.Argument_List;
   --  Return as an Argument_List:
   --     - the whole command line if Include_Command is True
   --     - only the arguments if Include_Command is False
   --  Caller must free the result.

   ---------------------------
   -- Conversions to string --
   ---------------------------

   function To_Display_String (C : Command_Line) return String;
   --  Return a string that represents C, for display purposes.
   --  For instance
   --       cmd /c make LIBRARY_TYPE=static

   function To_Debug_String (C : Command_Line) return String;
   --  Return a string that represents C, for display purposes.
   --  For instance:
   --      command: "cmd"
   --          arg: "/c"
   --          arg: "make LIBRARY_TYPE=static"

   function To_Script_String (C : Command_Line) return String;
   --  Return a string that represents C, ready to be sent to a script
   --  For instance:
   --      cmd /c make\ LIBRARY_TYPE=static

private

   type Argument_Type is record
      Mode : Argument_Mode;
      Text : Unbounded_String;
   end record;

   package Command_Line_Vector is new Ada.Containers.Vectors
     (Natural, Argument_Type);

   type Command_Line is record
      Mode : Command_Line_Mode := Separate_Args;
      V    : Command_Line_Vector.Vector;
      --  The element number 0 is the command, and the following elements are
      --  arguments.
   end record;

   Empty_Command_Line : constant Command_Line :=
     (Mode => Separate_Args,
      V    => Command_Line_Vector.Empty_Vector);

end GNATCOLL.Command_Lines;
