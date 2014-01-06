------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body GNATCOLL.Readline is

   type Rl_Completion_Function is access
      function (Text  : Interfaces.C.Strings.chars_ptr;
                Start : Integer;
                Last  : Integer) return Possible_Completions;
   pragma Convention (C, Rl_Completion_Function);

   type Rl_Compentry_Func is access
      function (Text : Interfaces.C.Strings.chars_ptr;
                State : Integer) return chars_ptr;
   pragma Convention (C, Rl_Compentry_Func);
   --  A function that returns each of the possible completions for
   --  Text. On the first call, STATE is set to 0, and then is non-zero.
   --  This function should return Null_Ptr when there are no more matches.

   function Rl_Completion_Matches
      (Text : chars_ptr;
       Generator : Rl_Compentry_Func) return Possible_Completions;
   pragma Import (C, Rl_Completion_Matches, "rl_completion_matches");
   --  Returns an array of strings which is a list of completions for
   --  TEXT. If there are no completions, returns NULL.
   --  GENERATOR is a function that returns all possible completions, in
   --  turn.

   procedure Using_History;
   pragma Import (C, Using_History, "using_history");
   --  Enables support for history.

   procedure Read_History (filename : chars_ptr);
   pragma Import (C, Read_History, "read_history");
   --  Reads the history from a file.

   procedure Write_History (Filename : chars_ptr);
   pragma Import (C, Write_History, "write_history");
   --  Write the current history back to a file.

   function C_Completer
      (Text  : Interfaces.C.Strings.chars_ptr;
       Start : Integer;
       Last  : Integer) return Possible_Completions;
   pragma Convention (C, C_Completer);
   --  Attempt to complete on the contents of TEXT.  START and END bound the
   --  region of rl_line_buffer that contains the word to complete.  TEXT is
   --  the word to complete.  We can use the entire contents of rl_line_buffer
   --  in case we want to do some simple parsing.  Return the array of matches,
   --  or NULL if there aren't any.

   Rl_Line_Buffer : chars_ptr;
   pragma Import (C, Rl_Line_Buffer, "rl_line_buffer");
   --  The line gathered so far by readline.

   Rl_Readline_Name : Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Rl_Readline_Name, "rl_readline_name");

   Rl_Attempted_Completion_Function : Rl_Completion_Function;
   pragma Import (C, Rl_Attempted_Completion_Function,
                  "rl_attempted_completion_function");
   --  A pointer to an alternative function to create matches.

   Ada_Completer : Completer_Function;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Appname      : String := "";
      History_File : String := "";
      Completer    : Completer_Function := null)
   is
      Tmp : chars_ptr;
   begin
      if Appname /= "" then
         Rl_Readline_Name := New_String (Appname);
      end if;

      if Completer /= null then
         Rl_Attempted_Completion_Function := C_Completer'Access;
         Ada_Completer := Completer;
      end if;

      Using_History;

      if History_File /= "" then
         Tmp := New_String (History_File);
         Read_History (Tmp);
         Free (Tmp);
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (History_File : String := "") is
      Tmp : chars_ptr;
   begin
      if History_File /= "" then
         Tmp := New_String (History_File);
         Write_History (Tmp);
         Free (Tmp);
      end if;
   end Finalize;

   ------------------------
   -- Completion_Matches --
   ------------------------

   function Completion_Matches
      (Text      : String;
       Generator : Completion_Entry_Func)
      return Possible_Completions
   is
      Txt : Interfaces.C.Strings.chars_ptr := New_String (Text);

      function Completion_Generator
         (Txt : Interfaces.C.Strings.chars_ptr;
          State : Integer) return chars_ptr;
      pragma Convention (C, Completion_Generator);
      --  Wrapper around the user's completion function.

      function Completion_Generator
         (Txt : Interfaces.C.Strings.chars_ptr;
          State : Integer) return chars_ptr
      is
         pragma Unreferenced (Txt);
         Choice : constant String := Generator (Text, State);
      begin
         if Choice = "" then
            return Null_Ptr;
         else
            return New_String (Choice);
         end if;
      end Completion_Generator;

      Result : Possible_Completions;
   begin
      Result := Rl_Completion_Matches (
         Txt, Completion_Generator'Unrestricted_Access);
      Free (Txt);
      return Result;
   end Completion_Matches;

   -----------------
   -- C_Completer --
   -----------------

   function C_Completer
      (Text  : Interfaces.C.Strings.chars_ptr;
       Start : Integer;
       Last  : Integer) return Possible_Completions
   is
   begin
      return Ada_Completer
         (Full_Line => Value (Rl_Line_Buffer),
          Text      => Value (Text),
          Start     => Start,
          Last      => Last);
   end C_Completer;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Prompt : String := "") return String is
      function Readline (Prompt : chars_ptr) return chars_ptr;
      pragma Import (C, Readline, "readline");

      procedure Add_History (Str : chars_ptr);
      pragma Import (C, Add_History, "add_history");

      Pr : chars_ptr;
      Result : chars_ptr;
   begin
      if Prompt /= "" then
         Pr := New_String (Prompt);
         Result := Readline (Pr);
         Free (Pr);
      else
         Result := Readline (Null_Ptr);
      end if;

      if Result /= Null_Ptr then
         declare
            Val : constant String := Value (Result);
         begin
            if Val /= "" then
               Add_History (Result);
            end if;

            Free (Result);
            return Val;
         end;
      else
         raise Ada.Text_IO.End_Error;
      end if;
   end Get_Line;

end GNATCOLL.Readline;
