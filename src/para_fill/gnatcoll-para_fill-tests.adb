-----------------------------------------------------------------------
--                          G N A T C O L L                          --
--                                                                   --
--                       Copyright (C) 2010, AdaCore                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  This software was originally contributed by William A. Duff.

with Ada.Command_Line;
with Ada.Command_Line.Remove;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

package body GNATCOLL.Para_Fill.Tests is

   procedure Command_Line_Error (Command_Name : String);
   --  Print usage message and exit with bad status code.

   function Line_Is_Blank (Line : String) return Boolean;
   --  Returns True if all the cahractersin Line  are whitespace (as defined by
   --  the Is_Whitespase function), or if Line is empty. Otherwise returns
   --  False.

   type Item_Kind is (End_Of_File, Single_Line, Comment, Dummy_Kind);
   --  Dummy_Kind is not really used; it is just there because we need a
   --  default for the Kind discriminant.

   --  We parse the input file into a sequence of "items". Each item is a
   --  single non-comment line of code (which could have a comment at the end),
   --  a comment, or the end-of-file mark.

   --  Comments are treated differently by Get_Line and Get_Item, both of which
   --  return an Item. Get_Line recognizes a single comment line, with nothing
   --  but whitespace before the "--". Get_Item (which calls Get_Line) collects
   --  multiple comment lines that form a block into a single Comment item.

   --  Example: Suppose the input contains:
   --    --  This is
   --    --  a comment.
   --    if Blah then  --  Non-comment line
   --  ^
   --  |
   --  start of line is here.
   --  Then Get_Line will return these Items:
   --
   --    (Kind => Comment, Prefix => "  --  ", Text => "This is" & NL)
   --    (Kind => Comment, Prefix => "  --  ", Text => "a comment." & NL)
   --    (Kind => Single_Line, Line => "if Blah then  --  Non-comment line");
   --    (Kind => End_Of_File)
   --
   --  Get_Item will combine the first two into one:
   --
   --    (Kind => Comment, Prefix => "  --  ",
   --      Text => "This is" & NL & "a comment." & NL)
   --    (Kind => Single_Line, Line => "if Blah then  --  Non-comment line");
   --    (Kind => End_Of_File)
   --
   --  The comment Text is suitable for passing to the paragraph formatting
   --  routine.  We need to reattach the Prefix on output.

   type Item (Kind : Item_Kind := Dummy_Kind) is record
      case Kind is
         when End_Of_File | Dummy_Kind =>
            null;

         when Single_Line =>
            Line : Unbounded_String;
         --  The text of the line, with no LF terminator

         when Comment =>
            Prefix : Unbounded_String;
            --  The prefix of the comment line or comment block
            Text : Unbounded_String;
            --  For Get_Line: the text of the comment line followed by NL.
            --  For Get_Item: the text of the comment block, with NL used as
            --  line terminator.
            --  In both cases, the Prefix has been removed.
      end case;
   end record;

   Cur_Line : Item;
   --  Current item read by Get_Line. Used as a lookahead by Get_Item.

   procedure Put_Item (Output : Text_IO.File_Type; X : Item);
   --  Send X to the Output. Single_Lines are sent unchanged. For Comments, we
   --  convert NL's to New_Line calls, and prefix each line with Prefix.

   function Comment_Prefix_Last (Line : String) return Natural;
   --  If Line is a comment line, this returns the index of the last character
   --  of the comment prefix. Example: for " -- xxx", returns 4, pointing just
   --  before the first x. If it's not a comment line, returns 0.

   procedure Get_Line (Input : Text_IO.File_Type; Line_Item : out Item);
   --  Get the next item from the Input, treating a comment line as a single
   --  Item.

   procedure Get_Item (Input : Text_IO.File_Type; Result : out Item);
   --  Same as Get_Line, except this combines multiple comment lines that form
   --  a comment block into a single Comment Item.

   ------------------------
   -- Command_Line_Error --
   ------------------------

   procedure Command_Line_Error (Command_Name : String) is
   begin
      Put_Line ("Usage: " & Command_Name & " [options] infile outfile");
      Put_Line ("  options:");
      Put_Line ("    -none -- no formatting");
      Put_Line ("    -greedy");
      Put_Line ("    -pretty");
      Put_Line ("    -knuth");
      Put_Line ("    -slow");
      Put_Line ("");
      Put_Line ("    -n (n = maximum line length; default ="
              & Default_Max_Line_Length'Img & ")");
      GNAT.OS_Lib.OS_Exit (Status => -1);
   end Command_Line_Error;

   -------------------------
   -- Comment_Prefix_Last --
   -------------------------

   Comment_Pattern : constant Pattern_Matcher := Compile ("^\s*--\s\s?[^\s]");
   --  Start of line, followed by zero or more spaces, followed by the Ada
   --  comment marker "--", followed by one or two spaces, followed by a
   --  non-space. If there are more than two spaces after "--", then we don't
   --  recognize it as a comment line (it's an indented comment, which should
   --  not be reformatted). We also don't recognize it as a comment line if
   --  there is no space after "--".

   function Comment_Prefix_Last (Line : String) return Natural is
      Matches : Match_Array (0 .. 0);
   begin
      Match (Comment_Pattern, Line, Matches);
      if Matches (0) /= No_Match then
         pragma Assert (Matches (0).First = Line'First);

         --  If the comment line ends with "--", we don't recognize it as a
         --  comment line, because it's probably part of the copyright header,
         --  which we don't want to reformat.

         if Line (Line'Last - 1 .. Line'Last) = "--" then
            Matches (0) := No_Match;
         end if;
      end if;

      return Matches (0).Last;
      --  Note that Matches (0).Last will be 0 if it didn't match
   end Comment_Prefix_Last;

   ---------------------
   -- Format_Ada_File --
   ---------------------

   procedure Format_Ada_File
     (Input, Output : Text_IO.File_Type;
      Format        : not null access function
        (Paragraph       : String;
         Max_Line_Length : Positive)
         return            String;
      Max_Line_Length : Positive)
   is

      procedure Form
        (Text            : in out Unbounded_String;
         Max_Line_Length : Positive);
      --  Wrapper to call Format converting the types as necessary

      procedure Form
        (Text            : in out Unbounded_String;
         Max_Line_Length : Positive)
      is
      begin
         Text :=
            To_Unbounded_String (Format (To_String (Text), Max_Line_Length));
      end Form;

      Cur_Item : Item;

   --  Start of processing for Format_Ada_File

   begin
      --  Initialize the lookahead

      Get_Line (Input, Cur_Line);

      --  Loop through all the Items, and send them to the Output, calling
      --  Format first for Comment items.

      loop
         Get_Item (Input, Cur_Item);
         exit when Cur_Item.Kind = End_Of_File;
         if Cur_Item.Kind = Comment then
            Form
              (Cur_Item.Text,
               Max_Line_Length => Max_Line_Length - Length (Cur_Item.Prefix));
         end if;
         Put_Item (Output, Cur_Item);
      end loop;
   end Format_Ada_File;

   ---------------------
   -- Format_Ada_File --
   ---------------------

   procedure Format_Ada_File
     (Input_Name, Output_Name : String;
      Format                  : not null access function
        (Paragraph       : String;
         Max_Line_Length : Positive)
      return            String;
      Max_Line_Length : Positive)
   is

      Input, Output : Text_IO.File_Type;
   begin
      Open (Input, In_File, Name => Input_Name);
      Create
        (Output,
         Out_File,
         Name => Output_Name,
         Form => "Text_Translation=No");

      Format_Ada_File (Input, Output, Format, Max_Line_Length);

      Close (Input);
      Close (Output);
   end Format_Ada_File;

   --------------
   -- Get_Item --
   --------------

   procedure Get_Item (Input : Text_IO.File_Type; Result : out Item) is
   begin
      Result := Cur_Line;
      case Result.Kind is
         when End_Of_File =>
            null;

         when Dummy_Kind =>
            raise Program_Error;

         when Single_Line =>
            Get_Line (Input, Cur_Line);

         --  Combine multiple comment lines into a comment block if they have
         --  the same Prefix.

         when Comment =>
            loop
               Get_Line (Input, Cur_Line);
               exit when Cur_Line.Kind /= Comment
                        or else Cur_Line.Prefix /= Result.Prefix;
               Append (Result.Text, Cur_Line.Text);
            end loop;
      end case;
   end Get_Item;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (Input : Text_IO.File_Type; Line_Item : out Item) is
   begin
      if End_Of_File (Input) then
         Line_Item := (Kind => End_Of_File);
      else
         declare
            Line : constant String  := Get_Line (Input);
            Last : constant Natural := Comment_Prefix_Last (Line);
         begin
            --  Not a comment line; return Single_Line
            if Last = 0 then
               Line_Item :=
                 (Kind => Single_Line,
                  Line => To_Unbounded_String (Line));

            --  A comment line. Split it into two parts, the prefix and the
            --  comment text, and terminate with LF.

            else
               declare
                  Prefix : String renames Line (Line'First .. Last - 1);
                  Suffix : String renames Line (Last .. Line'Last);
               begin
                  Line_Item :=
                    (Kind   => Comment,
                     Prefix => To_Unbounded_String (Prefix),
                     Text   => To_Unbounded_String (Suffix));
                  Append (Line_Item.Text, ASCII.LF);
               end;
            end if;
         end;
      end if;
   end Get_Line;

   -------------------
   -- Get_Paragraph --
   -------------------

   function Get_Paragraph (File : File_Type) return String is
      Paragraph : Unbounded_String := Null_Unbounded_String;
      Count     : Natural          := 0;
   begin
      while not End_Of_File (File) loop
         Count := Count + 1;
         declare
            Current_Line : constant String := Get_Line (File);
         begin
            if Line_Is_Blank (Current_Line) then
               exit;
            end if;
            Append (Paragraph, Current_Line);
            Append (Paragraph, ASCII.LF);
         end;
      end loop;
      return To_String (Paragraph);
   end Get_Paragraph;

   -------------------
   -- Line_Is_Blank --
   -------------------

   function Line_Is_Blank (Line : String) return Boolean is
   begin
      for Count in Line'Range loop
         if not (Line (Count) = ' ' or else Line (Count) = ASCII.HT) then
            return False;
         end if;
      end loop;
      return True;
   end Line_Is_Blank;

   --------------------------
   -- Process_Command_Line --
   --------------------------

   procedure Process_Command_Line (Command_Name : String) is
      Arg_Index : Positive := 1;
   begin
      while Arg_Index <= Command_Line.Argument_Count loop
         declare
            Arg : constant String := Command_Line.Argument (Arg_Index);
         begin
            if Arg'Length >= 1 and then Arg (Arg'First) = '-' then
               declare
                  Switch : constant String := Arg (Arg'First + 1 .. Arg'Last);
               begin
                  Command_Line.Remove.Remove_Argument (Arg_Index);

                  begin
                     Method := Formatting_Methods'Value (Switch);
                     goto Continue;
                  exception
                     when Constraint_Error =>
                        null;
                  end;

                  begin
                     Max_Line_Length := Positive'Value (Switch);
                     goto Continue;
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end;
               Command_Line_Error (Command_Name);

            else
               Arg_Index := Arg_Index + 1;
            end if;
         end;

         <<Continue>>
      end loop;

      if Command_Line.Argument_Count /= 2 then
         Command_Line_Error (Command_Name);
      end if;

      Input_Name  := new String'(Command_Line.Argument (1));
      Output_Name := new String'(Command_Line.Argument (2));

      case Method is
         when None =>
            Format := No_Fill'Access;

         when Greedy =>
            Format := Greedy_Fill'Access;

         when Pretty =>
            Format := Pretty_Fill'Access;

         when Knuth =>
            Format := Knuth_Fill'Access;

         when Slow =>
            Format := Slow_Fill'Access;
      end case;
   end Process_Command_Line;

   --------------
   -- Put_Item --
   --------------

   procedure Put_Item (Output : Text_IO.File_Type; X : Item) is
   begin
      case X.Kind is
         when End_Of_File | Dummy_Kind =>
            raise Program_Error;

         when Single_Line =>
            Put_Line (Output, X.Line);

         when Comment =>
            Put (Output, X.Prefix);
            for J in 1 .. Length (X.Text) loop
               declare
                  C : constant Character := Element (X.Text, J);
               begin
                  case C is
                     when ASCII.LF =>
                        New_Line (Output);
                        if J < Length (X.Text) then
                           Put (Output, X.Prefix);
                        end if;
                     when others =>
                        Put (Output, C);
                  end case;
               end;
            end loop;
      end case;
   end Put_Item;

end GNATCOLL.Para_Fill.Tests;
