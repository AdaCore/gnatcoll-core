------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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

with GNATCOLL.OS.Constants;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;

package body GNATCOLL.OS.Dir is

   --  Provide a simple stack of Dir_Handles needed by Walk function. As the
   --  implementation is private good usage of this API is assumed (and thus
   --  most validity checks removed).

   type Dir_Handle_Array is array (Natural range <>) of Dir_Handle;
   type Dir_Handle_Array_Access is access Dir_Handle_Array;
   type Dir_Handle_Stack is limited record
      Pos   : Natural := 0;
      Stack : Dir_Handle_Array_Access := null;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Dir_Handle_Array, Dir_Handle_Array_Access);

   function Allocate_Stack (Size : Natural) return Dir_Handle_Stack;
   --  Allocate a stack of Dir_Handle of size Size

   procedure Deallocate_Stack (Self : in out Dir_Handle_Stack);
   --  Free a stack

   procedure Push (Self : in out Dir_Handle_Stack; Element : Dir_Handle);
   --  Push Element into Self. User should ensure that Is_Full returns False
   --  before calling that function.

   function Pop (Self : in out Dir_Handle_Stack) return Dir_Handle;
   --  Pop Element from Self. User should ensure that Is_Empty returns False
   --  before calling that function.

   function Is_Empty (Self : Dir_Handle_Stack) return Boolean;
   --  Return True if Stack is empty

   function Is_Full (Self : Dir_Handle_Stack) return Boolean;
   --  Return True if Stack is full

   --------------------
   -- Allocate_Stack --
   --------------------

   function Allocate_Stack (Size : Natural) return Dir_Handle_Stack is
   begin
      return (Pos => 0, Stack => new Dir_Handle_Array (1 .. Size));
   end Allocate_Stack;

   ----------------------
   -- Deallocate_Stack --
   ----------------------

   procedure Deallocate_Stack (Self : in out Dir_Handle_Stack) is
   begin
      if Self.Stack /= null then
         Free (Self.Stack);
      end if;
   end Deallocate_Stack;

   ----------
   -- Push --
   ----------

   procedure Push (Self : in out Dir_Handle_Stack; Element : Dir_Handle) is
   begin
      Self.Pos := Self.Pos + 1;
      Self.Stack (Self.Pos) := Element;
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop (Self : in out Dir_Handle_Stack) return Dir_Handle is
   begin
      Self.Pos := Self.Pos - 1;
      return Self.Stack (Self.Pos + 1);
   end Pop;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Dir_Handle_Stack) return Boolean is
   begin
      return Self.Pos = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (Self : Dir_Handle_Stack) return Boolean is
   begin
      return Self.Pos = Self.Stack'Last;
   end Is_Full;

   ---------------
   -- Is_Opened --
   ---------------

   function Is_Opened (Handle : Dir_Handle) return Boolean is
   begin
      return Handle.Is_Opened;
   end Is_Opened;

   ----------------
   -- Attributes --
   ----------------

   function Attributes (Self : Dir_Entry) return Stat.File_Attributes
   is
   begin
      if End_Of_Iteration (Self) then
         raise OS_Error with "invalid directory entry";
      end if;
      return Self.Info;
   end Attributes;

   -----------
   -- Close --
   -----------

   procedure Close (Handle : in out Dir_Handle) is separate;

   ----------------------
   -- End_Of_Iteration --
   ----------------------

   function End_Of_Iteration (Self : Dir_Entry) return Boolean is
   begin
      return Self.Name_Last = 0;
   end End_Of_Iteration;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Directory (Attributes (Self));
   end Is_Directory;

   -------------------
   -- Is_Executable --
   -------------------

   function Is_Executable (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Executable (Attributes (Self));
   end Is_Executable;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Executable_File (Attributes (Self));
   end Is_Executable_File;

   -------------
   -- Is_File --
   -------------

   function Is_File (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_File (Attributes (Self));
   end Is_File;

   -----------------
   -- Is_Readable --
   -----------------

   function Is_Readable (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Readable (Attributes (Self));
   end Is_Readable;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Symbolic_Link (Attributes (Self));
   end Is_Symbolic_Link;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (Self : Dir_Entry) return Boolean is
   begin
      return Stat.Is_Writable (Attributes (Self));
   end Is_Writable;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time (Self : Dir_Entry) return Time is
   begin
      return Stat.Modification_Time (Attributes (Self));
   end Modification_Time;

   ----------
   -- Name --
   ----------

   function Name (Self : Dir_Entry) return UTF8.UTF_8_String is
   begin
      if End_Of_Iteration (Self) then
         raise OS_Error with "invalid directory entry";
      end if;

      return Self.Name_Buffer (1 .. Self.Name_Last);
   end Name;

   ----------
   -- Open --
   ----------

   function Open (Path : UTF8.UTF_8_String) return Dir_Handle is separate;

   ----------
   -- Path --
   ----------

   function Path (Handle : Dir_Handle) return UTF8.UTF_8_String
   is
   begin
      return Handle.Path (1 .. Handle.Path_Last);
   end Path;

   function Path (Dir : Dir_Handle; Self : Dir_Entry) return UTF8.UTF_8_String
   is
   begin
      if Dir.Path (Dir.Path_Last) /= GNATCOLL.OS.Constants.Dir_Sep then
         return Dir.Path (1 .. Dir.Path_Last)
         & GNATCOLL.OS.Constants.Dir_Sep & Name (Self);
      else
         return Dir.Path (1 .. Dir.Path_Last) & Name (Self);
      end if;
   end Path;

   ------------------------------
   -- Raise_Exception_On_Error --
   ------------------------------

   procedure Raise_Exception_On_Error
      (Dir : Dir_Handle; Element : Dir_Entry; Error : Walk_Error; Msg : String)
   is
      pragma Unreferenced (Dir);
      pragma Unreferenced (Element);
   begin
      raise OS_Error with Msg;
   end Raise_Exception_On_Error;

   ----------
   -- Read --
   ----------

   function Read
      (Handle : Dir_Handle;
       Follow_Symlinks : Boolean := True)
      return Dir_Entry is separate;

   ----------
   -- Walk --
   ----------

   procedure Walk
      (Path            : UTF8.UTF_8_String;
       File_Handler    : Process_File;
       Dir_Handler     : Process_Directory := null;
       Max_Depth       : Positive          := 256;
       On_Error        : Process_Error     := Ignore;
       Follow_Symlinks : Boolean           := False)
   is
      Stack     : Dir_Handle_Stack := Allocate_Stack (Size => Max_Depth - 1);
      Cur_Dir   : Dir_Handle;
      Cur_File  : Dir_Entry;
      Enter_Dir : Boolean;
   begin

      Cur_Dir := Open (Path);

      loop
         Cur_File := Read (Cur_Dir, Follow_Symlinks => Follow_Symlinks);

         if End_Of_Iteration (Cur_File) then
            --  Directory has been explored. Either stop here or continue
            --  exploration of the parent directory.
            Close (Cur_Dir);

            exit when Is_Empty (Stack);

            Cur_Dir := Pop (Stack);
         elsif Is_Directory (Cur_File) then
            --  Process the entry
            if Dir_Handler /= null then
               Enter_Dir := Dir_Handler (Dir => Cur_Dir, Element => Cur_File);
            else
               Enter_Dir := True;
            end if;

            --  If the entry is a directory and we should explore it push the
            --  current directory and start entry exploration.
            if Stat.Is_Directory (Attributes (Cur_File))
               and then Enter_Dir
               and then not Is_Full (Stack)
            then
               Push (Stack, Cur_Dir);

               begin
                  Cur_Dir := Open
                     (Path => OS.Dir.Path (Dir => Cur_Dir, Self => Cur_File));
               exception
                  when E : OS_Error =>
                     Cur_Dir := Pop (Stack);
                     if On_Error /= Ignore then
                        On_Error
                           (Cur_Dir,
                            Cur_File,
                            OPEN_DIR_ERROR,
                            Ada.Exceptions.Exception_Message (E));
                     end if;
               end;
            end if;
         else
            if File_Handler /= null then
               File_Handler (Dir => Cur_Dir, Element => Cur_File);
            end if;
         end if;

      end loop;
      Deallocate_Stack (Stack);
   exception
      when others =>
         Deallocate_Stack (Stack);
   end Walk;

end GNATCOLL.OS.Dir;
