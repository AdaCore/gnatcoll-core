------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021-2023, AdaCore                   --
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

with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with GNAT.Task_Lock;

package body GNATCOLL.OS.Process is

   function Internal_Spawn
     (Args     : Process_Types.Arguments;
      Cwd      : UTF8.UTF_8_String;
      Env      : Process_Types.Environ;
      Stdin    : FS.File_Descriptor;
      Stdout   : FS.File_Descriptor;
      Stderr   : FS.File_Descriptor;
      Priority : Priority_Class)
      return Process_Handle;
   --  Internal implementation of the process spawning. The function has the
   --  same signature as the public function Start.

   procedure List_To_Arguments
      (From : Argument_List;
       To   : in out Process_Types.Arguments);
   --  Transform an Argument_List into Process_Types.Arguments. To should be
   --  deallocated by the caller.

   procedure Dict_To_Environment
      (From    : Environment_Dict;
       Inherit : Boolean := False;
       To      : in out Process_Types.Environ);
   --  Transform an Environment_Dict into Process_Types.Environ. To should be
   --  deallocated by the caller.

   function Run
     (Args              : Process_Types.Arguments;
      Cwd               : UTF8.UTF_8_String  := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Common function used by several variants of Run

   -------------------------
   -- Dict_To_Environment --
   -------------------------

   procedure Dict_To_Environment
      (From    : Environment_Dict;
       Inherit : Boolean := False;
       To      : in out Process_Types.Environ)
   is
      procedure Fill_Env (Position : Env_Dicts.Cursor);
      procedure Fill_Env (Position : Env_Dicts.Cursor) is
      begin
         Process_Types.Set_Variable (To,  Key (Position), Element (Position));
      end Fill_Env;
   begin
      if Inherit then
         Process_Types.Import (To);
      end if;
      Iterate (From, Fill_Env'Unrestricted_Access);
   end Dict_To_Environment;

   --------------------------
   -- Equivalent_Variables --
   --------------------------

   function Equivalent_Variables
      (Left, Right : UTF8.UTF_8_String) return Boolean
   is
   begin
      if Process_Types.Case_Sensitive_Env_Var_Names then
         return Left = Right;
      else
         return Ada.Strings.Equal_Case_Insensitive (Left, Right);
      end if;
   end Equivalent_Variables;

   --------------------
   -- Internal_Spawn --
   --------------------

   function Internal_Spawn
     (Args     : Process_Types.Arguments;
      Cwd      : UTF8.UTF_8_String;
      Env      : Process_Types.Environ;
      Stdin    : FS.File_Descriptor;
      Stdout   : FS.File_Descriptor;
      Stderr   : FS.File_Descriptor;
      Priority : Priority_Class)
      return Process_Handle is separate;

   -----------------------
   -- List_To_Arguments --
   -----------------------

   procedure List_To_Arguments
      (From : Argument_List;
       To   : in out Process_Types.Arguments)
   is
      procedure Fill_Args (Position : Arg_Lists.Cursor);

      procedure Fill_Args (Position : Arg_Lists.Cursor) is
      begin
         Process_Types.Add_Argument (To, Element (Position));
      end Fill_Args;
   begin
      Iterate (From, Fill_Args'Unrestricted_Access);
   end List_To_Arguments;

   ---------
   -- Run --
   ---------

   function Run
     (Args              : Process_Types.Arguments;
      Cwd               : UTF8.UTF_8_String  := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Env : Process_Types.Environ;
   begin
      Process_Types.Inherit (Env);
      return Run
         (Args              => Args,
          Env               => Env,
          Cwd               => Cwd,
          Stdin             => Stdin,
          Stderr            => Stderr,
          Priority          => Priority,
          Universal_Newline => Universal_Newline,
          Strip             => Strip,
          Status            => Status);
   end Run;

   function Run
     (Args              : Argument_List;
      Cwd               : String             := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Final_Args : Process_Types.Arguments;
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List_To_Arguments (Args, Final_Args);
      Result := Run (Args => Final_Args,
                     Cwd  => Cwd,
                     Stdin => Stdin,
                     Stderr => Stderr,
                     Priority => Priority,
                     Universal_Newline => Universal_Newline,
                     Strip => Strip,
                     Status => Status);
      Process_Types.Deallocate (Final_Args);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         raise;
   end Run;

   function Run
     (Args              : Argument_List;
      Env               : Environment_Dict;
      Cwd               : String             := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Inherit_Env       : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List_To_Arguments (Args, Final_Args);
      Dict_To_Environment (Env, Inherit_Env, Final_Env);
      Result := Run (Args => Final_Args,
                     Env  => Final_Env,
                     Cwd  => Cwd,
                     Stdin => Stdin,
                     Stderr => Stderr,
                     Priority => Priority,
                     Universal_Newline => Universal_Newline,
                     Strip => Strip,
                     Status => Status);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         Process_Types.Deallocate (Final_Env);
         raise;
   end Run;

   function Run
     (Args              : Process_Types.Arguments;
      Env               : Process_Types.Environ;
      Cwd               : String             := "";
      Stdin             : FS.File_Descriptor := FS.Standin;
      Stderr            : FS.File_Descriptor := FS.Standerr;
      Priority          : Priority_Class     := INHERIT;
      Universal_Newline : Boolean            := False;
      Strip             : Boolean            := False;
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;

      Pipe_Read, Pipe_Write : OS.FS.File_Descriptor;
      Output                : Unbounded_String;
      Buffer                : String (1 .. 4096);
      Pid                   : Process_Handle;
      N                     : Integer;
   begin
      --  Allocate a PIPE to retrieve the output
      FS.Open_Pipe (Pipe_Read, Pipe_Write);

      --  Start the process.
      Pid := Start
        (Args, Env, Cwd, Stdin, Pipe_Write, Stderr, Priority);

      --  Be sure to close Pipe_Write otherwise following reads on Pipe_Read
      --  will block even if the child process ends.
      FS.Close (Pipe_Write);

      --  Read output and filter out if necessary CR characters. Note that
      --  call to Read is blocking.
      if Universal_Newline then
         declare
            Prev_Chunk_Last_Is_CR : Boolean := False;
         begin
            loop
               N := FS.Read (Pipe_Read, Buffer);
               exit when N <= 0;

               --  Check if last chunk ended with a CR and first char in this
               --  chunk is a LF
               if Prev_Chunk_Last_Is_CR and then Buffer (1) /= ASCII.LF then
                  Append (Output, ASCII.CR);
               end if;

               for Index in 1 .. N - 1 loop
                  if Buffer (Index) /= ASCII.CR
                     or else Buffer (Index + 1) /= ASCII.LF
                  then
                     Append (Output, Buffer (Index));
                  end if;
               end loop;

               if Buffer (N) = ASCII.CR then
                  Prev_Chunk_Last_Is_CR := True;
               else
                  Prev_Chunk_Last_Is_CR := False;
                  Append (Output, Buffer (N));
               end if;
            end loop;

            --  Trailing CR
            if Prev_Chunk_Last_Is_CR then
               Append (Output, ASCII.CR);
            end if;
         end;
      else
         loop
            N := FS.Read (Pipe_Read, Buffer);
            exit when N <= 0;
            Append (Output, Buffer (1 .. N));
         end loop;
      end if;

      FS.Close (Pipe_Read);

      --  Wait here is blocking but we reached EOF on Pipe_Read which in
      --  practice mean that the child process has ended. As a consequence
      --  Wait will return immediately with the process status.
      Status := Wait (Pid);

      if Strip then
         --  Strip leading and trailing white chars from the output.
         declare
            First : Integer := 1;
            Last  : Integer := Length (Output);
         begin
            for J in reverse 1 .. Length (Output) loop
               case Element (Output, J) is
                  when ASCII.LF | ASCII.CR | ASCII.HT | ' ' =>
                     Last := Last - 1;
                  when others => exit;
               end case;
            end loop;

            for J in 1 .. Last loop
               case Element (Output, J) is
                  when ASCII.LF | ASCII.CR | ASCII.HT | ' ' =>
                     First := First + 1;
                  when others => exit;
               end case;
            end loop;

            return Unbounded_Slice (Output, Low => First, High => Last);
         end;
      else
         return Output;
      end if;
   end Run;

   ---------
   -- Run --
   ---------

   function Run
      (Args     : Argument_List;
       Cwd      : UTF8.UTF_8_String := "";
       Stdin    : FS.File_Descriptor := FS.Standin;
       Stdout   : FS.File_Descriptor := FS.Standout;
       Stderr   : FS.File_Descriptor := FS.Standerr;
       Priority : Priority_Class     := INHERIT)
      return Integer
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Integer;
   begin
      Process_Types.Inherit (Final_Env);
      List_To_Arguments (Args, Final_Args);
      Result := Run
         (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr, Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         Process_Types.Deallocate (Final_Env);
         raise;
   end Run;

   function Run
      (Args        : Argument_List;
       Env         : Environment_Dict;
       Cwd         : UTF8.UTF_8_String  := "";
       Stdin       : FS.File_Descriptor := FS.Standin;
       Stdout      : FS.File_Descriptor := FS.Standout;
       Stderr      : FS.File_Descriptor := FS.Standerr;
       Priority    : Priority_Class     := INHERIT;
       Inherit_Env : Boolean            := False)
      return Integer
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Integer;
   begin
      List_To_Arguments (Args, Final_Args);
      Dict_To_Environment (Env, Inherit_Env, Final_Env);
      Result := Run (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr,
                     Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         Process_Types.Deallocate (Final_Env);
         raise;
   end Run;

   function Run
     (Args     : Process_Types.Arguments;
      Env      : Process_Types.Environ;
      Cwd      : UTF8.UTF_8_String     := "";
      Stdin    : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout   : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr   : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority : Priority_Class        := INHERIT)
      return Integer
   is
      Pid    : Process_Handle;
      Result : Integer;
   begin
      Pid := Start (Args, Env, Cwd, Stdin, Stdout, Stderr, Priority);
      Result := Wait (Pid);
      return Result;
   end Run;

   -----------
   -- Start --
   -----------

   function Start
      (Args        : Argument_List;
       Env         : Environment_Dict;
       Cwd         : UTF8.UTF_8_String  := "";
       Stdin       : FS.File_Descriptor := FS.Standin;
       Stdout      : FS.File_Descriptor := FS.Standout;
       Stderr      : FS.File_Descriptor := FS.Standerr;
       Priority    : Priority_Class     := INHERIT;
       Inherit_Env : Boolean            := False)
      return Process_Handle
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Process_Handle;

   begin
      List_To_Arguments (Args, Final_Args);
      Dict_To_Environment (Env, Inherit_Env, Final_Env);
      Result := Start (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr,
                       Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         Process_Types.Deallocate (Final_Env);
         raise;
   end Start;

   function Start
      (Args     : Argument_List;
       Cwd      : UTF8.UTF_8_String := "";
       Stdin    : FS.File_Descriptor := FS.Standin;
       Stdout   : FS.File_Descriptor := FS.Standout;
       Stderr   : FS.File_Descriptor := FS.Standerr;
       Priority : Priority_Class     := INHERIT)
      return Process_Handle
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Process_Handle;
   begin
      List_To_Arguments (Args, Final_Args);
      Process_Types.Inherit (Final_Env);
      Result := Start
         (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr, Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;

   exception
      when others =>
         Process_Types.Deallocate (Final_Args);
         Process_Types.Deallocate (Final_Env);
         raise;
   end Start;

   function Start
     (Args     : Process_Types.Arguments;
      Env      : Process_Types.Environ;
      Cwd      : UTF8.UTF_8_String     := "";
      Stdin    : OS.FS.File_Descriptor := OS.FS.Standin;
      Stdout   : OS.FS.File_Descriptor := OS.FS.Standout;
      Stderr   : OS.FS.File_Descriptor := OS.FS.Standerr;
      Priority : Priority_Class        := INHERIT)
      return Process_Handle
   is
      Result      : Process_Handle;

      --  By default final file descriptors are the one passed by the user
      Real_Stdin   : OS.FS.File_Descriptor := Stdin;
      Real_Stdout  : OS.FS.File_Descriptor := Stdout;
      Real_Stderr  : OS.FS.File_Descriptor := Stderr;
      Close_Stdin  : Boolean := False;
      Close_Stdout : Boolean := False;
      Close_Stderr : Boolean := False;

      use type OS.FS.File_Descriptor;
   begin

      --  Handle special cases for file descriptors
      --  First ensure all file descriptors are valid
      if Stdout = OS.FS.Invalid_FD then
         raise OS_Error with "invalid fd for process stdout";
      elsif Stdout = OS.FS.Null_FD then
         Real_Stdout := OS.FS.Open (OS.FS.Null_File, OS.FS.Write_Mode);
         Close_Stdout := True;
      elsif Stdout = OS.FS.To_Stdout then
         raise OS_Error with "cannot redirect stdout to stdout";
      end if;

      if Stdin = OS.FS.Invalid_FD then
         raise OS_Error with "invalid fd for process stdin";
      elsif Stdin = OS.FS.Null_FD then
         Real_Stdin := OS.FS.Open (OS.FS.Null_File, OS.FS.Read_Mode);
         Close_Stdin := True;
      elsif Stdin = OS.FS.To_Stdout then
         raise OS_Error with "cannot redirect stdin to stdout";
      end if;

      if Stderr = OS.FS.Invalid_FD then
         raise OS_Error with "invalid fd for process stderr";
      elsif Stderr = OS.FS.Null_FD then
         Real_Stderr := OS.FS.Open (OS.FS.Null_File, OS.FS.Write_Mode);
         Close_Stderr := True;
      elsif Stderr = OS.FS.To_Stdout then
         Real_Stderr := Real_Stdout;
      end if;

      --  A global lock is necessary here in order to avoid unwanted leaks
      --  of the file descriptors that are associated with stdin, stdout
      --  and stderr if other tasks also do process spawning at the same time.
      GNAT.Task_Lock.Lock;

      begin
         FS.Set_Close_On_Exec (Real_Stdin, False);
         FS.Set_Close_On_Exec (Real_Stdout, False);
         FS.Set_Close_On_Exec (Real_Stderr, False);

         Result := Internal_Spawn
           (Args,
            Cwd,
            Env,
            Real_Stdin, Real_Stdout, Real_Stderr,
            Priority);

         FS.Set_Close_On_Exec (Real_Stdin, True);
         FS.Set_Close_On_Exec (Real_Stdout, True);
         FS.Set_Close_On_Exec (Real_Stderr, True);

         GNAT.Task_Lock.Unlock;
      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end;

      if Close_Stdout then
         OS.FS.Close (Real_Stdout);
      end if;

      if Close_Stderr then
         OS.FS.Close (Real_Stderr);
      end if;

      if Close_Stdin then
         OS.FS.Close (Real_Stdin);
      end if;

      return Result;
   end Start;

   -----------
   -- State --
   -----------

   function State (H : Process_Handle) return Process_State is separate;

   ------------------------
   -- Variable_Name_Hash --
   ------------------------

   function Variable_Name_Hash (Key : UTF8.UTF_8_String)
      return Ada.Containers.Hash_Type
   is
   begin
      if Process_Types.Case_Sensitive_Env_Var_Names then
         return Ada.Strings.Hash (Key);
      else
         return Ada.Strings.Hash_Case_Insensitive (Key);
      end if;
   end Variable_Name_Hash;

   ----------
   -- Wait --
   ----------

   function Wait (H : Process_Handle) return Integer is separate;

   ------------------------
   -- Wait_For_Processes --
   ------------------------

   function Wait_For_Processes
     (Processes : Process_Array;
      Timeout   : Duration := INFINITE_TIMEOUT)
      return Integer is separate;

   function Wait_For_Processes
     (Processes : Process_Array;
      Timeout   : Duration := INFINITE_TIMEOUT)
      return Process_Handle
   is
      Result : constant Integer := Wait_For_Processes (Processes, Timeout);
   begin
      if Result < 0 then
         return Invalid_Handle;
      else
         return Processes (Result);
      end if;
   end Wait_For_Processes;

end GNATCOLL.OS.Process;
