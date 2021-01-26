------------------------------------------------------------------------------
--                              G N A T C O L L                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Characters.Handling;
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
      (From : Environment_Dict;
       To   : in out Process_Types.Environ);
   --  Transform an Environment_Dict into Process_Types.Environ. To should be
   --  deallocated by the caller.

   -------------------------
   -- Dict_To_Environment --
   -------------------------

   procedure Dict_To_Environment
      (From : Environment_Dict;
       To   : in out Process_Types.Environ)
   is
      procedure Fill_Env (Position : Env_Dicts.Cursor);
      procedure Fill_Env (Position : Env_Dicts.Cursor) is
      begin
         Process_Types.Set_Variable (To,  Key (Position), Element (Position));
      end Fill_Env;
   begin
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
         return Ada.Characters.Handling.To_Lower (Left) =
            Ada.Characters.Handling.To_Lower (Left);
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
      Status            : out Integer)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      Final_Args : Process_Types.Arguments;
      Final_Env  : Process_Types.Environ;
      Result     : Ada.Strings.Unbounded.Unbounded_String;
   begin
      List_To_Arguments (Args, Final_Args);
      Dict_To_Environment (Env, Final_Env);
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
      use type FS.File_Descriptor;
      use Ada.Strings.Unbounded;

      Pipe_Read, Pipe_Write : OS.FS.File_Descriptor;
      Output                : Unbounded_String;
      Real_Stderr_Fd        : OS.FS.File_Descriptor := Stderr;
      Buffer                : String (1 .. 4096);
      Pid                   : Process_Handle;
      N                     : Integer;
   begin
      --  Allocate a PIPE to retrieve the output
      FS.Open_Pipe (Pipe_Read, Pipe_Write);

      if Stderr = FS.To_Stdout then
         Real_Stderr_Fd := Pipe_Write;
      end if;

      --  Start the process.
      Pid := Start
        (Args, Env, Cwd, Stdin, Pipe_Write, Real_Stderr_Fd, Priority);

      --  Be sure to close Pipe_Write otherwise following reads on Pipe_Read
      --  will block even if the child process ends.
      FS.Close (Pipe_Write);

      --  Read output and filter out if necessary CR characters. Note that
      --  call to Read is blocking.
      if Universal_Newline then
         loop
            N := FS.Read (Pipe_Read, Buffer);
            exit when N <= 0;

            for Index in 1 .. N - 1 loop
               if not (Buffer (Index) = ASCII.CR
                       and then Buffer (Index + 1) = ASCII.LF)
               then
                  Append (Output, Buffer (Index));
               end if;
            end loop;
         end loop;
      else
         loop
            N := FS.Read (Pipe_Read, Buffer);
            exit when N <= 0;
            Append (Output, Buffer (1 .. N));
         end loop;
      end if;

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
   end Run;

   function Run
      (Args     : Argument_List;
       Env      : Environment_Dict;
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
      List_To_Arguments (Args, Final_Args);
      Dict_To_Environment (Env, Final_Env);
      Result := Run (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr,
                     Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;
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
      (Args     : Argument_List;
       Env      : Environment_Dict;
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
      Dict_To_Environment (Env, Final_Env);
      Result := Start (Final_Args, Final_Env, Cwd, Stdin, Stdout, Stderr,
                       Priority);
      Process_Types.Deallocate (Final_Args);
      Process_Types.Deallocate (Final_Env);
      return Result;
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
      Result  : Process_Handle;
   begin
      GNAT.Task_Lock.Lock;

      FS.Set_Close_On_Exec (Stdin, False);
      FS.Set_Close_On_Exec (Stdout, False);
      FS.Set_Close_On_Exec (Stderr, False);

      Result := Internal_Spawn
        (Args,
         Cwd,
         Env,
         Stdin, Stdout, Stderr,
         Priority);

      FS.Set_Close_On_Exec (Stdin, True);
      FS.Set_Close_On_Exec (Stdout, True);
      FS.Set_Close_On_Exec (Stderr, True);

      GNAT.Task_Lock.Unlock;
      return Result;
   end Start;

   ----------
   -- Wait --
   ----------

   function Wait (H : Process_Handle) return Integer is separate;

end GNATCOLL.OS.Process;
