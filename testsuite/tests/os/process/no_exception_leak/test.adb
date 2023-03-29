with GNAT.IO;
with Ada.Strings.Unbounded;
with Test_Assert;
with GNATCOLL.OS.Process;

with Memory_Statistics;

function Test return Integer
is
   package Mem renames Memory_Statistics;
   package Proc renames GNATCOLL.OS.Process;
   package A renames Test_Assert;
   package IO renames GNAT.IO;

   use all type Mem.Byte_Count;
   use all type Proc.Process_Handle;
   Mem_Mark1, Mem_Mark2 : Mem.Byte_Count;
begin
   Mem.Configure (Activate_Monitor => True);

   declare
      Output : Ada.Strings.Unbounded.Unbounded_String;
      Status : Integer;
      Handle : Proc.Process_Handle;
      Args   : Proc.Argument_List;
      Env    : Proc.Environment_Dict;
   begin
      Mem_Mark1 := Mem.Get_Ada_Allocations.Current;
      Env.Insert ("PATH", ".");
      Args.Append ("executable_not_found");
      Args.Append ("arg1");
      Args.Append ("arg2");
      Mem_Mark2 := Mem.Get_Ada_Allocations.Current;

      A.Assert
         (Mem_Mark1 /= Mem_Mark2,
          "allocation of args and env should use memory on heap");

      begin
         Output := Proc.Run (Args, Env, Status => Status);
         IO.Put_Line ("output: " & Ada.Strings.Unbounded.To_String (Output));
         IO.Put_Line ("status: " & Status'Img);
      exception
         when others => null;
      end;

      begin
         Output := GNATCOLL.OS.Process.Run (Args, Status => Status);
         IO.Put_Line ("output: " & Ada.Strings.Unbounded.To_String (Output));
         IO.Put_Line ("status: " & Status'Img);
      exception
         when others => null;
      end;

      begin
         Status := GNATCOLL.OS.Process.Run (Args, Env);
         IO.Put_Line ("status: " & Status'Img);
      exception
         when others => null;
      end;

      begin
         Status := GNATCOLL.OS.Process.Run (Args);
         IO.Put_Line ("status: " & Status'Img);
      exception
         when others => null;
      end;

      begin
         Handle := GNATCOLL.OS.Process.Start (Args);
         if Handle /= Proc.Invalid_Handle then
            Status := Wait (Handle);
            IO.Put_Line ("status: " & Status'Img);
         end if;
      exception
         when others => null;
      end;

      begin
         Handle := GNATCOLL.OS.Process.Start (Args, Env);
         if Handle /= Proc.Invalid_Handle then
            Status := Wait (Handle);
            IO.Put_Line ("status: " & Status'Img);
         end if;
      exception
         when others => null;
      end;

      Mem_Mark1 :=  Mem.Get_Ada_Allocations.Current;

   end;

   IO.Put_Line ("Before :" & Mem_Mark2'Image & " After :" & Mem_Mark1'Image);
   A.Assert (Mem_Mark1 = Mem_Mark2, "memory leak detected");
   return A.Report;
end Test;
