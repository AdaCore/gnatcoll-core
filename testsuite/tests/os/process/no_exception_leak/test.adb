with Ada.Strings.Unbounded;
with Ada.Text_IO;  use Ada.Text_IO;

with GNATCOLL.OS.Process;

with Memory_Statistics; use Memory_Statistics;

procedure test is
   Before : Memory_Statistics.Byte_Count;
   After  : Memory_Statistics.Byte_Count;
begin
   Memory_Statistics.Configure (Activate_Monitor => True);
   declare
      Output : Ada.Strings.Unbounded.Unbounded_String;
      Status : Integer;
      Handle : GNATCOLL.OS.Process.Process_Handle;
      Args   : GNATCOLL.OS.Process.Argument_List;
      Env    : GNATCOLL.OS.Process.Environment_Dict;
   begin
      Before := Memory_Statistics.Get_Ada_Allocations.Current;

      Env.Insert ("PATH", ".");

      Args.Append ("executable_not_found");
      Args.Append ("arg1");
      Args.Append ("arg2");

      After := Memory_Statistics.Get_Ada_Allocations.Current;

      if After = Before then
         Put_Line ("Memory allocations not detected.");
      end if;

      Before := Memory_Statistics.Get_Ada_Allocations.Current;

      begin
         Output := GNATCOLL.OS.Process.Run (Args, Env, Status => Status);
         Put_Line
           ("Unexpected Output := GNATCOLL.OS.Process.Run (Args, Env);");
      exception
         when others => null;
      end;

      begin
         Output := GNATCOLL.OS.Process.Run (Args, Status => Status);
         Put_Line
           ("Unexpected Output := GNATCOLL.OS.Process.Run (Args);");
      exception
         when others => null;
      end;

      begin
         Status := GNATCOLL.OS.Process.Run (Args, Env);
         Put_Line
           ("Unexpected Status := GNATCOLL.OS.Process.Run (Args, Env);");
      exception
         when others => null;
      end;

      begin
         Status := GNATCOLL.OS.Process.Run (Args);
         Put_Line
           ("Unexpected Status := GNATCOLL.OS.Process.Run (Args);");
      exception
         when others => null;
      end;

      begin
         Handle := GNATCOLL.OS.Process.Start (Args);
         Put_Line
           ("Unexpected Handle := GNATCOLL.OS.Process.Start (Args);");
      exception
         when others => null;
      end;

      begin
         Handle := GNATCOLL.OS.Process.Start (Args, Env);
         Put_Line
           ("Unexpected Handle := GNATCOLL.OS.Process.Start (Args, Env);");
      exception
         when others => null;
      end;

      After :=  Memory_Statistics.Get_Ada_Allocations.Current;

   end;

   if After /= Before then
      Put_Line
        ("Memory leak: Before :" & Before'Image & " After :" & After'Image
        );
   end if;

   Put_Line ("Done.");
end test;
