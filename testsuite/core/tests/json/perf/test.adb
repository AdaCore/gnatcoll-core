with GNATCOLL.Buffer; use GNATCOLL.Buffer;
with Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with GNATCOLL.OS.FS;
with GNATCOLL.OS.Dir; use GNATCOLL.OS.Dir;
with GNATCOLL.OS.Stat;
with GNATCOLL.JSON;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Test_Assert;

function Test return Integer is
   package A renames Test_Assert;
   package FS renames GNATCOLL.OS.FS;
   package Stat renames GNATCOLL.OS.Stat;
   package Dir renames GNATCOLL.OS.Dir;
   package JSON renames GNATCOLL.JSON;
   use type JSON.JSON_Parser_Event_Kind;
   Start   : Ada.Calendar.Time;
   Elapsed : Duration;
   M       : Integer := 3;
   Data_Dir_Path     : constant String := ".";
   Data_Dir          : Dir_Handle;
   Data_Dir_Entry    : Dir_Entry;

   function Mbs (Msg : String; D : Duration; S : Integer) return String;

   function Mbs (Msg : String; D : Duration; S : Integer) return String is
      Result : String (1 .. 80) := (others => ' ');
   begin
      if D = 0.0 then
         Result (3 .. Msg'Length + 2) := Msg;
         Result (62 .. 65) := "FAIL";
         return Result;
      end if;

      declare
         DD : constant Duration := 1.0 * S / D / 1024 / 1024;
         Result : String (1 .. 80) := (others => ' ');
         Mbs : constant String := Integer'Image
            (Integer (DD)) & "MB/s";
      begin

         Result (3 .. Msg'Length + 2) := Msg;
         if DD >= 1.0 then
            if DD > 999.0 then
               Result (59 .. 58 + Mbs'Length) := Mbs;
            elsif DD > 99.0 then
               Result (60 .. 59 + Mbs'Length) := Mbs;
            elsif DD > 9.0 then
               Result (61 .. 60 + Mbs'Length) := Mbs;
            else
               Result (62 .. 61 + Mbs'Length) := Mbs;
            end if;
            return Result;
         else
            declare
               Kbs : constant String := Integer'Image
            (Integer (DD * 1024.0)) & "KB/s";
            begin
               if DD > 999.0 then
                  Result (59 .. 58 + Kbs'Length) := Kbs;
               elsif DD > 99.0 then
                  Result (60 .. 59 + Kbs'Length) := Kbs;
               elsif DD > 9.0 then
                  Result (61 .. 60 + Kbs'Length) := Kbs;
               else
                  Result (62 .. 61 + Kbs'Length) := Kbs;
               end if;

            end;
            return Result;
         end if;
      end;
   end Mbs;

begin
   Data_Dir := Open (Data_Dir_Path);
   loop
      Data_Dir_Entry := Read (Data_Dir);
      exit when End_Of_Iteration (Data_Dir_Entry);

      declare
         File_Name : constant String := Name (Data_Dir_Entry);
         File_Size : constant Long_Long_Integer :=
            Stat.Length (Dir.Attributes (Data_Dir_Entry));
      begin
         --  Consider only JSON files
         if File_Name'Length > 5 and then
            File_Name (File_Name'Last - 4 .. File_Name'Last) = ".json"
         then
            Ada.Text_IO.Put_Line ("**** Parsing " & File_Name & " ****");

            --  New parser + decode with new structure
            M := 1;
            Start := Ada.Calendar.Clock;
            loop
               declare
                  FR : Reader := Open (File_Name);
                  PR : JSON.JSON_Parser;
                  Result : JSON.JSON_Parser_Event;
               begin
                  loop
                     Result := JSON.Parse_Next (PR, FR);
                     exit when Result.Kind = JSON.DOC_END;
                  end loop;
                  --  Close (FR);
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                        ("    ERROR!" & Ada.Exceptions.Exception_Message (E));
                     Elapsed := 0.0;
                     exit;
               end;

               Elapsed := Ada.Calendar.Clock - Start;
               exit when Elapsed > 2.0;
               M := M + 1;
            end loop;

            Elapsed := Elapsed / M;
            Ada.Text_IO.Put_Line
               (Mbs ("new parser (parse only - no decoding of values)",
                     Elapsed,
                     Integer (File_Size)));

            M := 1;
            Start := Ada.Calendar.Clock;
            loop
               declare
                  FD : constant FS.File_Descriptor := FS.Open (File_Name);
                  Content : constant Unbounded_String := FS.Read (FD);
                  pragma Warnings (Off);
                  Result : JSON.Read_Result;
                  pragma Warnings (On);
               begin
                  Result := JSON.Read (To_String (Content));
                  FS.Close (FD);
               exception
                  when E : others =>
                     Ada.Text_IO.Put_Line
                        ("    ERROR!" &
                         Ada.Exceptions.Exception_Message (E));
                     Elapsed := 0.0;
                     exit;
               end;
               Elapsed := Ada.Calendar.Clock - Start;
               exit when Elapsed > 2.0;
               M := M + 1;
            end loop;

            Elapsed := Elapsed / M;

            Ada.Text_IO.Put_Line
               (Mbs ("new parser (parse + former decoder)",
                     Elapsed,
                     Integer (File_Size)));
         end if;
      end;
   end loop;
   return A.Report;
end Test;
