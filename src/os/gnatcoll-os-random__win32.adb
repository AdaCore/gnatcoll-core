--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1
--
with GNATCOLL.OS.Win32;
with GNATCOLL.Plugins;
with GNAT.Task_Lock;

package body GNATCOLL.OS.Random is

   package DLL renames GNATCOLL.Plugins;

   use type System.Address;
   use type DLL.Plugin;

   BCryptPrimitives_Path : constant String :=
      "\bcryptprimitives.dll";
   --  Name of the system library containing the ProcessPrng symbols.
   --  We prepend Windows SystemRoot to that value to ensure that the system
   --  version of that DLL is always picked.
  
   BCryptPrimitives : DLL.Plugin := DLL.No_Plugin;
   --  DLL handler that contains the ProcessPrng function

   ProcessPrng : System.Address := System.Null_Address;
   --  Address of the ProcessPrng function

   ------------------
   -- Random_Bytes --
   ------------------

   procedure Random_Bytes
      (Buffer : System.Address;
       Size   : Interfaces.C.size_t)
   is
   begin
      --   If ProcessPrng address is not set it means that bcryptprimitives.dll
      --   should be loaded. Lock all tasks to ensure that only one perform the
      --   load. Once loaded, other tasks can safely reuse the address.
      if ProcessPrng = System.Null_Address then
         GNAT.Task_Lock.Lock;
         if ProcessPrng = System.Null_Address then

            declare
               DLL_Path : String (1 .. 261);
               --  The new max path is 32K on windows but using the former
               --  is enough in that context.
               DLL_Last : Integer;

               function GetSystemDirectoryA
                  (Buffer : System.Address; Size : Integer) return Integer
               with Import        => True,
                    Convention    => Stdcall,
                    External_Name => "GetSystemDirectoryA";
            begin
               --  Find the location of the Windows system directory. When
               --  GNATCOLL.Plugins is updated to use Unicode APIs update
               --  this call to use the Unicode version.
               DLL_Last := GetSystemDirectoryA
                  (DLL_Path (1)'Address, DLL_Path'Length);
               if DLL_Last = 0 or else DLL_Last > DLL_Path'Length then
                  GNAT.Task_Lock.Unlock;
                  raise OS_Error with
                  "cannot initialize PRNG (cannot find system dir)";
               end if;

               --  Check bounds
               if DLL_Last + BCryptPrimitives_Path'Length > DLL_Path'Length
               then
                  GNAT.Task_Lock.Unlock;
                  raise OS_Error with
                  "cannot initialize PRNG (system dir too long)";
               end if;

               DLL_Path
                  (DLL_Last + 1 .. DLL_Last + BCryptPrimitives_Path'Length) :=
                     BCryptPrimitives_Path;
               DLL_Last := DLL_Last + BCryptPrimitives_Path'Length;

               --  Try to load the DLL
               BCryptPrimitives := DLL.Load (DLL_Path (1 .. DLL_Last));

               if BCryptPrimitives = DLL.No_Plugin then
                  GNAT.Task_Lock.Unlock;
                  raise OS_Error with
                  "cannot initialize PRNG (cannot load " &
                  DLL_Path (1 .. DLL_Last) & ": " &
                  DLL.Last_Error_Message & ")";
               end if;

               --  Finally found ProcessPrng function
               ProcessPrng := GNATCOLL.Plugins.Routine_Address
                  (BCryptPrimitives, "ProcessPrng");
               if ProcessPrng = System.Null_Address then
                  GNAT.Task_Lock.Unlock;
                  raise OS_Error with
                  "cannot initialize PRNG (cannot load ProcessPrng " &
                  "function: " &
                  DLL.Last_Error_Message & ")";
               end if;
            end;
         end if;
         GNAT.Task_Lock.Unlock;
      end if;

      --  Call ProcessPrng. Note that this the recommended call to do by
      --  various implementations such as Rust rand, BoringSSL, ...
      declare
         Status : GNATCOLL.OS.Win32.BOOL;
         pragma Unreferenced (Status);
         --  No need to check Status as it is always True as stated by
         --  Microsoft doc.

         function Internal
           (Buffer : System.Address;
            Size   : Interfaces.C.size_t)
           return GNATCOLL.OS.Win32.BOOL
         with Import        => True,
              Convention    => Stdcall,
              Address       => ProcessPrng;
      begin
         Status := Internal (Buffer => Buffer, Size => Size);
      end;
   end Random_Bytes;

end GNATCOLL.OS.Random;
