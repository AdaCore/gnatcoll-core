------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Directories;
with GNATCOLL.Config;
with Test_Assert;

function Test return Integer is

   package IO renames Ada.Text_IO;
   package Dir renames Ada.Directories;
   package A renames Test_Assert;
   package Cfg renames GNATCOLL.Config;

   Ini  : Cfg.INI_Parser;
   Ini2 : Cfg.INI_Parser;
   Ini3 : Cfg.INI_Parser;
   Sys  : Cfg.INI_Parser;
   Pool : Cfg.Config_Pool;

begin

   --  Open non existing file
   begin
      Ini.Open ("non_existent.ini");
      A.Assert (False, "Name_Error should be raised");
   exception
      when IO.Name_Error =>
         A.Assert (True, "Name_Error raised if ini file does not exist");
      when others =>
         A.Assert (False, "Name_Error should have been raised");
   end;

   --  Open a simple file
   IO.Put_Line ("Loading test1.ini");
   Ini.Open ("test1.ini");
   Pool.Fill (Ini);
   A.Assert (Pool.Get ("key1"), "value1", "check that key1=value1");
   A.Assert (Pool.Get ("key2", Section => "section1"), "value2",
             "check that key2=value2 in section1");
   A.Assert (Pool.Get ("key3", Section => "section1"), "value 3",
             "check that trailing spaces are ignored");
   A.Assert (Pool.Get ("section1.key2",
                       Section => Cfg.Section_From_Key),
             "value2",
             "check that sections1.key2=value2 (dot notation)");
   A.Assert (Pool.Get ("section1.key4"),
             "value4",
             "check that comments around key " &
             "declaration do not have impacts");

   A.Assert (Pool.Get ("section1.key[5"),
             "value5",
             "key can contain [");

   A.Assert (Pool.Get ("section1.[key6"),
             "value6",
             "key can start with [");

   --  Try to read invalid file (parser is very laxist and does not crash)
   Ini.Open ("test2.ini");
   Pool.Fill (Ini);
   A.Assert (Pool.Get ("key1"), "value1", "check if key1=value1 is preserved");
   A.Assert (Pool.Get ("invalid"), "", "check if invalid key exist");

   --  Check if we can override values.
   --  The test reuse the same INI_Parser instance and thus check that on
   --  call to Open parser state is reset
   IO.Put_Line ("Loading test3.ini (parser reuse)");
   Ini.Open ("test3.ini");
   Pool.Fill (Ini);
   A.Assert (Pool.Get ("key1"), "value1_2",
             "check if key1 is overwritten");
   A.Assert (Pool.Get ("key7"), "value7", "check addition of new key key7");
   A.Assert (Pool.Get ("section1.key2"),
             "value2", "check if section1.key2=value2 is preserved");

   --  Check if we can override values
   --  Use a new parser instance
   IO.Put_Line ("Loading test3.ini (new parser)");
   Ini2.Open ("test3.ini");
   Pool.Fill (Ini2);
   A.Assert (Pool.Get ("key1"), "value1_2",
             "check if key1 value is overwritten");
   A.Assert (Pool.Get ("key7"), "value7", "check addtion of new key key7");
   A.Assert (Pool.Get ("section1.key2"),
             "value2", "check if section1.key2=value2 is preserved");

   --  Test non string values
   A.Assert (Pool.Get_Boolean ("bool1"), "boolean value (true)");
   A.Assert (not Pool.Get_Boolean ("bool2"), "boolean value (false)");
   A.Assert (Pool.Get_Integer ("int1") = 1, "integer value");

   --  Test Config_Key creation
   declare
      CK1 : constant Cfg.Config_Key := Cfg.Create ("key1");
   begin
      A.Assert (CK1.Get (Pool), "value1_2", "basic config_key test");
   end;

   --  Check that introducing invalid gnatcoll templates strings do
   --  not crash the parser.
   IO.Put_Line ("Loading test4.ini (crashing the gnatcoll-template)");
   begin
      Ini3.Open ("test4.ini");
      Pool.Fill (Ini3);
      A.Assert (True, "parsing test4.ini");
   exception
      when others =>
         A.Assert (False, "parsing test4.ini");
   end;
   --  Check some corner cases involving leading whitespaces for example
   IO.Put_Line ("Loading test5.ini");
   begin
      Ini3.Open ("test5.ini");
      Pool.Fill (Ini3);
      A.Assert (True, "parsing test5.ini");
      A.Assert (Pool.Get ("#comment") /= "key",
                "ensure comment was parsed correctly");
      A.Assert (Pool.Get ("key5_1") /= "value2",
                "ensure section was parsed correctly");
      A.Assert (Pool.Get ("tion#key5_2", Section => "sec") /= "value3",
                "ensure # is not considered as a special character");
   exception
      when others =>
         A.Assert (False, "parsing test5.ini");
   end;

   --  Check that a line containing only [ will not crash the parser
   begin
      Ini3.Open ("test6.ini");
      Pool.Fill (Ini3);
      A.Assert (True, "parsing test6.ini");
   exception
      when others =>
         A.Assert (False, "parsing test6.ini");
   end;

   --  Check system_id related functionality
   IO.Put_Line ("Loading test_sys.ini");
   Sys.Set_System_Id (Dir.Current_Directory);
   Sys.Open ("test_as.ini");
   A.Assert (Sys.As_Absolute_File, Dir.Full_Name ("test1.ini"),
             "check if file name is correctly retrieved from system ID");
   Sys.Next;
   declare
      D : constant String := Sys.As_Absolute_Dir;
   begin
      A.Assert (D (D'First .. D'Last - 1), Dir.Full_Name (".."), --  strip sep
                "check if dir is correctly retrieved from system ID");
   end;

   --  Check the rest of As_* functionality
   Sys.Next;
   A.Assert (Sys.As_Boolean,
             "check if boolean is correctly retrieved");
   Sys.Next;
   A.Assert (Sys.As_Integer, 15,
             "check if integer is correctly retrieved");

   return A.Report;

end Test;
