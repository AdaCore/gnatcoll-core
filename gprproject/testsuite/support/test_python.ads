with Ada.Strings.UTF_Encoding;

package Test_Python is

   package UTF8 renames Ada.Strings.UTF_Encoding;

   function Python_Executable return UTF8.UTF_8_String;
   --  Return absolute location to the python executable used to launch the
   --  testsuite.

end Test_Python;
