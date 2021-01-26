with Ada.Environment_Variables;

package body Test_Python is

   package Env renames Ada.Environment_Variables;

   -----------------------
   -- Python_Executable --
   -----------------------

   function Python_Executable return UTF8.UTF_8_String is
   begin
      return Env.Value ("PYTHON_EXEC_PATH");
   end Python_Executable;

end Test_Python;
