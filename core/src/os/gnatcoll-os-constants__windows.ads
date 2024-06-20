------------------------------------------------------------------------------
--                  G N A T C O L L . O S . C O N S T A N T S               --
--                                                                          --
--                        Copyright (C) 2017-2021, AdaCore                  --
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

--  This is the Windows version of GNATCOLL.OS.Constants package

package GNATCOLL.OS.Constants is

   -----------------------
   -- OS identification --
   -----------------------

   OS : constant OS_Type := Windows;

   -------------------------------------
   --  File system specific constants --
   -------------------------------------

   Dir_Sep : constant Character := '\';
   --  The default character that separates qualified filename components

   Dir_Seps : constant String := "\/";
   --  The list of allowed characters that separates qualified filename
   --  components

   Path_Sep : constant Character := ';';
   --  The character that separates paths in a path list

   Exe_Ext : constant String := ".exe";
   --  Executable image extension

   Default_Casing_Policy : constant Filename_Casing_Policy := Preserving;
   --  Default casing policy chosen by the OS

   ------------------------------------------------
   --  Dynamic link libraries specific constants --
   ------------------------------------------------

   DLL_Name : constant String := "DLL";
   --  The OS-specific term to refer to a DLL

   DLL_Search_Path_Var : constant String := "PATH";
   --  Environment variable used to search for DLLs

   DLL_Ext : constant String := ".dll";
   --  DLL image extension

end GNATCOLL.OS.Constants;
