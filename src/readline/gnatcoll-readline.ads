------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--
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

--  This package provides an interface to the readline library.
--  This library provides support for interactive input from the user,
--  providing nice key bindings to edit the current line (including support
--  for backspace, move to beginning or end of line,...), as well as support
--  for completion (via the <tab> key) and history (via up and down keys).
--
--  If readline is not available on your system, this package can still be
--  called by your application and will default to using the services provided
--  by Ada.Text_IO (which do not provide all the above advantages, of course).
--
--  Readline is licensed under the Full GNU General Public License. If you
--  distribute a program using this package and the readline library, this
--  program must be free software.

package GNATCOLL.Readline is

   ----------------
   -- Completion --
   ----------------
   --  Readline supports completion of the current word when the user presses
   --  <tab>.
   --  The default is to complete on file names and user names, but you can
   --  configure your own subprogram to do the completion. Since there is a
   --  single instance of readline per application, it heavily uses global
   --  variables. As a result, your own completer should also use global
   --  variables if it needs to save information.

   type Possible_Completions_Array (<>) is private;
   type Possible_Completions is access Possible_Completions_Array;

   type Completion_Entry_Func is access function
      (Text : String; State : Integer) return String;

   function Completion_Matches
      (Text      : String;
       Generator : Completion_Entry_Func)
       return Possible_Completions;
   --  An adaptator for a completion generator.
   --  The goal of Generator is to return (one by one) each of the possible
   --  completions for Text. The first time it is called for a given word,
   --  State will be 0. It will then be increased by 1 for each entry to
   --  suggest.
   --  When there are no more possible completions, it should return the
   --  the empty string.

   type Completer_Function is access function
      (Full_Line   : String;
       Text        : String;
       Start, Last : Integer) return Possible_Completions;
   --  This function returns an array of strings, that should be generated
   --  by Completion_Matches above; it can also return null
   --  to use the default completion from readline (on filenames).
   --  Full_Line is the current full line, as read by readline so far.
   --  Text is the text to complete. Start is the position of Text within
   --  the Full_Line (0 means this is the first word on the line).
   --
   --  For instance:
   --     if Start = 0 then
   --         --  First word on the line ?
   --         return Completion_Matches (Text, Command_Completion'Access);
   --     else
   --         --  default filename completion
   --         return null;
   --     end if;

   -----------
   -- Setup --
   -----------

   procedure Initialize
      (Appname      : String := "";
       History_File : String := "";
       Completer    : Completer_Function := null);
   --  Initialize the support for readline.
   --  If that library is not available on the system, this operation does
   --  nothing, and is safe to call.
   --  Appname is the name of the application, and is used for conditional
   --  parsing of the ~/.inputrc file.
   --  History_File is the name of the file that should read to initialize the
   --  history (useful for saving the history across sessions).

   procedure Finalize (History_File : String := "");
   --  Finalize the support for gnatcoll.
   --  In particular, saves the current history to History_File if specified,
   --  so that the next session can restore that history.

   function Get_Line (Prompt : String := "") return String;
   --  Display Prompt, and reads one line of input.
   --  When readline is not available on the system, this uses the services
   --  from Ada.Text_IO (although of course there is no possibility to edit the
   --  command line or go back in the history).
   --  The exception Ada.Text_IO.End_Error is raised if the user presses
   --  control-D

   type Ctrl_C_Handler is access procedure;
   pragma Convention (C, Ctrl_C_Handler);
   --  Any parameterless library level procedure can be used as a handler.
   --  Ctrl_C_Handler should not propagate exceptions.
   --  Such a function is useful for saving the history when the application
   --  exits for instance.

   procedure Install_Ctrl_C_Handler (Handler : Ctrl_C_Handler);
   pragma Import (C, Install_Ctrl_C_Handler, "__gnat_install_int_handler");
   --  Set up Handler to be called if the operator hits Ctrl-C

private
   type Possible_Completions_Array is null record;
end GNATCOLL.Readline;
