------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

--  This package provides support for writing assertions in source code,
--  while getting proper error messages.
--
--  GNAT's "pragma Assert" does not display the expression that was tested,
--  so users end up writing the name of the variables in the message string
--  itself. This message is then associated with the Assertion_Error exception
--  that is raised. But exceptions limit the maximal length of such messages,
--  so again it is hard to have a meaningful message.
--
--  GNATCOLL.Traces provides an Assert procedure which tests the boolean
--  expression and displays a log message if it is false (and optionally
--  raises an exception). But the expression still needs to be manually
--  described in the message.
--
--  This package intends to ease the writing of the error messages, by
--  automatically generating messages that include the expression that
--  was tested, as well as the source location for that test.
--  The result of a failed assertion is left under control of an error
--  reporter, which you can customize in your application to raise
--  exceptions, log error messages, write to a GUI console, send to a
--  socket,...

with GNAT.Source_Info;    use GNAT.Source_Info;

package GNATCOLL.Asserts is

   --------------------
   -- Error_Reporter --
   --------------------

   type Error_Reporter is interface;
   --  This type is responsible for reacting when an assertion fails.

   procedure On_Assertion_Failed
      (Self     : Error_Reporter;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String) is abstract;
   --  Report the assertion error.
   --  The exact behavior depends on your application, and you will in
   --  general provide your own implementation of Error_Reporter unless
   --  one of the basic ones provided below works for you.
   --  Self will often be a global variable, and should therefore be
   --  stateless as much as possible (if your application is multi-threaded,
   --  it will be shared among the threads). For this reason, the parameter
   --  is set as "in".

   ------------------------
   -- Exception_Reporter --
   ------------------------

   type Exception_Reporter is new Error_Reporter with null record;
   overriding procedure On_Assertion_Failed
      (Self     : Exception_Reporter;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String);
   --  An error reporter that simply raises an Assertion_Error and sets a
   --  message for it. Due to limitations in the implementation of
   --  exceptions, the length of the message is limited, and thus Msg will
   --  be truncated as needed.

   ----------------
   -- Assertions --
   ----------------
   --  The following packages provides multiple ways to test values. These
   --  are implemented as generic packages so that they can also be applied
   --  to user-defined types.
   --  All the Assert_* procedures receives some common parameters:
   --      Msg : String
   --         Is an extra message that should be part of the error message,
   --         and can be used to provide additional explanations. For
   --         efficiency, this should be a static string as much as possible,
   --         since the compiler will always need to build that string to
   --         pass it to Assert_*, even if the latter does not use it in the
   --         end.
   --      Location, Entity
   --         These are used to compute statically the source location of
   --         the failed assertion. You should not provide actual values for
   --         these.

   generic
      Report : Error_Reporter'Class;
      --  The error reporter used by all Assert_* procedures below. This
      --  will in general be a global variable.

      Enabled : Boolean := True;
      --  If you set this to False, none of the Assertions will perform any
      --  work, and they might be omitted from the final binary altogether.

   package Asserts is

      procedure Assert_Failed
        (Msg      : String := "";
         Location : String := GNAT.Source_Info.Source_Location;
         Entity   : String := GNAT.Source_Info.Enclosing_Entity);
      --  Unconditionally report an error.

      generic
         type T (<>) is limited private;
         with function Image (V : T) return String is <>;
         with function "=" (Left, Right : T) return Boolean is <>;
      package Equals is

         procedure Assert_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         procedure Assert
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity)
           renames Assert_Equal;
         --  Left must be equal to Right, using the "=" operator.

         procedure Assert_Not_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  Left must be different from Right, using the "=" operator.

      end Equals;

      generic
         type T (<>) is limited private;
         with function Image (V : T) return String is <>;
         with function "=" (Left, Right : T) return Boolean is <>;
         with function "<" (Left, Right : T) return Boolean is <>;
      package Compare is

         procedure Assert_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         procedure Assert
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity)
           renames Assert_Equal;
         --  Left must be equal to Right, using the "=" operator.

         procedure Assert_Not_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  Left must be different from Right, using the "=" operator.

         procedure Assert_Less
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  asserts Left < Right

         procedure Assert_Less_Or_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  asserts Left <= Right

         procedure Assert_Greater_Or_Equal
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  asserts Left >= Right

         procedure Assert_Greater
           (Left, Right : T;
            Msg         : String := "";
            Location    : String := GNAT.Source_Info.Source_Location;
            Entity      : String := GNAT.Source_Info.Enclosing_Entity);
         --  asserts Left > Right

      end Compare;

   end Asserts;

end GNATCOLL.Asserts;
