------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2010-2013, AdaCore                     --
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

--  This package provides a general handling mechanism for config files.
--  Any format of config files can be supported. The default implementation
--  provides support for Windows-like .ini files, that is:
--
--  # Comment
--  [Section]
--  key1 = value1
--  key2 = value2
--
--  As a special case, some strings are automatically substituted in the values
--  - HOME:  home directory for the user, as configured in the parser
--
--  This package is build through several layers of tagged objects:
--  - the first layer provides the parsing of config files, and through a
--    callback returns the (key, value) pairs to the application
--  - the second layer provides a pool of these pairs, ie provides the storage
--    on top of the first layer. Queries are done through strings
--  - a third layer makes the keys as real objects, so that you build the key
--    once, and then query the value from it directly. This is mostly syntactic
--    sugar, although it helps ensure that you are always reading existing keys
--    (if your coding convention forces users to use these keys).

pragma Ada_05;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with Ada.Strings.Unbounded;
with GNATCOLL.VFS;

package GNATCOLL.Config is

   --------------------------
   -- Parsing config files --
   --------------------------

   type Config_Parser is abstract tagged private;
   --  Abstract type for all config streams (files, in-memory,...), with any
   --  format. Concrete types below will provide the actual implementation.

   --  Typical usage looks like:

   --     declare
   --        C : File_Config_Parser;
   --     begin
   --        Open (C, "filename.txt");
   --        while not C.At_End loop
   --           Put_Line (C.Key & " = " & C.Value);
   --           C.Next;
   --        end loop;
   --     end;

   function At_End (Self : Config_Parser) return Boolean is abstract;
   --  Whether the config parsing is at the end

   procedure Next (Self : in out Config_Parser) is abstract;
   --  Move to the next (key, value) in the configuration. Before that call,
   --  the parser is left on the first value in the configuration.

   function Section (Self : Config_Parser) return String is abstract;
   function Key (Self : Config_Parser) return String is abstract;
   function Value (Self : Config_Parser) return String is abstract;
   --  Return the current (section, key, value);

   procedure Set_System_Id (Self : in out Config_Parser; System_ID : String);
   --  Sets the system ID for the config.
   --  If the config is found in a file, this should be the absolute path name
   --  to that file. This will generally be called automatically when opening
   --  the file.
   --  This system id is used to resolve absolute file names.

   function As_Integer       (Self : Config_Parser) return Integer;
   function As_Boolean       (Self : Config_Parser) return Boolean;
   function As_Absolute_File (Self : Config_Parser) return String;
   function As_Absolute_Dir  (Self : Config_Parser) return String;
   --  Assuming the current value is a file or directory, converts it to an
   --  absolute name, where relative paths are resolved relative to the
   --  config's system_id.
   --  These will raise Constraint_Error if used on non-matching values.

   -----------------
   -- File config --
   -----------------

   type File_Config_Parser is abstract new Config_Parser with private;
   --  A special implementation for config streams based on actual files

   procedure Open (Self : in out File_Config_Parser; Filename : String);
   --  Open a file

   overriding function At_End (Self : File_Config_Parser) return Boolean;

   ---------------
   -- INI files --
   ---------------

   type INI_Parser is new File_Config_Parser with private;
   --  a special parser for Windows' .ini files

   procedure Configure
     (Self             : in out INI_Parser;
      Comment_Start    : String := "#";
      Handles_Sections : Boolean := True;
      Home             : String := "");
   --  "Home" is the substitution pattern for "HOME" in the values. If
   --  unspecified, it is computed automatically.

   overriding procedure Open (Self : in out INI_Parser; Filename : String);
   overriding procedure Next (Self : in out INI_Parser);
   overriding function Section (Self : INI_Parser) return String;
   overriding function Key (Self : INI_Parser) return String;
   overriding function Value (Self : INI_Parser) return String;

   -------------------
   -- Resource pool --
   -------------------

   type Config_Pool is tagged private;
   --  This type provides storage for a config file

   procedure Set_System_Id (Self : in out Config_Pool; System_ID : String);
   --  Set the absolute name used to resolve file names in Get_File

   procedure Fill
     (Self   : in out Config_Pool;
      Config : in out Config_Parser'Class);
   --  Load all keys from Config, and store the (key, value) pairs in Self.
   --  Multiple files can be merged into the same pool.
   --  Set_System_Id is automatically called, thus file names will be resolved
   --  relative to the last Config loaded in the pool.

   Section_From_Key : constant String;
   --  Indicates that the section should in fact be read from the key (as
   --  opposed to being specified separately). In this case, the key is split
   --  at the first "." (if there is none, the section name is empty).
   --  For instance: "section1.key1" or "section1.key2".
   --
   --  It is often more convenient to specify the section that way, in exchange
   --  for a small performance penalty and a possible ambiguity if the key
   --  itself contains a ".", which is not recommended.

   Whole_Value : constant Natural := 0;

   function Get (Self    : Config_Pool;
                 Key     : String;
                 Section : String := Section_From_Key;
                 Index   : Natural := Whole_Value) return String;
   --  Return the value associated with Key.
   --  Index is used for comma-separated lists of values, and will retrieve
   --  one of the specific elements of the list. The whole value (no splitting)
   --  is returned if Index is Whole_Value. The empty string is returned if
   --  there is no such item in the list

   function Get_Integer
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return Integer;

   function Get_Boolean
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return Boolean;

   function Get_File
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Index   : Natural := Whole_Value) return String;
   --  Same as above, but returns an absolute filename. Relative paths are
   --  resolved relative to the config location where Key was declared.

   function To_File
     (Self    : Config_Pool;
      Key     : String;
      Section : String := Section_From_Key;
      Value   : String) return GNATCOLL.VFS.Virtual_File;
   --  Converts value to a file. It is relative to the location of the config
   --  file that provided Key. This is similar to calling Get_File directly,
   --  but is useful in contexts where you need to first manipulate the value
   --  read from the config and then interpret it as a file.

   procedure Set (Self : in out Config_Pool; Section, Key, Value : String);
   --  Override a specific key

   --------------------------------
   -- Resource pool, static keys --
   --------------------------------

   type Config_Key is tagged private;

   function Create (Key : String; Section : String := "") return Config_Key;
   --  Create a new config key

   function Get
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return String;
   function Get_Integer
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return Integer;
   function Get_Boolean
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return Boolean;
   function Get_File
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Index : Natural := Whole_Value) return String;
   function To_File
     (Self  : Config_Key;
      Conf  : Config_Pool'Class;
      Value : String) return GNATCOLL.VFS.Virtual_File;
   --  Read the key from the configuration.
   --  Using this API might help ensure that you are always accessing existing
   --  keys. In this case, you would have a global package that defines all
   --  valid keys:
   --
   --      Key1 : constant Config_Key := Create ("...");
   --      Key2 : constant Config_Key := Create ("...");
   --
   --  Then your coding standard should specify that you can only access the
   --  configuration via those keys:
   --
   --      Put_Line (Key1.Get);
   --
   --  There is therefore no possible typo in the name of the key, and if you
   --  rename the key in the configuration file, you have a single place to
   --  change.

private
   Section_From_Key : constant String := "#";

   type Config_Parser is abstract tagged record
      System_ID : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type File_Config_Parser is abstract new Config_Parser with record
      Contents : Ada.Strings.Unbounded.Unbounded_String;
      First    : Integer := Integer'Last;
   end record;

   type INI_Parser is new File_Config_Parser with record
      Equal, Eol    : Integer;
      Current_Section : Ada.Strings.Unbounded.Unbounded_String;
      Comment_Start : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("#");
      Use_Sections  : Boolean := True;

      Home          : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Get_Home_Directory;
   end record;

   type Config_Value (Len : Natural) is record
      System_ID : Ada.Strings.Unbounded.Unbounded_String;
      Value     : String (1 .. Len);
   end record;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,   --  "section#key"
      Element_Type    => Config_Value,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Config_Key is tagged record
      Section, Key : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Config_Pool is tagged record
      System_ID : Ada.Strings.Unbounded.Unbounded_String;
      Keys      : String_Maps.Map;
   end record;

end GNATCOLL.Config;
