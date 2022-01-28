with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Mylib is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   type Context is record
      Ints : Int_Vectors.Vector;
   end record;
   type Context_Array is array (Positive range <>) of Context;

   package Args is
      Parser : Argument_Parser :=
         Create_Argument_Parser (Help => "Test program");

      package Charset_Option is new Parse_Option
         (Parser      => Parser,
         Short       => "-C",
         Long        => "--charset",
         Arg_Type    => Unbounded_String,
         Usage_Text  => "[--charset|-C <charset name>]",
         Help        =>
            "What charset to use when parsing files. "
            & "Default is ""latin-1""",
         Default_Val => To_Unbounded_String ("latin-1"));

      package Day_Option is new Parse_Enum_Option
         (Parser      => Parser,
         Short       => "-D",
         Long        => "--day",
         Arg_Type    => Day,
         Usage_Text  => "[--day|-D <three letter day of week>]",
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

      package Files_Option is new Parse_Option_List
         (Parser      => Parser,
         Short       => "-F",
         Long        => "--files",
         Arg_Type    => Unbounded_String,
         Usage_Text  => "[--files|-F <list of filepaths to parse>]",
         Help        => "List of files to be parsed.");

      package Charset_Option2 is new Parse_Option
         (Parser      => Parser,
         Short       => "-C2",
         Long        => "--charset2",
         Arg_Type    => Unbounded_String,
         Help        =>
            "What charset to use when parsing files. "
            & "Default is ""latin-1""",
         Default_Val => To_Unbounded_String ("latin-1"));

      package Day_Option2 is new Parse_Enum_Option
         (Parser      => Parser,
         Short       => "-D2",
         Long        => "--day2",
         Arg_Type    => Day,
         Help        =>
            "What day of the week is it? "
            & "Default is ""Mon""",
         Default_Val => Mon);

      package Files_Option2 is new Parse_Option_List
        (Parser      => Parser,
         Short       => "-F2",
         Long        => "--files2",
         Arg_Type    => Unbounded_String,
         Help        => "List of files to be parsed.");

   end Args;

   procedure Run;

end Mylib;
