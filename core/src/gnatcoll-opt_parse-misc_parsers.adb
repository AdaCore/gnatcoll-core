--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: GPL-3.0-or-later WITH GCC-exception-3.1

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.Regpat; use GNAT.Regpat;

with GNATCOLL.Strings; use GNATCOLL.Strings;


package body GNATCOLL.Opt_Parse.Misc_Parsers is

   pragma Extensions_Allowed (On);

   package body Parse_Indexed_Option is

      Indexed_Component_Matcher : constant Pattern_Matcher :=
        Compile (Flag & "(?::(\w+))?(=.+)?$");

      package I is new Flag_Invariants
        ("", Flag, Name, Legacy_Long_Form => True);
      pragma Unreferenced (I);

      type Option_Parser is new Subparser_Type with record
         null;
      end record;

      overriding function Does_Accumulate
        (Self : Option_Parser) return Boolean is (True);

      overriding function Usage
        (Self : Option_Parser) return String;

      overriding function Help_Name
        (Dummy : Option_Parser) return String;

      overriding procedure Init_JSON_Help
        (Self : Option_Parser; Val : JSON_Value);

      overriding function Parse_Args
        (Self   : in out Option_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      overriding function JSON_Kind (Self : Option_Parser) return String
      is ("indexed_option");

      type Internal_Result is new Parser_Result with record
         Result : Result_Map_Access;
      end record;

      procedure Release (Self : in out Internal_Result);

      Self_Val : aliased Option_Parser :=
        Option_Parser'
          (Name   => +(if Name /= "" then Name
                       else Flag (3 .. Flag'Last)),
           Help   => +Help,
           Parser => Parser.Data,
           Opt    => True,
           others => <>);

      Self : constant Subparser := Self_Val'Unchecked_Access;

      function This return Subparser is (Self);

      -------------
      -- Release --
      -------------

      procedure Release (Self : in out Internal_Result) is
         procedure Free is new Ada.Unchecked_Deallocation
           (Result_Maps.Map, Result_Map_Access);
      begin
         Free (Self.Result);
      end Release;

      -----------
      -- Usage --
      -----------

      overriding function Usage
        (Self : Option_Parser) return String
      is
         Usage_Name : constant String :=
           (if Name /= "" then Name else To_Upper (+Self.Name));
      begin
         if Usage_Text = "" then
            return "[" & Flag & " " & Usage_Name & "]";
         end if;

         return Usage_Text;
      end Usage;

      ---------------
      -- Help_Name --
      ---------------

      overriding function Help_Name
        (Dummy : Option_Parser) return String
      is
      begin
         return Flag;
      end Help_Name;

      ---------
      -- Get --
      ---------

      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments)
      return Result_Map_Access
      is
      begin
         if not Enabled then
            return null;
         end if;

         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               return Internal_Result (R.all).Result;
            else
               return null;
            end if;
         end;
      end Get;

      ----------------
      -- Parse_Args --
      ----------------

      overriding function Parse_Args
        (Self   : in out Option_Parser;
         Args   : XString_Array;
         Pos    : Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         Res  : Parser_Result_Access
         renames Result.Ref.Unchecked_Get.Results (Self.Position);

         type Internal_Result_Access is access all Internal_Result;

         Unsafe_Res : Internal_Result_Access;


         New_Pos : Parser_Return;
         Raw     : XString;
         Matches : Match_Array (0 .. 2);

         Arg   : constant String := Args (Pos).To_String;
         Index : XString;
      begin
         Match (Indexed_Component_Matcher, Arg, Matches);

         --  Early exit if the regex doesn't match at all
         if Matches (0) = No_Match then
            return Error_Return;
         end if;

         --  Match index or else, index is empty string
         if Matches (1) = No_Match then
            Index := +"";
         else
            Index := +Arg (Matches (1).First .. Matches (1).Last);
         end if;

         --  Try to match the "=<value>" part
         if Matches (2) = No_Match then
            --  Arg doesn't contain "=<value>". The value is the next arg then.
            if Pos + 1 > Args'Last then
               raise Opt_Parse_Error with "Incomplete option";
            end if;

            New_Pos := Pos + 2;
            Raw := Args (Pos + 1);
         else
            --  Arg contains "=<value>". Extract the value
            Raw := +Arg (Matches (2).First + 1 .. Matches (2).Last);
            New_Pos := Pos + 1;
         end if;

         if New_Pos /= Error_Return then
            if Res = null then
               Unsafe_Res := new Internal_Result'
                 (Start_Pos => Pos,
                  End_Pos   => Pos,
                  Result    => new Result_Maps.Map);
               Res := Unsafe_Res.all'Unchecked_Access;
            end if;

            declare
               Dummy_Cursor : Result_Maps.Cursor;
               Inserted     : Boolean;
            begin
               Internal_Result (Res.all).Result.Insert
                 (+Index, Convert (+Raw), Dummy_Cursor, Inserted);

               if not Inserted then
                  raise Opt_Parse_Error with "Duplicate key in indexed option";
               end if;
            end;

         end if;

         return New_Pos;
      end Parse_Args;

      --------------------
      -- Init_JSON_Help --
      --------------------

      overriding procedure Init_JSON_Help
        (Self : Option_Parser; Val : JSON_Value) is
      begin
         if Flag /= "" then
            Val.Set_Field ("long_flag", Flag);
         end if;
      end Init_JSON_Help;

   begin
      if Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Indexed_Option;
end GNATCOLL.Opt_Parse.Misc_Parsers;
