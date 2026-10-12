with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.JSON.Serializers;
with GNATCOLL.Strings;
with Test_Assert;

function Test return Integer is
   Output : Unbounded_String;
   Fail_Sink : Boolean := False;
   Sink_Error : exception;

   type JSON_Writer is new Serializers.Serializer with null record;

   overriding procedure Write (Self : in out JSON_Writer; Content : String);

   overriding procedure Write (Self : in out JSON_Writer; Content : String) is
   begin
      if Fail_Sink then
         raise Sink_Error;
      end if;
      Append (Output, Content);
   end Write;

   W : JSON_Writer;

   type Operation is
     (Finish_Empty, Name_Outside, Close_Outside, Value_Without_Name,
      Double_Name, Close_With_Name, Wrong_Close, Name_In_Array,
      Finish_Open, Second_Root, After_Finish);

   procedure Check_Error (Op : Operation);

   procedure Check_Error (Op : Operation) is
      Before : Unbounded_String;
   begin
      W.Reset;
      Output := Null_Unbounded_String;
      case Op is
         when Value_Without_Name | Double_Name | Close_With_Name =>
            W.Start_Object;
            if Op /= Value_Without_Name then
               W.Encode_Name ("key");
            end if;
         when Wrong_Close | Name_In_Array | Finish_Open =>
            W.Start_Array;
         when Second_Root | After_Finish =>
            W.Encode_Null;
            if Op = After_Finish then
               W.Close;
            end if;
         when others => null;
      end case;
      Before := Output;
      begin
         case Op is
            when Finish_Empty | Finish_Open => W.Close;
            when Name_Outside | Double_Name | Name_In_Array =>
               W.Encode_Name ("x");
            when Close_Outside | Close_With_Name | Wrong_Close => W.End_Object;
            when others => W.Encode_Null;
         end case;
         Test_Assert.Assert (False, "missing error: " & Op'Image);
      exception
         when Serializers.JSON_Serialization_Error =>
            Test_Assert.Assert (Output = Before, "error changed output");
      end;
   end Check_Error;

   procedure Check_String (Text : String);

   procedure Check_String (Text : String) is
   begin
      W.Reset;
      Output := Null_Unbounded_String;
      W.Encode_String (Text);
      W.Close;
      Test_Assert.Assert
        (To_String (Output), Create (Text).Write (Compact => True),
         "same escaping as DOM writer");
   end Check_String;

   Controls : String (1 .. 128);
   UTF8 : constant String :=
     Character'Val (16#C3#) & Character'Val (16#A9#) &
     Character'Val (16#F0#) & Character'Val (16#9F#) &
     Character'Val (16#98#) & Character'Val (16#80#);
begin
   W.Start_Object;
   W.Encode_Name ("");
   W.Start_Array;
   W.Encode_Number (Long_Long_Integer'First);
   W.Encode_Number (Long_Long_Integer'Last);
   W.Encode_Number (Long_Integer'First);
   W.Encode_Number (Long_Integer'Last);
   W.Encode_Number (Integer'First);
   W.Encode_Number (Integer'Last);
   W.Encode_Number (Long_Float'(1.25));
   W.Encode_Boolean (True);
   W.Encode_Boolean (False);
   W.Encode_Null;
   W.Encode_String (To_Unbounded_String ("hello"));
   W.Encode_String (GNATCOLL.Strings.To_XString ("world"));
   W.Start_Object;
   W.End_Object;
   W.Start_Array;
   W.End_Array;
   W.End_Array;
   W.Encode_Name ("quoted""key");
   W.Encode_String (UTF8);
   W.End_Object;
   W.Close;
   Test_Assert.Assert
     (Read (To_String (Output)).Write (Compact => True), To_String (Output),
      "nested document round trip");

   for I in Controls'Range loop
      Controls (I) := Character'Val (I - 1);
   end loop;
   Check_String (Controls);
   Check_String (UTF8);
   Test_Assert.Assert
     (To_String (Output), """\u00E9\uD83D\uDE00""", "Unicode escaping");
   Check_String ("");
   Check_String ("a""b\c");
   Check_String (Controls (7 .. 10));
   Check_String (Character'Val (16#FF#) & "x");
   Check_String ("" & Character'Val (16#E2#));

   for Op in Operation loop
      Check_Error (Op);
   end loop;

   W.Reset;
   Fail_Sink := True;
   begin
      W.Start_Object;
      Test_Assert.Assert (False, "missing sink error");
   exception
      when Sink_Error => null;
   end;
   Fail_Sink := False;
   Check_String ("sink recovery");

   W.Reset;
   Output := Null_Unbounded_String;
   for I in 1 .. 1000 loop
      W.Start_Array;
   end loop;
   W.Encode_Null;
   for I in 1 .. 1000 loop
      W.End_Array;
   end loop;
   W.Close;
   Test_Assert.Assert (Length (Output) = 2004, "deep nesting");
   return Test_Assert.Report;
end Test;
