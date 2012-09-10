with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
package body GNATCOLL.Xref.Database is
   pragma Style_Checks (Off);
   use type Cst_String_Access;

   function FK (Self : T_E2e'Class; Foreign : T_E2e_Kind'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;

   function FK (Self : T_Entities'Class; Foreign : T_Entity_Kinds'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;

   function FK (Self : T_Entities'Class; Foreign : T_Files'Class) return SQL_Criteria is
   begin
      return Self.Decl_File = Foreign.Id;
   end FK;

   function FK (Self : T_Entities'Class; Foreign : T_Entities'Class) return SQL_Criteria is
   begin
      return Self.Decl_Caller = Foreign.Id;
   end FK;

   function FK (Self : T_Entity_Refs'Class; Foreign : T_Files'Class) return SQL_Criteria is
   begin
      return Self.File = Foreign.Id;
   end FK;

   function FK (Self : T_Entity_Refs'Class; Foreign : T_Reference_Kinds'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;

   function FK (Self : T_F2f'Class; Foreign : T_F2f_Kind'Class) return SQL_Criteria is
   begin
      return Self.Kind = Foreign.Id;
   end FK;

   procedure Create_Database
      (DB : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class)
   is
      DbSchema : constant String := "|TABLE| files" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|path|Text|NOT NULL,INDEX||" & ASCII.LF
         & "|stamp|timestamp with time zone|||" & ASCII.LF
         & "|language|Text|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| f2f_kind" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|name|Text|NOT NULL||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| f2f" & ASCII.LF
         & "|fromFile|FK files|NOT NULL,NOINDEX||" & ASCII.LF
         & "|toFile|FK files|NOT NULL,NOINDEX||" & ASCII.LF
         & "|kind|FK f2f_kind|NOT NULL,NOINDEX||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| entity_kinds" & ASCII.LF
         & "|id|Character(1)|PK||" & ASCII.LF
         & "|display|Text|NOT NULL,NOINDEX||" & ASCII.LF
         & "|is_subprogram|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_container|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|body_is_full_declaration|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_abstract|Boolean|,NOINDEX|false|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| entities" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|name|Text|NOT NULL,NOINDEX,NOCASE||" & ASCII.LF
         & "|kind|FK entity_kinds|NOT NULL,NOINDEX||" & ASCII.LF
         & "|decl_file|FK files|NOT NULL,NOINDEX||" & ASCII.LF
         & "|decl_line|Integer|NOT NULL,NOINDEX||" & ASCII.LF
         & "|decl_column|Integer|NOT NULL,NOINDEX||" & ASCII.LF
         & "|decl_caller|FK entities|,NOINDEX||" & ASCII.LF
         & "|mangled_name|Text|||" & ASCII.LF
         & "|exported|Boolean|NOT NULL,NOINDEX|false|" & ASCII.LF
         & "|obsolete|Boolean|NOT NULL,NOINDEX|false|" & ASCII.LF
         & "|INDEX:|decl_file,decl_line,decl_column|entity_loc" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| e2e_kind" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|name|Text|NOT NULL,NOINDEX||" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| e2e" & ASCII.LF
         & "|fromEntity|FK entities|NOT NULL,NOINDEX||" & ASCII.LF
         & "|toEntity|FK entities|NOT NULL,NOINDEX||" & ASCII.LF
         & "|kind|FK e2e_kind|NOT NULL,NOINDEX||" & ASCII.LF
         & "|order_by|Integer|NOT NULL,NOINDEX|1|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| reference_kinds" & ASCII.LF
         & "|id|Character(1)|PK||" & ASCII.LF
         & "|display|Text|NOT NULL,NOINDEX||" & ASCII.LF
         & "|is_real|Boolean|,NOINDEX|true|" & ASCII.LF
         & "|is_read|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_write|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_end|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|show_in_callgraph|Boolean|,NOINDEX|true|" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| entity_refs" & ASCII.LF
         & "|entity|FK entities|NOT NULL,NOINDEX||" & ASCII.LF
         & "|file|FK files|NOT NULL,NOINDEX||" & ASCII.LF
         & "|line|Integer|NOT NULL,NOINDEX||" & ASCII.LF
         & "|column|Integer|NOT NULL,NOINDEX||" & ASCII.LF
         & "|kind|FK reference_kinds|NOT NULL,NOINDEX||" & ASCII.LF
         & "|caller|FK entities|,NOINDEX||" & ASCII.LF
         & "|from_instantiation|Text|,NOINDEX||" & ASCII.LF
         & "|INDEX:|""file""|entity_refs_file" & ASCII.LF
         & "|INDEX:|""entity""|entity_refs_entity" & ASCII.LF
         & "" & ASCII.LF
         & "";
      Data : constant String := "|TABLE|entity_kinds|||||" & ASCII.LF
         & "|id|display|is_subprogram|is_container|body_is_full_declaration|is_abstract|" & ASCII.LF
         & "|a|array|false|false|false|false|" & ASCII.LF
         & "|A|array type|false|false|false|false|" & ASCII.LF
         & "|b|boolean|false|false|false|false|" & ASCII.LF
         & "|B|boolean type|false|false|false|false|" & ASCII.LF
         & "|c|class wide|false|false|false|false|" & ASCII.LF
         & "|C|class wide type|false|false|false|false|" & ASCII.LF
         & "|d|decimal fixed point|false|false|false|false|" & ASCII.LF
         & "|D|decimal fixed point type|false|false|false|false|" & ASCII.LF
         & "|e|enumeration|false|true|true|false|" & ASCII.LF
         & "|E|enumeration type|false|true|true|false|" & ASCII.LF
         & "|f|floating point|false|false|false|false|" & ASCII.LF
         & "|F|floating point type|false|false|false|false|" & ASCII.LF
         & "|g|macro|true|false|false|false|" & ASCII.LF
         & "|G|function macro|false|false|false|false|" & ASCII.LF
         & "|h|interface|false|false|false|true|" & ASCII.LF
         & "|H|abstract record type|false|true|true|true|" & ASCII.LF
         & "|i|integer|false|false|false|false|" & ASCII.LF
         & "|I|integer type|false|false|false|false|" & ASCII.LF
         & "|j|class instance|false|false|false|false|" & ASCII.LF
         & "|J|class|false|false|false|false|" & ASCII.LF
         & "|k|generic package|false|true|false|false|" & ASCII.LF
         & "|K|package|false|true|false|false|" & ASCII.LF
         & "|l|loop label|false|false|false|false|" & ASCII.LF
         & "|L|statement label|false|false|false|false|" & ASCII.LF
         & "|m|unsigned integer|false|false|false|false|" & ASCII.LF
         & "|M|unsigned integer type|false|false|false|false|" & ASCII.LF
         & "|n|enumeration literal|false|false|false|false|" & ASCII.LF
         & "|N|named number|false|false|false|false|" & ASCII.LF
         & "|o|fixed point|false|false|false|false|" & ASCII.LF
         & "|O|fixed point type|false|false|false|false|" & ASCII.LF
         & "|p|pointer|false|false|false|false|" & ASCII.LF
         & "|P|access type|false|false|false|false|" & ASCII.LF
         & "|q|block label|false|false|false|false|" & ASCII.LF
         & "|Q|include file|false|false|false|false|" & ASCII.LF
         & "|r|record|false|true|true|false|" & ASCII.LF
         & "|R|record type|false|true|true|false|" & ASCII.LF
         & "|s|string|false|false|false|false|" & ASCII.LF
         & "|S|string type|false|false|false|false|" & ASCII.LF
         & "|t|task|true|true|false|false|" & ASCII.LF
         & "|T|task type|true|true|false|false|" & ASCII.LF
         & "|u|generic procedure|true|true|false|false|" & ASCII.LF
         & "|U|procedure|true|true|false|false|" & ASCII.LF
         & "|v|generic function|true|true|false|false|" & ASCII.LF
         & "|V|function|true|true|false|false|" & ASCII.LF
         & "|w|protected object|false|false|false|false|" & ASCII.LF
         & "|W|protected type|false|false|false|false|" & ASCII.LF
         & "|x|abstract procedure|true|true|false|false|" & ASCII.LF
         & "|X|exception|false|false|false|false|" & ASCII.LF
         & "|y|abstract function|true|true|false|false|" & ASCII.LF
         & "|Y|entry|true|true|false|false|" & ASCII.LF
         & "|z|unknown|false|false|false|false|" & ASCII.LF
         & "|Z|unknown|false|false|false|false|" & ASCII.LF
         & "|+|generic formal|false|false|false|false|" & ASCII.LF
         & "|*|private object|false|false|false|false|" & ASCII.LF
         & "|TABLE|reference_kinds||||||" & ASCII.LF
         & "|id|display|is_real|is_read|is_write|is_end|show_in_callgraph|" & ASCII.LF
         & "|b|body|true|true|false|false|false|" & ASCII.LF
         & "|c|full declaration|true|true|false|false|false|" & ASCII.LF
         & "|D|object definition|true|true|false|false|false|" & ASCII.LF
         & "|e|end of spec|false|false|false|true|false|" & ASCII.LF
         & "|i|implicit reference|false|false|false|false|false|" & ASCII.LF
         & "|k|parent package|false|false|false|false|false|" & ASCII.LF
         & "|H|abstract type|true|false|false|false|false|" & ASCII.LF
         & "|l|label on end line|true|true|false|false|false|" & ASCII.LF
         & "|m|write reference|true|false|true|false|false|" & ASCII.LF
         & "|o|own reference|true|true|false|false|false|" & ASCII.LF
         & "|r|reference|true|true|false|false|true|" & ASCII.LF
         & "|s|static call|true|true|false|false|true|" & ASCII.LF
         & "|R|dispatching call|true|true|false|false|true|" & ASCII.LF
         & "|t|end of body|false|false|false|true|false|" & ASCII.LF
         & "|w|with line|true|true|false|false|false|" & ASCII.LF
         & "|x|type extension|true|true|false|false|false|" & ASCII.LF
         & "|X|type extension|true|true|false|false|false|" & ASCII.LF
         & "|z|formal generic parameter|false|false|false|false|false|" & ASCII.LF
         & "|TABLE|f2f_kind||" & ASCII.LF
         & "|id|name||" & ASCII.LF
         & "|0|withs|" & ASCII.LF
         & "|1|has ALI|" & ASCII.LF
         & "|TABLE|e2e_kind||" & ASCII.LF
         & "|id|name||" & ASCII.LF
         & "|0|pointed type|" & ASCII.LF
         & "|1|parent type|" & ASCII.LF
         & "|3|renames|" & ASCII.LF
         & "|4|has primitive|" & ASCII.LF
         & "|5|instance of|" & ASCII.LF
         & "|6|is param of|" & ASCII.LF
         & "|7|has index|" & ASCII.LF
         & "|8|overrides|" & ASCII.LF
         & "|9|component type|" & ASCII.LF
         & "|10|from enumeration|" & ASCII.LF
         & "|11|of type|" & ASCII.LF
         & "|12|in parameter|" & ASCII.LF
         & "|13|out parameter|" & ASCII.LF
         & "|14|in out parameter|" & ASCII.LF
         & "|15|access parameter|" & ASCII.LF
         & "|16|has discriminant|" & ASCII.LF
         & "|17|is formal of|" & ASCII.LF
         & "|TABLE|files|||" & ASCII.LF
         & "|id|path|stamp|language|" & ASCII.LF
         & "|-1|/|1970-01-01|ada|" & ASCII.LF
         & "|TABLE|entities||||" & ASCII.LF
         & "|name|kind|decl_file|decl_line|decl_column|" & ASCII.LF
         & "|__va_list_tag|p|-1|-1|-1|" & ASCII.LF
         & "|boolean|B|-1|-1|-1|" & ASCII.LF
         & "|char|I|-1|-1|-1|" & ASCII.LF
         & "|character|E|-1|-1|-1|" & ASCII.LF
         & "|double|F|-1|-1|-1|" & ASCII.LF
         & "|duration|O|-1|-1|-1|" & ASCII.LF
         & "|float|F|-1|-1|-1|" & ASCII.LF
         & "|int|I|-1|-1|-1|" & ASCII.LF
         & "|integer|I|-1|-1|-1|" & ASCII.LF
         & "|long double|F|-1|-1|-1|" & ASCII.LF
         & "|long int|I|-1|-1|-1|" & ASCII.LF
         & "|long long int|I|-1|-1|-1|" & ASCII.LF
         & "|long long unsigned int|I|-1|-1|-1|" & ASCII.LF
         & "|long unsigned int|I|-1|-1|-1|" & ASCII.LF
         & "|long_float|F|-1|-1|-1|" & ASCII.LF
         & "|long_long_float|F|-1|-1|-1|" & ASCII.LF
         & "|long_integer|I|-1|-1|-1|" & ASCII.LF
         & "|long_long_integer|I|-1|-1|-1|" & ASCII.LF
         & "|natural|I|-1|-1|-1|" & ASCII.LF
         & "|positive|I|-1|-1|-1|" & ASCII.LF
         & "|short int|I|-1|-1|-1|" & ASCII.LF
         & "|short unsigned int|I|-1|-1|-1|" & ASCII.LF
         & "|short_float|F|-1|-1|-1|" & ASCII.LF
         & "|short_integer|I|-1|-1|-1|" & ASCII.LF
         & "|short_short_integer|I|-1|-1|-1|" & ASCII.LF
         & "|signed char|I|-1|-1|-1|" & ASCII.LF
         & "|string|A|-1|-1|-1|" & ASCII.LF
         & "|unsigned char|I|-1|-1|-1|" & ASCII.LF
         & "|unsigned int|I|-1|-1|-1|" & ASCII.LF
         & "|void|p|-1|-1|-1|" & ASCII.LF
         & "|wide_character|E|-1|-1|-1|" & ASCII.LF
         & "|wide_wide_character|E|-1|-1|-1|" & ASCII.LF
         & "|wide_string|A|-1|-1|-1|" & ASCII.LF
         & "|wide_wide_string|A|-1|-1|-1|" & ASCII.LF
         & "";
      F : File_Schema_IO;
      D : DB_Schema_IO;
      Schema : DB_Schema;
   begin
      Schema := Read_Schema (F, DbSchema);
      D.DB := Database_Connection (DB);
      Write_Schema (D, Schema);
      if DB.Success then
         Load_Data (DB, Data, Schema);
      end if;
   end Create_Database;
end GNATCOLL.Xref.Database;
