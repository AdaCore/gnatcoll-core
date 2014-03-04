with GNATCOLL.SQL.Inspect; use GNATCOLL.SQL.Inspect;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
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

   function FK (Self : T_Files'Class; Foreign : T_Files'Class) return SQL_Criteria is
   begin
      return Self.Project = Foreign.Id;
   end FK;

   procedure Create_Database
      (DB : access GNATCOLL.SQL.Exec.Database_Connection_Record'Class)
   is
      DbSchema : constant String := "|TABLE| files" & ASCII.LF
         & "|id|AUTOINCREMENT|PK||" & ASCII.LF
         & "|path|Text|NOT NULL,INDEX||" & ASCII.LF
         & "|stamp|timestamp with time zone|||" & ASCII.LF
         & "|language|Text|NOT NULL||" & ASCII.LF
         & "|project|FK files|||" & ASCII.LF
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
         & "|is_generic|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_access|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_type|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_printable_in_gdb|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_array|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|has_methods|Boolean|,NOINDEX|false|" & ASCII.LF
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
         & "|is_global|Boolean|NOT NULL,NOINDEX|false|" & ASCII.LF
         & "|is_static_local|Boolean|NOT NULL,NOINDEX|false|" & ASCII.LF
         & "|INDEX:|decl_file,decl_line,decl_column|entity_loc" & ASCII.LF
         & "|INDEX:|decl_caller|entity_decl_caller" & ASCII.LF
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
         & "|INDEX:|""fromEntity""|e2e_from" & ASCII.LF
         & "|INDEX:|""toEntity""|e2e_to" & ASCII.LF
         & "" & ASCII.LF
         & "|TABLE| reference_kinds" & ASCII.LF
         & "|id|Character(1)|PK||" & ASCII.LF
         & "|display|Text|NOT NULL,NOINDEX||" & ASCII.LF
         & "|is_real|Boolean|,NOINDEX|true|" & ASCII.LF
         & "|is_read|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_write|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_end|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|show_in_callgraph|Boolean|,NOINDEX|true|" & ASCII.LF
         & "|is_dispatching|Boolean|,NOINDEX|false|" & ASCII.LF
         & "|is_implicit|Boolean|,NOINDEX|false|" & ASCII.LF
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
         & "|INDEX:|line,column|entity_refs_loc" & ASCII.LF
         & "|INDEX:|caller|refs_caller" & ASCII.LF
         & "" & ASCII.LF
         & "";
      Data : constant String := "|TABLE|entity_kinds|||||||||||" & ASCII.LF
         & "|id|display|has_methods|is_array|is_printable_in_gdb|is_type|is_subprogram|is_container|body_is_full_declaration|is_abstract|is_generic|is_access|" & ASCII.LF
         & "|a|array|false|true|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|A|array type|false|true|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|b|boolean|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|B|boolean type|false|false|false|trye|false|false|false|false|false|false|" & ASCII.LF
         & "|c|class wide|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|C|class wide type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|d|decimal fixed point|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|D|decimal fixed point type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|e|enumeration|false|false|true|false|false|true|true|false|false|false|" & ASCII.LF
         & "|E|enumeration type|false|false|false|true|false|false|true|false|false|false|" & ASCII.LF
         & "|f|floating point|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|F|floating point type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|g|macro|false|false|false|false|true|false|false|false|false|false|" & ASCII.LF
         & "|G|function macro|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|h|interface|true|false|false|true|false|false|false|true|false|false|" & ASCII.LF
         & "|H|abstract record type|true|false|false|true|false|true|true|true|false|false|" & ASCII.LF
         & "|i|integer|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|I|integer type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|j|class instance|true|false|false|false|false|true|false|false|false|false|" & ASCII.LF
         & "|J|class|true|false|true|true|false|true|false|false|false|false|" & ASCII.LF
         & "|k|generic package|false|false|false|false|false|true|false|false|true|false|" & ASCII.LF
         & "|K|package|false|false|false|false|false|true|false|false|false|false|" & ASCII.LF
         & "|l|loop label|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|L|statement label|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|m|unsigned integer|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|M|unsigned integer type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|n|enumeration literal|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|N|named number|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|o|fixed point|false|false|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|O|fixed point type|false|false|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|p|pointer|false|false|true|false|false|false|false|false|false|true|" & ASCII.LF
         & "|P|access type|false|false|true|true|false|false|false|false|false|true|" & ASCII.LF
         & "|q|block label|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|Q|include file|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|r|record|true|false|true|false|false|true|true|false|false|false|" & ASCII.LF
         & "|R|record type|true|false|false|true|false|true|true|false|false|false|" & ASCII.LF
         & "|s|string|false|true|true|false|false|false|false|false|false|false|" & ASCII.LF
         & "|S|string type|false|true|false|true|false|false|false|false|false|false|" & ASCII.LF
         & "|t|task|false|false|false|false|true|true|false|false|false|false|" & ASCII.LF
         & "|T|task type|false|false|false|true|true|true|false|false|false|false|" & ASCII.LF
         & "|u|generic procedure|false|false|false|false|true|true|false|false|true|false|" & ASCII.LF
         & "|U|procedure|false|false|false|false|true|true|false|false|false|false|" & ASCII.LF
         & "|v|generic function|false|false|false|false|true|true|false|false|true|false|" & ASCII.LF
         & "|V|function|false|false|false|false|true|true|false|false|false|false|" & ASCII.LF
         & "|w|protected object|false|false|true|false|false|true|false|false|false|false|" & ASCII.LF
         & "|W|protected type|false|false|false|true|false|true|false|false|false|false|" & ASCII.LF
         & "|x|abstract procedure|false|false|false|false|true|true|false|true|false|false|" & ASCII.LF
         & "|X|exception|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|y|abstract function|false|false|false|false|true|true|false|true|false|false|" & ASCII.LF
         & "|Y|entry|false|false|false|false|true|true|false|false|false|false|" & ASCII.LF
         & "|z|unknown|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|Z|unknown|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|+|generic formal|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|@|abstract state|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|*|private object|false|false|false|false|false|false|false|false|false|false|" & ASCII.LF
         & "|TABLE|reference_kinds||||||||" & ASCII.LF
         & "|id|display|is_implicit|is_real|is_read|is_write|is_end|show_in_callgraph|is_dispatching|" & ASCII.LF
         & "|b|body|false|true|true|false|false|false|false|" & ASCII.LF
         & "|c|full declaration|false|true|true|false|false|false|false|" & ASCII.LF
         & "|D|object definition|false|true|true|false|false|false|false|" & ASCII.LF
         & "|e|end of spec|false|false|false|false|true|false|false|" & ASCII.LF
         & "|E|private part|false|false|false|false|false|false|false|" & ASCII.LF
         & "|i|implicit reference|true|true|false|false|false|false|false|" & ASCII.LF
         & "|H|abstract type|false|true|false|false|false|false|false|" & ASCII.LF
         & "|l|label on end line|false|true|true|false|false|false|false|" & ASCII.LF
         & "|m|write reference|false|true|false|true|false|false|false|" & ASCII.LF
         & "|o|own reference|false|true|true|false|false|false|false|" & ASCII.LF
         & "|r|reference|false|true|true|false|false|true|false|" & ASCII.LF
         & "|s|static call|false|true|true|false|false|true|false|" & ASCII.LF
         & "|R|dispatching call|false|true|true|false|false|true|true|" & ASCII.LF
         & "|t|end of body|false|false|false|false|true|false|false|" & ASCII.LF
         & "|w|with line|false|true|true|false|false|false|false|" & ASCII.LF
         & "|x|type extension|false|true|true|false|false|false|false|" & ASCII.LF
         & "|X|type extension|false|true|true|false|false|false|false|" & ASCII.LF
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
         & "|18|parent package|" & ASCII.LF
         & "|TABLE|files||||" & ASCII.LF
         & "|id|path|stamp|language|project|" & ASCII.LF
         & "|-2|/|1970-01-01|project|NULL|" & ASCII.LF
         & "|-1|/|1970-01-01|ada|-2|" & ASCII.LF
         & "|TABLE|entities|||||" & ASCII.LF
         & "|name|kind|decl_file|decl_line|decl_column|is_global|" & ASCII.LF
         & "|__va_list_tag|p|-1|-1|-1|true|" & ASCII.LF
         & "|Boolean|B|-1|-1|-1|true|" & ASCII.LF
         & "|char|I|-1|-1|-1|true|" & ASCII.LF
         & "|Character|E|-1|-1|-1|true|" & ASCII.LF
         & "|Double|F|-1|-1|-1|true|" & ASCII.LF
         & "|Duration|O|-1|-1|-1|true|" & ASCII.LF
         & "|Float|F|-1|-1|-1|true|" & ASCII.LF
         & "|int|I|-1|-1|-1|true|" & ASCII.LF
         & "|Integer|I|-1|-1|-1|true|" & ASCII.LF
         & "|bool|B|-1|-1|-1|true|" & ASCII.LF
         & "|long double|F|-1|-1|-1|true|" & ASCII.LF
         & "|long int|I|-1|-1|-1|true|" & ASCII.LF
         & "|long long int|I|-1|-1|-1|true|" & ASCII.LF
         & "|long long unsigned int|I|-1|-1|-1|true|" & ASCII.LF
         & "|long unsigned int|I|-1|-1|-1|true|" & ASCII.LF
         & "|Long_Float|F|-1|-1|-1|true|" & ASCII.LF
         & "|Long_Long_Float|F|-1|-1|-1|true|" & ASCII.LF
         & "|Long_Integer|I|-1|-1|-1|true|" & ASCII.LF
         & "|Long_Long_Integer|I|-1|-1|-1|true|" & ASCII.LF
         & "|Natural|I|-1|-1|-1|true|" & ASCII.LF
         & "|Positive|I|-1|-1|-1|true|" & ASCII.LF
         & "|short int|I|-1|-1|-1|true|" & ASCII.LF
         & "|short unsigned int|I|-1|-1|-1|true|" & ASCII.LF
         & "|Short_Float|F|-1|-1|-1|true|" & ASCII.LF
         & "|Short_Integer|I|-1|-1|-1|true|" & ASCII.LF
         & "|Short_Short_Integer|I|-1|-1|-1|true|" & ASCII.LF
         & "|signed char|I|-1|-1|-1|true|" & ASCII.LF
         & "|String|A|-1|-1|-1|true|" & ASCII.LF
         & "|unsigned char|I|-1|-1|-1|true|" & ASCII.LF
         & "|unsigned int|I|-1|-1|-1|true|" & ASCII.LF
         & "|void|p|-1|-1|-1|true|" & ASCII.LF
         & "|_Bool|B|-1|-1|-1|true|" & ASCII.LF
         & "|Wide_Character|E|-1|-1|-1|true|" & ASCII.LF
         & "|Wide_Wide_Character|E|-1|-1|-1|true|" & ASCII.LF
         & "|Wide_String|A|-1|-1|-1|true|" & ASCII.LF
         & "|Wide_Wide_String|A|-1|-1|-1|true|" & ASCII.LF
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
