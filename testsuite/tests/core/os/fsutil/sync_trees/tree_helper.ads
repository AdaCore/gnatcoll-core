with Ada.Strings.UTF_Encoding;

package Tree_Helper is
   package UTF8 renames Ada.Strings.UTF_Encoding;

   procedure Display_Trees (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String);
   --  Display both src and dst tree hierarchy

   procedure Compare_Tree_Files_Content
     (Src : UTF8.UTF_8_String; Dst : UTF8.UTF_8_String);
   --  Check if regular files in dst have the same content as in src. Assume
   --  that a regular file in dst is also present in dst

end Tree_Helper;