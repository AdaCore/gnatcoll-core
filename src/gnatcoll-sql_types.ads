package GNATCOLL.Sql_Types is

   K_Delta : constant := 0.01;
   K_Decimals : constant := 2;
   K_Digits : constant := 14;

   --  T_Money type is a numeric (14,2) type for SQL
   type T_Money is delta K_Delta digits K_Digits;
end GNATCOLL.Sql_Types;
