with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

package Support is
   type Custom_Error_Handler is new Error_Handler with record
      Last_Error   : XString;
      Last_Warning : XString;
   end record;

   procedure Warning (Self : in out Custom_Error_Handler; Msg : String);
   procedure Error (Self : in out Custom_Error_Handler; Msg : String);

end Support;
