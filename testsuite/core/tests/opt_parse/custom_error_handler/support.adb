package body Support is
   procedure Warning (Self : in out Custom_Error_Handler; Msg : String) is
   begin
      Self.Last_Warning := To_XString (Msg);
   end Warning;

   procedure Error (Self : in out Custom_Error_Handler; Msg : String) is
   begin
      Self.Last_Error := To_XString (Msg);
   end Error;
end Support;
