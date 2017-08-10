with USB;
with USB.LibUSB1;
with Interfaces.C;
procedure InitExit is
   Ctx: aliased USB.LibUSB1.Context_Access;
   R: USB.LibUSB1.Error;
begin
   R := USB.LibUSB1.Init_Lib(Ctx'Access);
   USB.LibUSB1.Exit_Lib(Ctx);
end;
