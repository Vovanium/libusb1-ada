with USB;
with USB.LibUSB1;
with Interfaces.C;
with Ada.Text_IO;
use Ada.Text_IO;
procedure InitExit is
   Ctx: aliased USB.LibUSB1.Context_Access;
   R: USB.LibUSB1.Status;
begin
   R := USB.LibUSB1.Init_Lib(Ctx'Access);
   Put(USB.LibUSB1.Status'Image(R));
   Put_Line("");
   USB.LibUSB1.Exit_Lib(Ctx);
end;
