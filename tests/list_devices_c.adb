with USB;
with USB.LibUSB1;
with Interfaces.C;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use USB.LibUSB1;
procedure List_Devices_C is
   Ctx: aliased Context_Access;
   R: Status;
   Cnt: ssize_t;
   Devs: aliased Device_Access_Lists.Pointer;
begin
   R := Init_Lib(Ctx'Access);
   if R /= Success then
      Put(Status'Image(R));
      Put_Line("");
      return;
   end if;

   Cnt := Get_Device_List(Ctx, Devs'Access);
   if Cnt < 0 then
      --R := Status(Cnt);
      --Put(Status'Image(R));
      Put_Line("");
      --Exit_Lib(Ctx);
      --return;
   else
      declare
         Dev_Array: Device_Access_Array(0..Cnt-1);
      begin
         Dev_Array := Device_Access_Lists.Value(Devs, Interfaces.C.ptrdiff_t(Cnt));
         Put(Integer(Cnt));
         Put_Line(" devices listed");
         for I in Dev_Array'Range loop
            Put(Integer(I), 3);
            Put(Integer(Get_Bus_Number(Dev_Array(I))), 4);
            Put(Integer(Get_Device_Address(Dev_Array(I))), 4);
            New_Line;
         end loop;
      end;

      Free_Device_List(Devs, 1);
   end if;

   Exit_Lib(Ctx);
end;
