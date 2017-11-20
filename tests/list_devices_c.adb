with USB;
with USB.LibUSB1;
with USB.Protocol;
with Interfaces.C;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
use Ada.Text_IO;
use Ada.Integer_Text_IO;
use USB.LibUSB1;
procedure List_Devices_C is
   Ctx: aliased Context_Access;
   R: Status;
   Ri: Integer;
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
         Dev_Array: Device_Access_Array :=
          Device_Access_Lists.Value(Devs, Interfaces.C.ptrdiff_t(Cnt));
         Desc: aliased USB.Protocol.Device_Descriptor;
         Path: Port_Number_Array(0..7);
      begin
         Put(Integer(Cnt));
         Put_Line(" devices listed");
         for I in Dev_Array'Range loop
            Put(Integer(I-Dev_Array'First), 3);
            Put(Integer(Get_Bus_Number(Dev_Array(I))), 4);
            Put(Integer(Get_Device_Address(Dev_Array(I))), 4);
            R := Get_Device_Descriptor(Dev_Array(I), Desc'Access);
            if R /= Success then
               Put("Failed to get device descriptor: " & Status'Image(R));
            else
               Put(Integer(Desc.idVendor), 10, 16);
               Put(Integer(Desc.idProduct), 10, 16);
               Ri := Integer(Get_Port_Numbers(Dev_Array(I),
                  Path(0)'Unrestricted_Access, -- do not know how to do right
                  Path'Length));
               if Ri > 0 then
                  Put(" path: ");
                  for I in 0 .. Ri-1 loop
                     Put(Integer(Path(I)), 4);
                  end loop;
               end if;
            end if;
            New_Line;
         end loop;
      end;

      Free_Device_List(Devs, 1);
   end if;

   Exit_Lib(Ctx);
end;
