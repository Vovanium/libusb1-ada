with Interfaces;
with Interfaces.C;
with System;

package USB.LibUSB1 is
   use Interfaces.C;

   type Error is (
     Error_Other,
     Error_Not_Supported,
     Error_No_Mem,
     Error_Interrupted,
     Error_Pipe,
     Error_Overflow,
     Error_Timeout,
     Error_Busy,
     Error_Not_Found,
     Error_No_Device,
     Error_Access,
     Error_Invalid_Param,
     Error_IO,
     Success
   );
   for Error use (
     Error_Other => -99,
     Error_Not_Supported => -12,
     Error_No_Mem => -11,
     Error_Interrupted => -10,
     Error_Pipe => -9,
     Error_Overflow => -8,
     Error_Timeout => -7,
     Error_Busy => -6,
     Error_Not_Found => -5,
     Error_No_Device => -4,
     Error_Access => -3,
     Error_Invalid_Param => -2,
     Error_IO => -1,
     Success => 0
   );
   pragma Convention(C, Error);

   type Context is new System.Address;

   --type Context_Access_Access is access all Context_Access;
   --pragma Convention(C, Context_Access_Access);
   
   function Init_Lib(Ctx: access Context) return Error; -- rewrite with System.Address_To_Access_Conversions
   pragma Import(C, Init_Lib, "libusb_init");

   procedure Exit_Lib(Ctx: in Context);
   pragma Import(C, Exit_Lib, "libusb_exit");

   procedure Set_Debug(Ctx: in Context);
   pragma Import(C, Set_Debug, "libusb_set_debug");
   
   -- Device handling and enumeration
   
   type Device is new System.Address;
   
   type Device_List is new System.Address;
   
   type Device_Handle is new System.Address;
   
   type Speed is (Unknown, Low, Full, High, Super);
   pragma Convention(C, Speed);
   for Speed use (
     Unknown => 0,
     Low => 1,
     Full => 2,
     High => 3,
     Super => 4
   );
   
   type Ssize_T is range -(Interfaces.C.Size_T'Last-1)/2..(Interfaces.C.Size_T'Last-1)/2;
   for Ssize_T'Size use Size_T'Size;
   
   function Get_Device_List(Ctx: in Context; List: access Device_List) return Ssize_T;
   pragma Import(C, Get_Device_List, "libusb_get_device_list");
   
   procedure Free_Device_List(List: in Device_List; Unref_Devices: Int);
   pragma Import(C, Free_Device_List, "libusb_free_device_list");
end USB.LibUSB1;
