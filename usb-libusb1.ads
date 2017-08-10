with Interfaces;
with Interfaces.C;
with System;

package USB.LibUSB1 is
   use Interfaces.C;

   type Error is (
     ERROR_NOT_SUPPORTED,
     ERROR_NO_MEM,
     ERROR_INTERRUPTED,
     ERROR_PIPE,
     ERROR_OVERFLOW,
     ERROR_TIMEOUT,
     ERROR_BUSY,
     ERROR_NOT_FOUND,
     ERROR_NO_DEVICE,
     ERROR_ACCESS,
     ERROR_INVALID_PARAM,
     ERROR_IO,
     SUCCESS
   );
   for Error use (
    -12,
    -11,
    -10,
    -9,
    -8,
    -7,
    -6,
    -5,
    -4,
    -3,
    -2,
    -1,
    0
   );
   pragma Convention(C, Error);

   type Context_Access is new System.Address;

   --type Context_Access_Access is access all Context_Access;
   --pragma Convention(C, Context_Access_Access);

   function Init_Lib(Context: access Context_Access) return Error;
   pragma Import(C, Init_Lib, "libusb_init");

   procedure Exit_Lib(Ctx: Context_Access);
   pragma Import(C, Exit_Lib, "libusb_exit");

   procedure Set_Debug(Ctx: Context_Access);
   pragma Import(C, Set_Debug, "set_debug");
end USB.LibUSB1;
