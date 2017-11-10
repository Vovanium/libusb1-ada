with Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;
with System;

package USB.LibUSB1 is
   use Interfaces.C;

   ---- Library initialisation and finalisation

   type Status is (
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
   for Status use (
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
   pragma Convention(C, Status);

   type Context_Access is new System.Address;
   --type Context_Access_Access is access all Context_Access;
   --pragma Convention(C, Context_Access_Access);

   type Log_Level is (
      None,
      Error,
      Warning,
      Info,
      Debug
   );
   for Log_Level use (
      0,
      1,
      2,
      3,
      4
   );
   pragma Convention(C, Log_Level);

   function Init_Lib(Context: access Context_Access) return Status;
   pragma Import(C, Init_Lib, "libusb_init");

   procedure Exit_Lib(Ctx: Context_Access);
   pragma Import(C, Exit_Lib, "libusb_exit");

   procedure Set_Debug(Ctx: Context_Access; Level: Log_Level);
   pragma Import(C, Set_Debug, "libusb_set_debug");

   ---- Device handling and enumeration

   type ssize_t is range 1-size_t'Modulus/2..size_t'Modulus/2-1;
   for ssize_t'Size use size_t'Size;
   pragma Convention(C, ssize_t);

   type Device_Handle_Access is new System.Address;
   type Device_Access is new System.Address;
   type Device_Access_Array is array(ssize_t range <>) of aliased Device_Access;
   package Device_Access_Lists is new Interfaces.C.Pointers(
    Index => ssize_t,
    Element => Device_Access,
    Element_Array => Device_Access_Array,
    Default_Terminator => Device_Access(System.Null_Address));

   type Speed is (
      Unknown,
      Low,
      Full,
      High,
      Super
   );
   for Speed use (
      0,
      1,
      2,
      3,
      4
   );
   pragma Convention(C, Speed);

   type Bus_Number is mod 2**8;
   pragma Convention(C, Bus_Number);

   type Port_Number is mod 2**8;
   pragma Convention(C, Port_Number);
   type Port_Number_Array is array(Integer range <>) of aliased Port_Number;
   package Port_Number_Lists is new Interfaces.C.Pointers(
    Index => Integer,
    Element => Port_Number,
    Element_Array => Port_Number_Array,
    Default_Terminator => Port_Number'(0));

   type Device_Address is mod 2**8;
   pragma Convention(C, Device_Address);

   type Vendor_Id is mod 2**16;
   pragma Convention(C, Vendor_Id);

   type Product_Id is mod 2**16;
   pragma Convention(C, Product_Id);

   type Attributes is mod 2**32;  -- How to set it to size of C enum?
   for Attributes'Size use int'Size;
   pragma Convention(C, Attributes);
   type Supported_Speed is new Attributes;
   pragma Convention(C, Supported_Speed);
   Low_Speed_Operation: constant Supported_Speed := 1;
   Full_Speed_Operation: constant Supported_Speed := 2;
   High_Speed_Operation: constant Supported_Speed := 4;
   Super_Speed_Operation: constant Supported_Speed := 8;

   type USB_2_0_Extension_Attributes is new Attributes;
   pragma Convention(C, USB_2_0_Extension_Attributes);
   BM_LPM_Support: constant USB_2_0_Extension_Attributes := 2;

   type SS_USB_Device_Capability_Attributes is mod 2**32;
   pragma Convention(C, SS_USB_Device_Capability_Attributes);

   type BOS_Type is (
      Wireless_USB_Device_Capability,
      USB_2_0_Extension,
      SS_USB_Device_Capability,
      Container_ID
   );
   for BOS_Type use (
      1,
      2,
      3,
      4
   );
   pragma Convention(C, BOS_Type);


   function Get_Device_List(Ctx: Context_Access;
    List: access Device_Access_Lists.Pointer) return ssize_t;
   pragma Import(C, Get_Device_List, "libusb_get_device_list");

   procedure Free_Device_List(List: Device_Access_Lists.Pointer;
    Unref_Devices: Int);
   pragma Import(C, Free_Device_List, "libusb_free_device_list");

   function Get_Bus_Number(Dev: Device_Access) return Bus_Number;
   pragma Import(C, Get_Bus_Number, "libusb_get_bus_number");

   function Get_Port_Number(Dev: Device_Access) return Port_Number;
   pragma Import(C, Get_Port_Number, "libusb_get_port_number");

   function Get_Port_Numbers(Dev: Device_Access;
    Port_Numbers: Port_Number_Lists.Pointer;
    Port_Numbers_Len: Integer) return int;
   pragma Import(C, Get_Port_Numbers, "libusb_get_port_numbers");

   function Get_Port_Path(Ctx: Context_Access; Dev: Device_Access;
    Port_Numbers: Port_Number_Lists.Pointer;
    Port_Numbers_Len: Integer) return Integer;
   pragma Import(C, Get_Port_Path, "libusb_get_port_path");

   function Get_Parent(Dev: Device_Access) return Device_Access;
   pragma Import(C, Get_Parent, "libusb_get_parent");

   function Get_Device_Address(Dev: Device_Access) return Device_Address;
   pragma Import(C, Get_Device_Address, "libusb_get_device_address");

   function Get_Device_Speed(Dev: Device_Access) return Speed;
   pragma Import(C, Get_Device_Speed, "libusb_get_device_speed");

   function Get_Max_Packet_Size(Dev: Device_Access;
    Endpoint: unsigned_char) return int;
   pragma Import(C, Get_Max_Packet_Size, "libusb_get_max_packet_size");

   function Get_Max_Iso_Packet_Size(Dev: Device_Access;
    Endpoint: unsigned_char) return int;
   pragma Import(C, Get_Max_Iso_Packet_Size, "libusb_get_max_iso_packet_size");

   function Ref_Device(Dev: Device_Access) return Device_Access;
   pragma Import(C, Ref_Device, "libusb_ref_device");

   procedure Unref_Device(Dev: Device_Access);
   pragma Import(C, Unref_Device, "libusb_unref_device");

   function Open(Dev: Device_Access;
    Handle: access Device_Handle_Access) return Status;
   pragma Import(C, Open, "libusb_open");

   function Open_Device_with_VID_PID(Ctx: Context_Access;
    VID: Vendor_Id;
    PID: Product_Id) return Device_Handle_Access;
   pragma Import(C, Open_Device_with_VID_PID,
    "libusb_open_device_with_vid_pid");

   procedure Close(Dev_Handle: Device_Handle_Access);
   pragma Import(C, Close, "libusb_close");

   function Get_Device(Dev_Handle: Device_Handle_Access) return Device_Access;
   pragma Import(C, Get_Device, "libusb_get_device");

   function Get_Configuration(Dev: Device_Handle_Access;
    Config: access int) return Status;
   pragma Import(C, Get_Configuration, "libusb_get_configuration");

   function Set_Configuration(Dev: Device_Handle_Access;
    Configuration: int) return Status;
   pragma Import(C, Set_Configuration, "libusb_set_configuration");

   function Claim_Interface(Dev: Device_Handle_Access;
    Interface_Number: int) return Status;
   pragma Import(C, Claim_Interface, "libusb_claim_interface");

   function Release_Interface(Dev: Device_Handle_Access;
    Interface_Number: int) return Status;
   pragma Import(C, Release_Interface, "libusb_release_interface");

   function Set_Interface_Alt_Setting(Dev: Device_Handle_Access;
    Interface_Number: int; Alternate_Setting: int) return Status;
   pragma Import(C, Set_Interface_Alt_Setting,
    "libusb_set_interface_alt_setting");

   function Clear_Halt(Dev: Device_Handle_Access; Endpoint: int) return Status;
   pragma Import(C, Clear_Halt, "libusb_clear_halt");

   function Reset_Device(Dev: Device_Handle_Access) return Status;
   pragma Import(C, Reset_Device, "libusb_reset_device");

   function Kernel_Driver_Active(Dev: Device_Handle_Access;
    Interface_Number: int) return int;
   pragma Import(C, Kernel_Driver_Active, "libusb_kernel_driver_active");

   function Detach_Kernel_Driver(Dev: Device_Handle_Access;
    Interface_Number: int) return Status;
   pragma Import(C, Detach_Kernel_Driver, "libusb_detach_kernel_driver");

   function Attach_Kernel_Driver(Dev: Device_Handle_Access;
    Interface_Number: int) return Status;
   pragma Import(C, Attach_Kernel_Driver, "libusb_attach_kernel_driver");

   function Set_Auto_Detach_Kernel_Driver(Dev: Device_Handle_Access;
    Enable: int) return Status;
   pragma Import(C, Set_Auto_Detach_Kernel_Driver,
    "libusb_set_auto_detach_kernel_driver");
end USB.LibUSB1;
