with Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with System;
with USB.Protocol;

package USB.LibUSB1 is
   use Interfaces;
   use Interfaces.C;
   use USB.Protocol;

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
      Speed_Unknown,
      Speed_Low,
      Speed_Full,
      Speed_High,
      Speed_Super
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
    Endpoint: unsigned_char) return int; -- ?? endpoint address?
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
    VID: USB.Protocol.Vendor_Id;
    PID: USB.Protocol.Product_Id) return Device_Handle_Access;
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

   ---- Miscellaneus

   API_Version: constant Integer := 16#01000104#;

   type Lib_Capability is (
      Cap_Has_Capability, Cap_Has_Hotplug,
      Cap_Has_HID_Access, Cap_Supports_Detach_Kernel_Driver
   );
   for Lib_Capability use (
      16#0000#, 16#0001#,
      16#0100#, 16#0101#
   );
   pragma Convention(C, Lib_Capability);

   type Version_Number is mod 2**16 with Size => 16, Convention => C;

   type Lib_Version is record
      Major, Minor, Micro, Nano: Version_Number;
      Rc, Describe: Strings.chars_ptr;
   end record;

   function Has_Capability(Capability: Lib_Capability) return int;
   pragma Import(C, Has_Capability, "libusb_has_capability");

   function Error_Name(Error_Code: int) return Strings.chars_ptr;
   pragma Import(C, Error_Name, "libusb_error_name");

   function Get_Version return access constant Lib_Version;
   pragma Import(C, Get_Version, "libusb_get_version");

   function SetLocale(Locale: Strings.chars_ptr) return Status;
   pragma Import(C, SetLocale, "libusb_setlocale");

   function StrError(errcode: Status) return Strings.chars_ptr;
   pragma Import(C, StrError, "libusb_strerror");

   ---- Descriptors

   type Transfer_Type is (
      Transfer_Type_Control, Transfer_Type_Isochronous,
      Transfer_Type_Bulk, Transfer_Type_Interrupt,
      Transfer_Type_Bulk_Stream
   );
   for Transfer_Type use (
      0, 1,
      2, 3,
      4
   );
   pragma Convention(C, Transfer_Type);

   -- Those records corresponds to USB standard ones,
   -- except several fields added by LibUSB.
   -- Including standard record as element of this
   -- could break compatibility due to C alignment issues.
   type Endpoint_Descriptor is record
      -- Descriptor: USB.Protocol.Endpoint_Descriptor;
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      bEndpointAddress: Endpoint_Address;
      bmAttributes: Endpoint_Descriptors.Attributes;
      wMaxPacketSize: Unsigned_16;
      bInterval: Unsigned_8;
      bRefresh: Unsigned_8;
      bSynchAddress: Unsigned_8;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Endpoint_Descriptor);

   type Endpoint_Descriptor_Array is array(Integer range <>) of
    aliased Endpoint_Descriptor;

   Endpoint_Descriptor_Default_Terminator: constant Endpoint_Descriptor := (
      0, DT_Endpoint, 0, (
         Endpoint_Descriptors.Transfer_Type_Control,
         Endpoint_Descriptors.Iso_Sync_Type_None,
         Endpoint_Descriptors.Iso_Usage_Type_Data
      ), 0, 0, 0, 0,
      System.Null_Address, 0
   ); -- just for pointers to compile

   package Endpoint_Descriptor_Lists is new Interfaces.C.Pointers(
    Index => Integer,
    Element => Endpoint_Descriptor,
    Element_Array => Endpoint_Descriptor_Array,
    Default_Terminator => Endpoint_Descriptor_Default_Terminator);

   type Interface_Descriptor is record
      -- Descriptor: USB.Protocol.Interface_Descriptor;
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      bInterfaceNumber: Unsigned_8;
      bAlternateSetting: Unsigned_8;
      bNumEndpoints: Unsigned_8;
      bInterfaceClass: Class_Code;
      bInterfaceSubClass: Unsigned_8;
      bInterfaceProtocol: Unsigned_8;
      iInterface: Unsigned_8;
      Endpoint: Endpoint_Descriptor_Lists.Pointer;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Interface_Descriptor);

   type Interface_List is record
      AltSetting: Endpoint_Descriptor_Lists.Pointer;
      Num_AltSetting: Integer;
   end record;

   type Configuration_Descriptor is record
      -- Descriptor: USB.Protocol.Configuration_Descriptor;
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      wTotalLength: Unsigned_16;
      bNumInterfaces: Unsigned_8;
      bConfigurationValue: Unsigned_8;
      iConfiguration: Unsigned_8;
      bmAttributes: Configuration_Descriptors.Attributes;
      bMaxPower: Unsigned_8;
      Interface_List: LibUSB1.Interface_List;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Configuration_Descriptor);

   type Configuration_Descriptor_Access is access all Configuration_Descriptor;
   pragma Convention(C, Configuration_Descriptor_Access);

   type SS_Endpoint_Companion_Descriptor_Access is
    access all Superspeed_Endpoint_Companion_Descriptor;
   pragma Convention(C, SS_Endpoint_Companion_Descriptor_Access);

   type BOS_Descriptor_Access is access all BOS_Descriptor;
   pragma Convention(C, BOS_Descriptor_Access);

   type USB_2_0_Extension_Descriptor_Access is
    access all USB_2_0_Extension_Descriptor;
   pragma Convention(C, USB_2_0_Extension_Descriptor_Access);

   type SS_USB_Device_Capability_Descriptor_Access is
    access all Superspeed_USB_Device_Capability_Descriptor;
   pragma Convention(C, SS_USB_Device_Capability_Descriptor_Access);

   type Container_Id_Descriptor_Access is
    access all Container_Id_Descriptor;
   pragma Convention(C, Container_Id_Descriptor_Access);

   function Get_Device_Descriptor(
      Dev: Device_Access;
      Desc: out Device_Descriptor
   ) return Status;
   pragma Import(C, Get_Device_Descriptor, "libusb_get_device_descriptor");

   function Get_Active_Config_Descriptor(
      Dev: Device_Access;
      Config: access Configuration_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Active_Config_Descriptor,
      "libusb_get_active_config_descriptor");

   function Get_Config_Descriptor(
      Dev: Device_Access;
      Config_Index: Unsigned_8;
      Config: out Configuration_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Config_Descriptor, "libusb_get_config_descriptor");

   function Get_Config_Descriptor_by_Value(
      Dev: Device_Access;
      bConfigurationValue: Unsigned_8;
      Config: out Configuration_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Config_Descriptor_by_Value,
      "libusb_get_config_descriptor_by_value");

   procedure Free_Config_Descriptor(Config: Configuration_Descriptor_Access);
   pragma Import(C, Free_Config_Descriptor, "libusb_free_config_descriptor");

   function Get_SS_Endpoint_Companion_Descriptor(
      Ctx: Context_Access;
      Endpoint: access constant Endpoint_Descriptor;
      Ep_Comp: out SS_Endpoint_Companion_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_SS_Endpoint_Companion_Descriptor,
      "libusb_get_ss_endpoint_companion_descriptor");

   procedure Free_SS_Endpoint_Companion_Descriptor(
      Ep_Comp: SS_Endpoint_Companion_Descriptor_Access
   );
   pragma Import(C, Free_SS_Endpoint_Companion_Descriptor,
      "libusb_free_ss_endpoint_companion_descriptor");

   function Get_BOS_Descriptor(
      Handle: Device_Handle_Access;
      BOS: out BOS_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_BOS_Descriptor, "libusb_get_bos_descriptor");

   procedure Free_BOS_Descriptor(BOS: BOS_Descriptor_Access);
   pragma Import(C, Free_BOS_Descriptor, "libusb_free_bos_descriptor");

   function Get_USB_2_0_Extension_Descriptor(
      Ctx: Context_Access;
      Dev_Cap: access BOS_Device_Capability_Descriptor;
      USB_2_0_Extension: out USB_2_0_Extension_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_USB_2_0_Extension_Descriptor,
      "libusb_get_usb_2_0_endpoint_descriptor");

   procedure Free_USB_2_0_Extension_Descriptor(
      USB_2_0_Extension: USB_2_0_Extension_Descriptor_Access
   );
   pragma Import(C, Free_USB_2_0_Extension_Descriptor,
      "libusb_free_usb_2_0_extension_descriptor");

   function Get_SS_USB_Device_Capability_Descriptor(
      Ctx: Context_Access;
      Dev_Cap: access BOS_Device_Capability_Descriptor;
      SS_USB_Device_Cap: out SS_USB_Device_Capability_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_SS_USB_Device_Capability_Descriptor,
      "libusb_get_ss_usb_device_capability_descriptor");

   procedure Free_SS_USB_Device_Capability_Descriptor(
      SS_USB_Device_Cap: SS_USB_Device_Capability_Descriptor_Access
   );
   pragma Import(C, Free_SS_USB_Device_Capability_Descriptor,
      "libusb_free_ss_usb_device_capability_descriptor");

   function Get_Container_Id_Descriptor(
      Ctx: Context_Access;
      Dev_Cap: access BOS_Device_Capability_Descriptor;
      Container_Id: out Container_Id_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Container_Id_Descriptor,
      "libusb_get_container_id_descriptor");

   procedure Free_Container_Id_Descriptor(
      Container_Id: Container_Id_Descriptor_Access
   );
   pragma Import(C, Free_Container_id_Descriptor,
      "libusb_free_container_id_descriptor");

   function Get_String_Descriptor_ASCII(
      Dev: Device_Handle_Access;
      Desc_Index: Unsigned_8;
      Data: Char_Array;
      Length: Integer
   ) return Integer;
   pragma Import(C, Get_String_Descriptor_ASCII,
      "libusb_get_string_descriptor_ascii");

end USB.LibUSB1;
