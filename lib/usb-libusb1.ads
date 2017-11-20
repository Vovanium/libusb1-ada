with Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;
with System;
with USB.Protocol;

package USB.LibUSB1 is
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

   ---- Miscellaneus

   API_Version: constant Integer := 16#01000104#;


   ---- Descriptors

   type Class_Code is (
      Class_Per_Interface,  Class_Audio,
      Class_Comm,           Class_HID,
      Class_Physical,       Class_Image,
      Class_Printer,        Class_Mass_Storage,
      Class_Hub,            Class_Data,

      Class_Smart_Card,     Class_Content_Security,
      Class_Video,          Class_Personal_Healthcare,
      Class_Diagnostic_Device, Class_Wireless,
      Class_Application,    Class_Vendor_Spec
   );
   for Class_Code use (
      16#00#, 16#01#,
      16#02#, 16#03#,
      16#05#, 16#06#,
      16#07#, 16#08#,
      16#09#, 16#0A#,

      16#0B#, 16#0D#,
      16#0E#, 16#0F#,
      16#DC#, 16#E0#,
      16#FE#, 16#FF#
   );
   for Class_Code'Size use 8;
   -- pragma Convention(C, Class_Code);

   type UInt8 is range 0..2**8-1;
   for UInt8'Size use 8;
   pragma Convention(C, UInt8);

   type UInt16 is range 0..2**16-1;
   for UInt16'Size use 16;
   pragma Convention(C, UInt16);

   -- !!!should be record (7 bit number + 1 bit direction)
   type Endpoint_Address is mod 2**8;
   for Endpoint_Address'Size use 8;
   pragma Convention(C, Endpoint_Address);

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

   type Iso_Sync_Type is (
      Iso_Sync_Type_None, Iso_Sync_Type_Async,
      Iso_Sync_Type_Adaptive, Iso_Sync_Type_Sync
   );
   for Iso_Sync_Type use (
      0, 1,
      2, 3
   );
   pragma Convention(C, Iso_Sync_Type);

   type Iso_Usage_Type is (
      Iso_Usage_Type_Data, Iso_Usage_Type_Feedback,
      Iso_Usage_Type_Implicit
   );
   pragma Convention(C, Iso_Usage_Type);



   type Device_Descriptor is record
      Length: UInt8;
      bDescriptorType: Descriptor_Type;
      bcdUSB: UInt16;
      bDeviceClass: Class_Code;
      bDeviceSubClass: UInt8; -- Should be distinct type
      bDeviceProtocol: UInt8; -- Should be distinct type
      bMaxPacketSize0: UInt8;
      idVendor: UInt16;
      idProduct: UInt16;
      bcdDevice: UInt16;
      iManufacturer: UInt8;
      iProduct: UInt8;
      iSerialNumber: UInt8;
      bNumConfigurations: UInt8;
   end record;
   pragma Convention(C, Device_Descriptor);

   type Endpoint_Attributes is record
      Transfer_Type: USB.LibUSB1.Transfer_Type
       range Transfer_Type_Control..Transfer_Type_Interrupt;
      Iso_Sync_Type: USB.LibUSB1.Iso_Sync_Type;
      Iso_Usage_Type: USB.LibUSB1.Iso_Usage_Type;
   end record;
   for Endpoint_Attributes use record
      Transfer_Type at 0 range 0..1;
      Iso_Sync_Type at 0 range 2..3;
      Iso_Usage_Type at 0 range 4..5;
   end record;
   for Endpoint_Attributes'Size use 8;
   pragma Convention(C, Endpoint_Attributes);

   type Endpoint_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bEndpointAddress: Endpoint_Address;
      bmAttributes: Endpoint_Attributes;
      wMaxPacketSize: UInt16;
      bInterval: UInt8;
      bRefresh: UInt8;
      bSynchAddress: UInt8;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Endpoint_Descriptor);

   type Endpoint_Descriptor_Array is array(Integer range <>) of
    aliased Endpoint_Descriptor;

   Endpoint_Descriptor_Default_Terminator: constant Endpoint_Descriptor := (
      0, DT_Endpoint, 0, (
         Transfer_Type_Control, Iso_Sync_Type_None, Iso_Usage_Type_Data
      ), 0, 0, 0, 0,
      System.Null_Address, 0
   ); -- just for pointers to compile

   package Endpoint_Descriptor_Lists is new Interfaces.C.Pointers(
    Index => Integer,
    Element => Endpoint_Descriptor,
    Element_Array => Endpoint_Descriptor_Array,
    Default_Terminator => Endpoint_Descriptor_Default_Terminator);

   type Interface_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bInterfaceNumber: UInt8;
      bAlternateSetting: UInt8;
      bNumEndpoints: UInt8;
      bInterfaceClass: Class_Code;
      bInterfaceSubClass: UInt8;
      bInterfaceProtocol: UInt8;
      iInterface: UInt8;
      Endpoint: Endpoint_Descriptor_Lists.Pointer;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Interface_Descriptor);

   type Interface_List is record
      AltSetting: Endpoint_Descriptor_Lists.Pointer;
      Num_AltSetting: Integer;
   end record;

   type Config_Descriptor is record
      bLength: UInt8;
      bDescriptorType: USB.Protocol.Descriptor_Type;
      wTotalLength: UInt16;
      bNumInterfaces: UInt8;
      bConfigurationValue: UInt8;
      iConfiguration: UInt8;
      Attributes: UInt8;
      MaxPower: Uint8;
      Interface_List: USB.LibUSB1.Interface_List;
      Extra: System.Address;
      Extra_Length: Integer;
   end record;
   pragma Convention(C, Config_Descriptor);

   type Config_Descriptor_Access is access all Config_Descriptor;
   pragma Convention(C, Config_Descriptor_Access);

   type SS_Endpoint_Companion_Attributes is mod 2**8;
   for SS_Endpoint_Companion_Attributes'Size use 8;
   pragma Convention(C, SS_Endpoint_Companion_Attributes);

   type SS_Endpoint_Companion_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bMaxBurst: UInt8;
      bmAttributes: SS_Endpoint_Companion_Attributes;
      wBytesPerInterval: UInt16;
   end record;
   pragma Convention(C, SS_Endpoint_Companion_Descriptor);

   type SS_Endpoint_Companion_Descriptor_Access is
    access all SS_Endpoint_Companion_Descriptor;
   pragma Convention(C, SS_Endpoint_Companion_Descriptor_Access);

   type BOS_Dev_Capability_Data_Array is array (0..-1) of UInt8;
   pragma Convention(C, BOS_Dev_Capability_Data_Array);

   type BOS_Dev_Capability_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bDevCapabilityType: UInt8;
      Dev_Capability_Data: BOS_Dev_Capability_Data_Array;
   end record;
   pragma Convention(C, BOS_Dev_Capability_Descriptor);

   type BOS_Dev_Capability_Descriptor_Array is array (0..-1)
    of BOS_Dev_Capability_Descriptor;
   pragma Convention(C, BOS_Dev_Capability_Descriptor_Array);

   type BOS_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      wTotalLength: UInt16;
      bNumDeviceCaps: UInt8;
      Dev_Capability: BOS_Dev_Capability_Descriptor_Array;
   end record;
   pragma Convention(C, BOS_Descriptor);

   type BOS_Descriptor_Access is access all BOS_Descriptor;
   pragma Convention(C, BOS_Descriptor_Access);

   pragma Warnings (Off, "24 bits of ""USB_2_0_Extension_Attributes"" unused");
   type USB_2_0_Extension_Attributes is record
      LPM_Support: Boolean;
   end record;
   for USB_2_0_Extension_Attributes use record
      LPM_Support at 0 range 1..1;
   end record;
   for USB_2_0_Extension_Attributes'Size use 32;
   pragma Convention(C, USB_2_0_Extension_Attributes);
   pragma Warnings (On, "24 bits of ""USB_2_0_Extension_Attributes"" unused");

   type USB_2_0_Extension_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bDevCapabilityType: UInt8;
      bmAttributes: USB_2_0_Extension_Attributes;
   end record;
   pragma Convention(C, USB_2_0_Extension_Descriptor);

   type USB_2_0_Extension_Descriptor_Access is
    access all USB_2_0_Extension_Descriptor;
   pragma Convention(C, USB_2_0_Extension_Descriptor_Access);

   type SS_USB_Device_Capability_Attributes is record
      LTM_Support: Boolean;
   end record;
   for SS_USB_Device_Capability_Attributes use record
      LTM_Support at 0 range 1..1;
   end record;
   for SS_USB_Device_Capability_Attributes'Size use 8;
   pragma Convention(C, SS_USB_Device_Capability_Attributes);

   type SS_USB_Device_Capability_Speed_Supported is mod 2**16;
   for SS_USB_Device_Capability_Speed_Supported'Size use 16;
   pragma Convention(C, SS_USB_Device_Capability_Speed_Supported);

   type SS_USB_Device_Capability_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bDevCapabilityType: UInt8;
      bmAttributes: SS_USB_Device_Capability_Attributes;
      wSpeedSupported: SS_USB_Device_Capability_Speed_Supported;
      bFunctionalitySupport: UInt8;
      bU1DevExitLat: UInt8;
      bU2DevExitLat: UInt16;
   end record;
   pragma Convention(C, SS_USB_Device_Capability_Descriptor);

   type SS_USB_Device_Capability_Descriptor_Access is
    access all SS_USB_Device_Capability_Descriptor;
   pragma Convention(C, SS_USB_Device_Capability_Descriptor_Access);

   type UUID is array (0..15) of UInt8;

   type Container_Id_Descriptor is record
      bLength: UInt8;
      bDescriptorType: Descriptor_Type;
      bDevCapabilityType: UInt8;
      bReserved: UInt8;
      ContainerID: UUID;
   end record;
   pragma Convention(C, Container_Id_Descriptor);

   type Container_Id_Descriptor_Access is access all Container_Id_Descriptor;
   pragma Convention(C, Container_Id_Descriptor_Access);

   function Get_Device_Descriptor(
      Dev: Device_Access;
      Desc: access Device_Descriptor
   ) return Status;
   pragma Import(C, Get_Device_Descriptor, "libusb_get_device_descriptor");

   function Get_Active_Config_Descriptor(
      Dev: Device_Access;
      Config: access Config_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Active_Config_Descriptor,
      "libusb_get_active_config_descriptor");

   function Get_Config_Descriptor(
      Dev: Device_Access;
      Config_Index: UInt8;
      Config: access Config_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Config_Descriptor, "libusb_get_config_descriptor");

   function Get_Config_Descriptor_by_Value(
      Dev: Device_Access;
      bConfigurationValue: UInt8;
      Config: access Config_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_Config_Descriptor_by_Value,
      "libusb_get_config_descriptor_by_value");

   procedure Free_Config_Descriptor(Config: Config_Descriptor_Access);
   pragma Import(C, Free_Config_Descriptor, "libusb_free_config_descriptor");

   function Get_SS_Endpoint_Companion_Descriptor(
      Ctx: Context_Access;
      Endpoint: access Endpoint_Descriptor;
      Ep_Comp: access SS_Endpoint_Companion_Descriptor_Access
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
      BOS: access BOS_Descriptor_Access
   ) return Status;
   pragma Import(C, Get_BOS_Descriptor, "libusb_get_bos_descriptor");

   procedure Free_BOS_Descriptor(BOS: BOS_Descriptor_Access);
   pragma Import(C, Free_BOS_Descriptor, "libusb_free_bos_descriptor");

   function Get_USB_2_0_Extension_Descriptor(
      Ctx: Context_Access;
      Dev_Cap: access BOS_Dev_Capability_Descriptor;
      USB_2_0_Extension: access USB_2_0_Extension_Descriptor_Access
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
      Dev_Cap: access BOS_Dev_Capability_Descriptor;
      SS_USB_Device_Cap: access SS_USB_Device_Capability_Descriptor_Access
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
      Dev_Cap: access BOS_Dev_Capability_Descriptor;
      Container_Id: access Container_Id_Descriptor_Access
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
      Desc_Index: UInt8;
      Data: Char_Array;
      Length: Integer
   ) return Integer;
   pragma Import(C, Get_String_Descriptor_ASCII,
      "libusb_get_string_descriptor_ascii");

end USB.LibUSB1;
