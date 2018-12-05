with Interfaces;
use Interfaces;
-- Definitions of standard USB protocol: Data structures, Values, etc
package USB.Protocol is

   -- Those types are used in structures

   -- Per Table 9-3 USB 3-2 rev 1.0
   type Request_Type_Code is (
      Request_Type_Standard, Request_Type_Class,
      Request_Type_Vendor, Request_Type_Reserved
   );
   for Request_Type_Code use (
      16#00#, 16#01#,
      16#02#, 16#03#
   );

   type Request_Recipient is (
      Recipient_Device, Recipient_Interface,
      Recipient_Endpoint, Recipient_Other,
      Recipient_Vendor_Specific
   );
   for Request_Recipient use (
      0, 1,
      2, 3,
      31
   );
   type Transfer_Direction is (
      Direction_Host_to_Device, Direction_Device_to_Host
   );
   for Transfer_Direction use (0, 1);

   type Request_Type is record
      Recipient: Request_Recipient;
      Code: Request_Type_Code;
      Direction: Transfer_Direction;
   end record;
   for Request_Type use record
      Recipient at 0 range 0..4;
      Code at 0 range 5..6;
      Direction at 0 range 7..7;
   end record;
   for Request_Type'Size use 8;
   pragma Convention(C_Pass_by_Copy, Request_Type);

   type Request_Code is mod 2**8;
   for Request_Code'Size use 8;

   -- Standard requests
   type Standard_Request_Code is (
      Request_Get_Status, Request_Clear_Feature,
      Request_Set_Feature,
      Request_Set_Address,
      Request_Get_Descriptor, Request_Set_Descriptor,
      Request_Get_Configuration, Request_Set_Configuration,

      Request_Get_Interface, Request_Set_Interface,
      Request_Synch_Frame, Request_Set_Sel,
      Request_Isoch_Delay
   );
   for Standard_Request_Code use (
      0, 1,
      3,
      5,
      6, 7,
      8, 9,
      16#0A#, 16#0B#,

      16#0C#, 16#30#,
      16#31#
   );

   type Control_Setup is record
      bmRequestType: Request_Type;
      bRequest: Request_Code;
      wValue: Unsigned_16;
      wIndex: Unsigned_16;
      wLength: Unsigned_16;
   end record;

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
   ) with Size => 8;
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

   -- Per Table 9-6 of USB 3.2 rev 1.0
   type Descriptor_Type is (
      DT_Device,      DT_Config,
      DT_String,      DT_Interface,
      DT_Endpoint,      DT_BOS,
      DT_Device_Capability,      DT_HID, -- ??
      DT_Report,      DT_Physical, -- ?? ??

      DT_Hub,      DT_Superspeed_Hub, -- ?? ??
      DT_SS_Endpoint_Companion
   ) with Size => 8;
   for Descriptor_Type use (
      1, 2,
      3, 4,
      5, 15,
      16, 16#21#,
      16#22#, 16#23#,

      16#29#, 16#2A#,
      48
   );

   type Vendor_Id is mod 2**16;
   pragma Convention(C, Vendor_Id);

   type Product_Id is mod 2**16;
   pragma Convention(C, Product_Id);

   -- Per Table 9-11 of USB 3.2 rev 1.0
   type Device_Descriptor is record
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      bcdUSB: Unsigned_16;
      bDeviceClass: Class_Code;
      bDeviceSubClass: Unsigned_8; -- Should be distinct type
      bDeviceProtocol: Unsigned_8; -- Should be distinct type
      bMaxPacketSize0: Unsigned_8;
      idVendor: Vendor_Id;
      idProduct: Product_Id;
      bcdDevice: Unsigned_16;
      iManufacturer: Unsigned_8;
      iProduct: Unsigned_8;
      iSerialNumber: Unsigned_8;
      bNumConfigurations: Unsigned_8;
   end record with Size => 18*8;
   pragma Convention(C, Device_Descriptor);

   -- Per Table 9-12 of USB 3.2 rev 1.0
   type BOS_Descriptor is record
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      wTotalLength: Unsigned_16;
      bNumDeviceCaps: Unsigned_8;
      -- Dev_Capability: BOS_Dev_Capability_Descriptor_Array;
   end record with Size => 5*8;
   pragma Convention(C, BOS_Descriptor);

   -- Per Table 9-14 of USB 3.2 rev 1.0
   type BOS_Device_Capability_Type is (
      Wireless_USB,
      USB_2_0_Extension, Superspeed_USB,
      Container_Id, Platform,
      Power_Delivery_Capability, Battery_Info_Capability,

      PD_Consumer_Port_Capability, PD_Provider_Port_Capability,
      Superspeed_Plus, Precision_Time_Meaurement,
      Wireless_USB_Ext, Billboard,
      Authentication, Billboard_Ex,

      Configuration_Summary
   ) with Size => 8;
   for BOS_Device_Capability_Type use (
      16#01#,
      16#02#, 16#03#,
      16#04#, 16#05#,
      16#06#, 16#07#,

      16#08#, 16#09#,
      16#0A#, 16#0B#,
      16#0C#, 16#0D#,
      16#0E#, 16#0F#,

      16#10#
   );

   -- Per Table 9-13 of USB 3.2 rev 1.0
   type BOS_Device_Capability_Descriptor is record
      bLength: Unsigned_8;
      bDescriptorType: Descriptor_Type;
      bDevCapabilityType: BOS_Device_Capability_Type;
      -- Dev_Capability_Data: BOS_Dev_Capability_Data_Array;
   end record with Size => 3*8;
   pragma Convention(C, BOS_Device_Capability_Descriptor);

   -- Per Table 9-15 of USB 3.2 rev 1.0
   package USB_2_0_Extension_Descriptors is
      pragma Warnings (Off, "24 bits of ""Attributes"" unused");
      type Attributes is record
         LPM: Boolean;
      end record with Size => 32;
      for Attributes use record
         LPM at 0 range 1..1;
      end record;
      pragma Convention(C_Pass_by_Copy, Attributes);
      pragma Warnings (On, "24 bits of ""Attributes"" unused");

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bDevCapabilityType: BOS_Device_Capability_Type;
         bmAttributes: Attributes;
      end record with Size => 7*8;
      pragma Convention(C, Descriptor);
   end USB_2_0_Extension_Descriptors;

   subtype USB_2_0_Extension_Descriptor
    is USB_2_0_Extension_Descriptors.Descriptor;

   -- Per Table 9-16 of USB 2.3 rev 1.0
   package Superspeed_USB_Device_Capability_Descriptors is
      type Attributes is record
         LTM_Capable: Boolean;
      end record;
      for Attributes use record
         LTM_Capable at 0 range 1..1;
      end record;
      for Attributes'Size use 8;
      pragma Convention(C_Pass_by_Copy, Attributes);

      pragma Warnings (Off, "8 bits of ""Speeds_Supported"" unused");
      type Speeds_Supported is record
         Low: Boolean;
         Full: Boolean;
         High: Boolean;
         Gen1: Boolean;
      end record;
      for Speeds_Supported use record
         Low at 0 range 0..0;
         Full at 0 range 1..1;
         High at 0 range 2..2;
         Gen1 at 0 range 3..3;
      end record;
      for Speeds_Supported'Size use 16;
      pragma Convention(C_Pass_by_Copy, Speeds_Supported);
      pragma Warnings (On, "8 bits of ""Speeds_Supported"" unused");

      type Functionality_Support is (
         Low_Speed,
         Full_Speed,
         High_Speed,
         Gen1_Speed
      ) with Size => 8;
      for Functionality_Support use (
         0, 1, 2, 3
      );

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bDevCapabilityType: BOS_Device_Capability_Type;
         bmAttributes: Attributes;
         wSpeedsSupported: Speeds_Supported;
         bFunctionalitySupport: Functionality_Support;
         bU1DevExitLat: Unsigned_8;
         bU2DevExitLat: Unsigned_16;
      end record with Size => 10*8;
      pragma Convention(C, Descriptor);
   end Superspeed_USB_Device_Capability_Descriptors;

   subtype Superspeed_USB_Device_Capability_Descriptor
    is Superspeed_USB_Device_Capability_Descriptors.Descriptor;

   -- Per Table 9-17 of USB 2.3 rev 1.0
   package Container_Id_Descriptors is
      type UUID is array (0..15) of Unsigned_8;

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bDevCapabilityType: BOS_Device_Capability_Type;
         bReserved: Unsigned_8;
         ContainerID: UUID;
      end record with Size => 20*8;
      pragma Convention(C, Descriptor);
   end Container_Id_Descriptors;

   subtype Container_Id_Descriptor is Container_Id_Descriptors.Descriptor;

   -- Per Table 9-22 of USB 2.3 rev 1.0
   package Configuration_Descriptors is
      type Attributes is record
         Remote_Wakeup: Boolean;
         Self_Powered: Boolean;
         Set_True: Boolean;
      end record with Size => 8;
      for Attributes use record
         Remote_Wakeup at 0 range 5..5;
         Self_Powered at 0 range 6..6;
         Set_True at 0 range 7..7;
      end record;
      pragma Convention(C_Pass_by_Copy, Attributes);

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         wTotalLength: Unsigned_16;
         bNumInterfaces: Unsigned_8;
         bConfigurationValue: Unsigned_8;
         iConfiguration: Unsigned_8;
         bmAttributes: Attributes;
         bMaxPower: Unsigned_8;
      end record with Size => 9*8;
   end Configuration_Descriptors;

   subtype Configuration_Descriptor is Configuration_Descriptors.Descriptor;

   -- Per Table 9-24 of USB 2.3 rev 1.0
   package Interface_Descriptors is
      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bInterfaceNumber: Unsigned_8;
         bAlternateSetting: Unsigned_8;
         bNumEndpoints: Unsigned_8;
         bInterfaceClass: Class_Code;
         bInterfaceSubClass: Unsigned_8;
         bInterfaceProtocol: Unsigned_8;
         iInterface: Unsigned_8;
      end record with Size => 9*8;
   end Interface_Descriptors;

   subtype Interface_Descriptor is Interface_Descriptors.Descriptor;

   -- !!!should be record (7 bit number + 1 bit direction)
   type Endpoint_Address is mod 2**8;
   for Endpoint_Address'Size use 8;
   pragma Convention(C, Endpoint_Address);

   -- Per Table 9-25 of USB 2.3 rev 1.0
   package Endpoint_Descriptors is
      type Transfer_Type is (
         Transfer_Type_Control, Transfer_Type_Isochronous,
         Transfer_Type_Bulk, Transfer_Type_Interrupt
      );
      for Transfer_Type use (
         0, 1,
         2, 3
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

      type Attributes is record
         Transfer_Type: Endpoint_Descriptors.Transfer_Type;
         Iso_Sync_Type: Endpoint_Descriptors.Iso_Sync_Type;
         Iso_Usage_Type: Endpoint_Descriptors.Iso_Usage_Type;
      end record with Size => 8;
      for Attributes use record
         Transfer_Type at 0 range 0..1;
         Iso_Sync_Type at 0 range 2..3;
         Iso_Usage_Type at 0 range 4..5;
      end record;
      pragma Convention(C_Pass_by_Copy, Attributes);

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bEndpointAddress: Endpoint_Address;
         bmAttributes: Attributes;
         wMaxPacketSize: Unsigned_16;
         bInterval: Unsigned_8;
      end record with Size => 7*8;
      pragma Convention(C, Descriptor);
   end Endpoint_Descriptors;

   subtype Endpoint_Descriptor is Endpoint_Descriptors.Descriptor;

   -- Per Table 9-27 of USB 2.3 rev 1.0
   package Superspeed_Endpoint_Companion_Descriptors is
      type Attributes is mod 2**8;
      for Attributes'Size use 8;
      pragma Convention(C, Attributes);

      type Descriptor is record
         bLength: Unsigned_8;
         bDescriptorType: Descriptor_Type;
         bMaxBurst: Unsigned_8;
         bmAttributes: Attributes;
         wBytesPerInterval: Unsigned_16;
      end record with Size => 6*8;
      pragma Convention(C, Descriptor);
   end Superspeed_Endpoint_Companion_Descriptors;

   subtype Superspeed_Endpoint_Companion_Descriptor is
    Superspeed_Endpoint_Companion_Descriptors.Descriptor;

end USB.Protocol;

