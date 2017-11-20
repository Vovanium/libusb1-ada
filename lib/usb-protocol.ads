-- Definitions of standard USB protocol: Data structures, Values, etc
package USB.Protocol is

   -- Those types are used in structures
   type UInt8 is mod 2**8 with Size => 8;

   type UInt16 is mod 2**16 with Size => 16;

   -- Per Table 9-3 USB 3-2 spec
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
   pragma Convention(C, Request_Type);

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
      wValue: UInt16;
      wIndex: UInt16;
      wLength: UInt16;
   end record;

   -- Descriptors
   type Descriptor_Type is (
      DT_Device,      DT_Config,
      DT_String,      DT_Interface,
      DT_Endpoint,      DT_BOS,
      DT_Device_Capability,      DT_HID, -- ??
      DT_Report,      DT_Physical, -- ?? ??

      DT_Hub,      DT_Superspeed_Hub, -- ?? ??
      DT_SS_Endpoint_Companion
   );
   for Descriptor_Type use (
      1, 2,
      3, 4,
      5, 15,
      16, 16#21#,
      16#22#, 16#23#,

      16#29#, 16#2A#,
      48
   );
   for Descriptor_Type'Size use 8;


end USB.Protocol;
