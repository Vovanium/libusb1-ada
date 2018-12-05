# Libusb bindings notes and progress

## General notes

### How libusb bindings are organized?

Bindings for libusb1 library are placed in ada package USB.Libusb1.
All its entities are imported using their corresponding names,
however several changes are made:
- prefix "libusb_" is removed from all entities;
- in every enumeration, common prefix to its value is removed;
- pointer types got "_Access" suffix;
- pointer array pointers implemented through instantion Interfaces.C.Pointers with "_Lists" suffix;
- opaque pointers are implemented using System.Address;
- several individual changes are made, see list below.

## Progress

- [ ] Module Library initialization/deinitialization
  - [ ] Types
    - [ ] version
    - [x] context (opaque)
    - [x] log_level
    - [ ] option
  - [ ] Functions
    - [x] init (under name Lib_Init)
    - [x] exit (under name Lib_Exit)
    - [x] set_debug
    - [ ] set_option (vararg function, so c wrappers are required)
- [ ] Module Device handling and enumeration
  - [ ] Types
    - [x] device (opaque)
    - [x] device_handle (opaque)
    - [x] speed
    - [ ] supported_speed
    - [ ] usb_2_0_extension_attributes
    - [ ] ss_usb_device_capability_attributes
    - [ ] bos_type
  - [ ] Functions
    - [x] get_device_list
    - [x] free_device_list
    - [x] get_bus_number
    - [x] get_port_number
    - [x] get_port_numbers
    - [x] get_port_path
    - [x] get_parent
    - [x] get_device_address
    - [x] get_device_speed
    - [x] get_max_packet_size
    - [x] get_max_iso_packet_size
    - [x] ref_device
    - [x] unref_device
    - [x] open
    - [x] open_device_with_vid_pid
    - [x] close
    - [x] get_device
    - [x] get_cofiguration
    - [x] set_configuration
    - [x] claim_interface
    - [x] release_interface
    - [x] set_interface_alt_setting
    - [x] clear_halt
    - [x] reset_device
    - [x] kernel_driver_active
    - [x] detach_kerel_driver
    - [x] attach_kernel_driver
    - [x] set_auto_detach_kernel_driver
- [ ] Module USB descriptors
- [ ] Module Synchronous device I/O
- [ ] Module Asynchronous device I/O
- [ ] Module Polling and timing
- [ ] Module Device hotplug event notification
- [ ] Module Miscellaneous