external procedure mac_header;
external procedure mac_list (arealist: code_list; init_ic: code_address);
external procedure mac_pad (unit_range);
external procedure mac_end (startdef: def; code_size, const_size, static_size: unit_range);
type
  cvhex_string = string[9];
external function cvhex (int_type): cvhex_string;
  