program stat;

$SYSTEM pascal
$SYSTEM pasfil

type
  one_byte    = 0..2 ** 8 - 1;
  two_bytes   = 0..2 ** 16 - 1;
  three_bytes = 0..2 ** 24 - 1;
  four_bytes  = 0..2 ** 32 - 1;
  fake_word   = packed array [1..4] of one_byte;
  string_10 = string[10];
  ESDID_range = 17..255;
  ESDID_record = record
    name : string_10;
    count : 0..maximum (integer);
  end;

var
  tty_flag : boolean          (* output to terminal or file? *);
  id_flag : boolean           (* identification record encountered yet? *);
  end_flag : boolean          (* end record encountered yet? *);
  current_ESDID : ESDID_range (* ESDID count for XREF's *);
  byte_ptr : integer          (* which byte of input_word to get next *);
  byte_count : integer        (* number of bytes remaining in current logical record *);
  input_word : fake_word      (* current four bytes read in from the input file *);
  input_file : file of fake_word;
  output_file : file_block;
  ESDID_array : array [ESDID_range] of ESDID_record;
$PAGE initialize
procedure initialize;

var
  local_index : ESDID_range;

begin
  open (tty);
  rewrite (tty);
  tty_flag := true;
  id_flag := false;
  end_flag := false;
  current_ESDID := minimum (ESDID_range);
  byte_ptr := 1;
  for local_index := minimum (ESDID_range) to maximum (ESDID_range) do begin
    with ESDID_array[local_index] do begin
      name := '';
      count := 0;
    end (* with ESDID_array[local_index] *);
  end (* for *);
end (* initialize *);
$PAGE write_string
procedure write_string (input_string : string[*]);

begin
  if tty_flag then
    writeln (tty, input_string)
  else
    fio_line (output_file, input_string);
end (* write_string *);
$PAGE convert_decimal
function convert_decimal (input_number : integer) : string_10;

begin
  putstring (convert_decimal, input_number:0);
end (* convert_decimal *);
$PAGE write_header
procedure write_header (var fb : file_block);

begin
  fio_line (fb, 'M68000 Runtime call statistics for file: ' || 
	    filename (input_file) || 
	    '         Page ' || 
	    convert_decimal (fb.pageno));
  fio_skip (fb);
end (* write_header *);
$PAGE warning
procedure warning (input_warning_message : string[*]);

begin
  writeln (tty, '*** Warning: ' || input_warning_message);
  if not tty_flag then begin
    fio_page (output_file);
    write_string ('*** Warning: ' || input_warning_message);
  end;
end (* warning *);
$PAGE error
procedure error (input_error_message : string[*]);

begin
  writeln (tty, '*** Error: ' || input_error_message);
  if not tty_flag then begin
    fio_page (output_file);
    write_string ('*** Error: ' || input_error_message);
  end;
  close;
  stop;
end (* error *);
$PAGE check_eof
procedure check_eof;

begin
  if eof (input_file) then
    error ('Unexpected EOF.');
end (* check_eof *);
$PAGE read_one_byte
procedure read_one_byte (var output_one_byte : one_byte);

begin
  output_one_byte := input_word[byte_ptr];
  byte_ptr := byte_ptr + 1;
  if byte_ptr = 5 then begin
    if eof (input_file) then
      input_word := (0, 0, 0, 0)
    else
      read (input_file, input_word);
    byte_ptr := 1;
  end;
end (* read_one_byte *);
$PAGE read_two_bytes
procedure read_two_bytes (var output_two_bytes : two_bytes);

var
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_two_bytes := local_one_byte;
  read_one_byte (local_one_byte);
  check_eof;
  output_two_bytes := output_two_bytes * 256 + local_one_byte;
end (* read_two_bytes *);
$PAGE read_three_bytes
procedure read_three_bytes (var output_three_bytes : three_bytes);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_three_bytes := local_one_byte;
  for local_counter := 1 to 2 do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_three_bytes := output_three_bytes * 256 + local_one_byte;
  end;
end (* read_three_bytes *);
$PAGE read_four_bytes
procedure read_four_bytes (var output_four_bytes : four_bytes);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  read_one_byte (local_one_byte);
  check_eof;
  output_four_bytes := local_one_byte;
  for local_counter := 1 to 3 do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_four_bytes := output_four_bytes * 256 + local_one_byte;
  end;
end (* read_four_bytes *);
$PAGE read_string
procedure read_string (input_length : one_byte;
                       var output_string : string[*]);

var
  local_counter : one_byte;
  local_one_byte : one_byte;

begin
  output_string := '';
  for local_counter := 1 to input_length do begin
    read_one_byte (local_one_byte);
    check_eof;
    output_string := output_string || chr (local_one_byte);
  end;
end (* read_string *);
$PAGE process_identification_record
procedure process_identification_record;

var
  local_string : string[255];

begin
  if end_flag then
    warning ('Record encountered after End Record.');
  read_string (byte_count - 1, local_string);
  check_eof;
  id_flag := true;
end (* process_identification_record *);
$PAGE process_ESD_record
procedure process_ESD_record;

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;
  local_string : string_10;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  if end_flag then
    warning ('Record encountered after End Record.');
  read_one_byte (local_one_byte);
  check_eof;
  case local_one_byte div 16 of

    2 : begin
	  read_four_bytes (local_four_bytes);
	end;

    4 : begin
	  read_string (10, local_string);
	  read_four_bytes (local_four_bytes);
	end;

    7 : begin
	  read_string (10, local_string);
          ESDID_array[current_ESDID].name := local_string;
          current_ESDID := current_ESDID + 1;
	end;

    others : error ('Illegal ESD record type.');

  end (* case *);
end (* process_ESD_record *);
$PAGE get_bits
function get_bits (input_byte : one_byte;
		   input_last_bit : 0..7;
		   input_number_bits : one_byte) : one_byte;

var
  local_counter : one_byte;
  local_bit : one_byte;
  local_byte : one_byte;

begin
  get_bits := 0;
  local_byte := input_byte;
  for local_counter := 0 to 7 do begin
    local_bit := local_byte mod 2;
    local_byte := local_byte div 2;
    if (local_counter >= input_last_bit) and
       (local_counter < input_last_bit + input_number_bits) then
      get_bits := get_bits + (local_bit * 2 ** (local_counter - input_last_bit));
  end (* for *);
end (* get_bits *);
$PAGE decrement_check_byte_count
procedure decrement_check_byte_count (input_amount : integer);

begin
  byte_count := byte_count - input_amount;
  if byte_count < 0 then
    error ('Incorrect Text Record byte count.');
end (* decrement_check_byte_count *);
$PAGE process_absolute_code
procedure process_absolute_code (input_counter : 0..32);

var
  local_two_bytes : two_bytes;

begin
  read_two_bytes (local_two_bytes);
  decrement_check_byte_count (2);
end (* process_absolute_code *);
$PAGE process_relocation_data
procedure process_relocation_data (input_counter : 0..32);

var
  local_counter : one_byte;
  local_num_ESDID : one_byte;
  local_offset_length : one_byte;
  local_one_byte : one_byte;
  local_two_bytes : two_bytes;
  local_three_bytes : three_bytes;
  local_four_bytes : four_bytes;

begin
  read_one_byte (local_one_byte);
  check_eof;
  decrement_check_byte_count (1);
  local_num_ESDID := get_bits (local_one_byte, 5, 3);
  local_offset_length := get_bits (local_one_byte, 0, 3);
  for local_counter := 1 to local_num_ESDID do begin
    read_one_byte (local_one_byte);
    check_eof;
    decrement_check_byte_count (1);
    if local_one_byte >= minimum (ESDID_range) then
      ESDID_array[local_one_byte].count := ESDID_array[local_one_byte].count + 1;
  end (* for *);
  case local_offset_length of
    0 : ;
    1 : read_one_byte (local_one_byte);
    2 : read_two_bytes (local_two_bytes);
    3 : read_three_bytes (local_three_bytes);
    4 : read_four_bytes (local_four_bytes);
    others : error ('Illegal offset length.');
  end (* case *);
  decrement_check_byte_count (local_offset_length);
end (* process_relocation_data *);
$PAGE process_text_record
procedure process_text_record;

var
  local_counter : 0..32;
  local_map : array [1..4] of one_byte;
  local_ESDID : one_byte;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  if end_flag then
    warning ('Record encountered after End Record.');
  decrement_check_byte_count (1);
  for local_counter := 1 to 4 do begin
    read_one_byte (local_map[local_counter]);
    check_eof;
    decrement_check_byte_count (1);
  end (* for *);
  read_one_byte (local_ESDID);
  check_eof;
  decrement_check_byte_count (1);
  local_counter := 0;
  while (local_counter < 32) and
	 (byte_count > 0) do begin
    if get_bits (local_map[ (local_counter div 8) + 1], 
		 7 - (local_counter mod 8), 1) = 0 then
      process_absolute_code (local_counter)
    else
      process_relocation_data (local_counter);
    local_counter := local_counter + 1;
  end (* while *);
  if byte_count <> 0 then
    error ('Incorrect Text Record byte count.');
end (* process_text_record *);
$PAGE process_end_record
procedure process_end_record;

var
  local_one_byte : one_byte;
  local_four_bytes : four_bytes;

begin
  if not id_flag then
    warning ('Record encountered before Identification Record.');
  read_one_byte (local_one_byte);
  check_eof;
  if local_one_byte < 17 then
    read_four_bytes (local_four_bytes);
  end_flag := true;
end (* process_end_record *);
$PAGE process_input
procedure process_input;

var
  local_one_byte : one_byte;

begin
  read_one_byte (byte_count);
  while not eof (input_file) do begin
    if byte_count > 0 then begin
      read_one_byte (local_one_byte);
      check_eof;
      case local_one_byte of
	ord ('1') : process_identification_record;
	ord ('2') : process_ESD_record;
	ord ('3') : process_text_record;
	ord ('4') : process_end_record;
	others    : error ('Illegal record type.');
      end (* case *);
    end (* if *);
    read_one_byte (byte_count);
  end (* while *);
  if not id_flag then
    warning ('Missing Identification Record.');
  if not end_flag then
    warning ('Missing End Record.');
end (* process_input *);
$PAGE print_ESDID_statistics
procedure print_ESDID_statistics;

var
  local_index : ESDID_range;

begin
  local_index := minimum (ESDID_range);
  write_string ('Runtime Statistics:');
  write_string ('');
  write_string ('Name        Calls');
  write_string ('----------  -----');
  write_string ('');
  while (local_index <= maximum (ESDID_range)) andif
        (ESDID_array[local_index].name <> '') do begin
    with ESDID_array[local_index] do 
      write_string (name || '  ' || convert_decimal (count));
    local_index := local_index + 1;
  end (* while *);
end (* print_ESDID_statistics *);
$PAGE get_file_names
procedure get_file_names;

var
  input_file_name : file_name;
  output_file_name : file_name;

begin
  writeln (tty, 'M68000 Runtime call statistics program, Version 1.0');
  write (tty, 'Input file: ');
  break (tty);
  readln (tty);
  read (tty, input_file_name);
  if input_file_name = '' then
    stop;  (* <--- No input file *)
  reset (input_file, '.RO ' || input_file_name, [retry]);
  if iostatus (input_file) <> io_ok then
    error ('Bad input file.');
  write (tty, 'Output file: ');
  break (tty);
  readln (tty);
  read (tty, output_file_name);
  if output_file_name <> ' ' then begin
    fio_open (output_file, '.LS ' || output_file_name);
    if iostatus <> io_ok then
      error ('Bad output file.');
    with output_file do begin
      page_header := write_header;
      width := 120;
      plength := 42;
    end (* with output_file *);
    fio_page (output_file);
    tty_flag := false;
  end;
  read (input_file, input_word);
end (* get_file_names *);
$PAGE main
begin
  initialize;
  get_file_names;
  process_input;
  print_ESDID_statistics;
  close;
end.
    