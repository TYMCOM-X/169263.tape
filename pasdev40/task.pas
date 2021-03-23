program task options special (ptr, coercions);

$SYSTEM trap1
$SYSTEM trap3
$SYSTEM trap4
$SYSTEM inireg

const
  null = chr (0);
  four_nulls = null || null || null || null;
  eight_nulls = four_nulls || four_nulls;

type
  one_byte = 0 .. 2 ** 8 - 1;
  two_bytes = 0 .. 2 ** 16 - 1;
  four_bytes = integer;
  two_chars = packed array [1 .. 2] of char;
  four_chars = packed array [1 .. 4] of char;
  eight_chars = packed array [1 .. 8] of char;

var
  return_code : four_bytes;
  DELAY_amount : four_bytes;
  step_amount : four_bytes;
  step_count : four_bytes;
  ASR_address : four_bytes;
  M_RET_AD_address : four_bytes;
  program_start_PC : four_bytes;
  trace_flag : boolean;
  single_step_flag : boolean;
  monitor_taskname : four_chars;
  sub_task_taskname : four_chars;
  sub_task_filename : eight_chars;
  sub_task_session : four_chars;
  bucket_file : text;
  result_file : text;

public var
  flag : boolean;
$PAGE error
procedure error (input_message : packed array [1..*] of char);

begin
  writeln (tty, '<Monitor> Error: ', input_message);
  stop;
end (* error *);
$PAGE abort
public procedure abort (input_message : packed array [1..*] of char);

begin
  writeln (tty, '<Monitor> Abort: ', input_message,
	   ', Return code = ', return_code:8:h);
  stop;
end (* abort *);
$PAGE get_file_names
procedure get_file_names;

var
  bucket_file_name : file_name;
  result_file_name : file_name;

begin
  write (tty, '<Monitor> Bucket File: ');
  break (tty);
  readln (tty);
  read (tty, bucket_file_name);
  if bucket_file_name = '' then
    stop;
  reset (bucket_file, '.BF ' || bucket_file_name);
  if iostatus (bucket_file) <> io_ok then
    error ('Bad bucket file.');
  write (tty, '<Monitor> Resulting Histogram File: ');
  break (tty);
  readln (tty);
  read (tty, result_file_name);
  if result_file_name = '' then
    stop;
  rewrite (result_file, '.HG ' || result_file_name);
  if iostatus (result_file) <> io_ok then
    error ('Bad result file.');
end (* get_file_names *);
$PAGE get_parameters
procedure get_parameters;

var
  local_flag : boolean;
  local_buffer : string[80];

begin
  local_flag := false;
  while not local_flag do begin
    write (tty, '<Monitor> Single Step or Delay Sampling mode (SS/DS): ');
    break (tty);
    readln (tty);
    read (tty, local_buffer);
    local_buffer := uppercase (local_buffer);
    if (length (local_buffer) >= 2) andif
       ((substr (local_buffer, 1, 2) = 'DS') or
	(substr (local_buffer, 1, 2) = 'SS')) then
      local_flag := true
    else
      writeln (tty, '<Monitor> Error in input, please enter "SS" or "DS".');
  end (* while *);
  if substr (local_buffer, 1, 2) = 'SS' then begin
    single_step_flag := true;
    local_flag := false;
    while not local_flag do begin
      write (tty, '<Monitor> Step increment (1-1000): ');
      break (tty);
      readln (tty);
      read (tty, local_buffer);
      getstring (local_buffer, step_amount);
      if (step_amount < 1) or
         (step_amount > 1000) then
        writeln (tty, '<Monitor> Error in input, please enter a number between ',
                 '1 and 1000.')
      else
        local_flag := true;
    end (* while *);
  end
  else begin
    single_step_flag := false;
    local_flag := false;
    while not local_flag do begin
      write (tty, '<Monitor> Delay increment, in milliseconds (1-1000): ');
      break (tty);
      readln (tty);
      read (tty, local_buffer);
      getstring (local_buffer, DELAY_amount);
      if (DELAY_amount < 1) or
         (DELAY_amount > 1000) then
        writeln (tty, '<Monitor> Error in input, please enter a number between ',
                 '1 and 1000.')
      else
        local_flag := true;
    end (* while *);
  end (* else *);
  local_flag := false;
  while not local_flag do begin
    write (tty, '<Monitor> Trace mode (Y/N): ');
    break (tty);
    readln (tty);
    read (tty, local_buffer);
    local_buffer := uppercase (local_buffer);
    if (length (local_buffer) >= 1) andif
       ((substr (local_buffer, 1, 1) = 'Y') or
	(substr (local_buffer, 1, 1) = 'N')) then
      local_flag := true
    else
      writeln (tty, '<Monitor> Error in input, please enter "Y" or "N".');
  end (* while *);
  trace_flag := false;
  if substr (local_buffer, 1, 1) = 'Y' then
    trace_flag := true;
end (* get_parameters *);
$PAGE set_up_buckets
procedure set_up_buckets;

begin
end (* set_up_buckets *);
$PAGE trace
procedure trace (input_name : string[8];
                 input_before_flag : boolean);

begin
  if not trace_flag then
    return;
  if input_before_flag then
    writeln (tty, '<Monitor> Before ', input_name)
  else
    writeln (tty, '<Monitor> After  ', input_name);
end (* trace *);
$PAGE GTASQ
procedure GTASQ;

var
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    status : one_byte;
    message_length : one_byte;
    queue_length : four_bytes;
    ASR : four_bytes;
    receiving_area : four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := monitor_taskname;
    session := 0;
    status := #h05;
    message_length := 255;
    queue_length := 100;
    ASR := ASR_address;
    receiving_area := 0;
  end (* with parameter_area *);
  trace ('GTASQ', true);
  if not trap1 (31, address (parameter_area), return_code) then
    abort ('GTASQ');
  trace ('GTASQ', false);
end (* GTASQ *);
$PAGE LOAD
procedure LOAD;

var
  local_file_name : file_name;
  local_user_number : four_bytes;
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    CRTCB_options : two_bytes;
    monitor_taskname : four_chars;
    monitor_session : four_bytes;
    initial_priority : one_byte;
    limit_priority : one_byte;
    task_attributes : two_bytes;
    task_entry_point : four_bytes;
    user_id : two_chars;
    LOAD_options : two_bytes;
    command_line_length : one_byte;
    LUN_load_file : one_byte;
    reserved : eight_chars;
    command_line_address : four_bytes;
    FHS_code : one_byte;
    FHS_command : one_byte;
    FHS_options : two_bytes;
    FHS_status : one_byte;
    FHS_LUN : one_byte;
    FHS_volume_name : four_chars;
    FHS_user_number : two_bytes;
    FHS_catalog_name : eight_chars;
    FHS_file_name : eight_chars;
    FHS_extension : two_chars;
    FHS_reserved : two_bytes;
    FHS_write_code : one_byte;
    FHS_read_code : one_byte;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    CRTCB_options := #h8000;
    monitor_taskname := four_nulls;
    monitor_session := 0;
    initial_priority := 0;
    limit_priority := 0;
    task_attributes := 0;
    task_entry_point := 0;
    user_id := substr (sub_task_taskname, 1, 2);
    LOAD_options := #h400C;
    command_line_length := 0;
    LUN_load_file := 0;
    reserved := eight_nulls;
    command_line_address := 0;
    FHS_code := 0;
    FHS_command := 0;
    FHS_options := 0;
    FHS_status := 0;
    FHS_LUN := 0;
    FHS_volume_name := 'SYS ';
    local_file_name := filename (bucket_file);
    getstring (substr (local_file_name, 5, 4), local_user_number);
    FHS_user_number := local_user_number;
    FHS_catalog_name := '        ';
    FHS_file_name := sub_task_filename;
    FHS_extension := 'LO';
    FHS_reserved := 0;
    FHS_write_code := 0;
    FHS_read_code := 0;
  end (* with parameter_area *);
  trace ('LOAD', true);
  if not trap4 (1, address (parameter_area), return_code) then
    abort ('LOAD');
  trace ('LOAD', false);
end (* LOAD *);
$PAGE change_LUN
procedure change_LUN (input_LUN : one_byte);

var
  parameter_area : packed record
    FHS_code : one_byte;
    FHS_command : one_byte;
    FHS_options : two_bytes;
    FHS_status : one_byte;
    FHS_old_LUN : one_byte;
    FHS_new_LUN : one_byte;
    FHS_filler : one_byte;
    FHS_taskname : four_chars;
    FHS_session : four_chars;
  end;

begin
  with parameter_area do begin
    FHS_code := #h01;
    FHS_command := #h10;
    FHS_options := #h8000;
    FHS_status := 0;
    FHS_old_LUN := input_LUN;
    FHS_new_LUN := input_LUN;
    FHS_filler := 0;
    FHS_taskname := sub_task_taskname;
    FHS_session := sub_task_session;
  end (* with parameter_area *);
  trace ('LUN', true);
  if not trap3 (address (parameter_area), return_code) then
    abort ('LUN');
  trace ('LUN', false);
end (* change_LUN *);
$PAGE START
procedure START;

var
  local_record : record
    case boolean of
      true  : (char_var : four_chars);
      false : (byte_var : four_bytes);
  end;
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    START_options : two_bytes;
    monitor_taskname : four_chars;
    monitor_session : four_bytes;
    registers : array [1..16] of four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    START_options := #hA000;
    monitor_taskname := four_nulls;
    monitor_session := 0;
    registers := inireg;
    with local_record do begin
      char_var := sub_task_taskname;
      registers[9] := byte_var (* A0 gets taskname *);
      byte_var := registers[2] (* get session number from D1 *);
      sub_task_session := char_var;
    end (* with local_record *);
    change_LUN (5);
    change_LUN (6);
  end (* with parameter_area *);
  trace ('START', true);
  if not trap1 (13, address (parameter_area), return_code) then
    abort ('START');
  trace ('START', false);
end (* START *);
$PAGE EXMON
procedure EXMON;

var
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    EXMON_taskname : four_chars;
    EXMON_session : four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    EXMON_taskname := monitor_taskname;
    EXMON_session := 0;
  end (* with parameter_area *);
  trace ('EXMON', true);
  if not trap1 (64, address (parameter_area), return_code) then
    abort ('EXMON');
  trace ('EXMON', false);
end (* EXMON *);
$PAGE EXMMSK
procedure EXMMSK;

var
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    EXMON_mask : four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    EXMON_mask := #h10000000;
  end (* with parameter_area *);
  trace ('EXMMSK', true);
  if not trap1 (66, address (parameter_area), return_code) then
    abort ('EXMMSK');
  trace ('EXMMSK', false);
end (* EXMMSK *);
$PAGE REXMON
procedure REXMON;

var
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    buffer_address : ptr;
  end;
  buffer : packed record
    execution_options : two_bytes;
    value_location : four_bytes;
    value : four_bytes;
    value_mask : four_bytes;
    max_instruction_count : four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    buffer_address := address (buffer);
  end (* with parameter_area *);
  with buffer do begin
    execution_options := 0;
    value_location := 0;
    value := 0;
    value_mask := 0;
    max_instruction_count := 0;
    if single_step_flag then
      execution_options := #h1000;
  end (* with buffer *);
  trace ('REXMON', true);
  if not trap1 (69, address (parameter_area), return_code) then
    abort ('REXMON');
  trace ('REXMON', false);
end (* REXMON *);
$PAGE DELAY
procedure DELAY;

begin
  trace ('DELAY', true);
  if not trap1 (21, ptr (DELAY_amount), return_code) then
    abort ('DELAY');
  trace ('DELAY', false);
end (* DELAY *);
$PAGE initialize
procedure initialize;

var
  local_buffer : string[10];

begin
  get_file_names;
  get_parameters;
  readln (bucket_file, local_buffer);
  sub_task_filename := substr (local_buffer || '        ', 1, 8);
  if sub_task_filename = '' then
    error ('No filename specified.');
  readln (bucket_file, ASR_address:8:h);
  readln (bucket_file, M_RET_AD_address:8:h);
  readln (bucket_file, program_start_PC:8:h);
  monitor_taskname := 'TASK';
  sub_task_taskname := substr (sub_task_filename, 1, 4);
  flag := true;
  set_up_buckets;
  GTASQ;
  LOAD;
  EXMON;
  EXMMSK;
  START;
  REXMON;
end (* initialize *);
$PAGE RSTATE
procedure RSTATE (var curr_PC : four_bytes);

var
  parameter_area : packed record
    taskname : four_chars;
    session : four_bytes;
    buffer_address : ptr;
    registers : array [1..13] of four_bytes;
    PC : four_bytes;
    SR : two_bytes;
    EXMON_mask : four_bytes;
    task_status : four_bytes;
    execution_options : two_bytes;
    value_location : four_bytes;
    value : four_bytes;
    value_mask : four_bytes;
    max_instruction_count : four_bytes;
  end;

begin
  with parameter_area do begin
    taskname := sub_task_taskname;
    session := 0;
    buffer_address := address (parameter_area);
  end (* with parameter_area *);
  trace ('RSTATE', true);
  if not trap1 (67, address (parameter_area), return_code) then
    abort ('RSTATE');
  curr_PC := parameter_area.PC;
  trace ('RSTATE', false);
end (* RSTATE *);
$PAGE MOVELL
procedure MOVELL (var curr_PC : four_bytes);

var
  local_PC : four_bytes;
  parameter_area : packed record
    source_taskname : four_chars;
    source_session : four_bytes;
    source_address : four_bytes;
    dest_taskname : four_chars;
    dest_session : four_bytes;
    dest_address : ptr;
    data_length : four_bytes;
  end;

begin
  with parameter_area do begin
    source_taskname := sub_task_taskname;
    source_session := 0;
    source_address := M_RET_AD_address;
    dest_taskname := monitor_taskname;
    dest_session := 0;
    dest_address := address (local_PC);
    data_length := 4;
  end (* with parameter_area *);
  trace ('MOVELL', true);
  if not trap1 (6, address (parameter_area), return_code) then
    abort ('MOVELL');
  trace ('MOVELL', false);
  curr_PC := local_PC;
end (* MOVELL *);
$PAGE read_PC
procedure read_PC;

var
  local_PC : four_bytes;

begin
  local_PC := 0;
  RSTATE (local_PC);
  write (result_file, local_PC:8:h);
  if local_PC < program_start_PC then begin
    local_PC := 0;
    MOVELL (local_PC);
    write (result_file, '  ', local_PC:8:h);
  end (* if *);
  writeln (result_file);
end (* read_PC *);
$PAGE main
begin
  initialize;
  while flag do begin
    read_PC;
    if single_step_flag then begin
      for step_count := 1 to step_amount do
	REXMON;
    end
    else
      DELAY;
  end (* while *);
end.
   