program mapper;

$system tempfi

type
  link = ^link_record;
  module_string = string[8];
  pc_range = 0 .. maximum (integer);

  link_record = record
    next : link;
    module_name : module_string;
    start_pc : pc_range;
  end;

var
  map_file : text;
  link_file : text;
  bucket_file : text;
  number_buckets : integer;
  link_head : link;
  last_link : link;
  sub_task_filename : string[10];
  ASR_address : string[8];
  M_RET_AD_address : string[8];
  program_start_PC : string[8];
  program_end_PC : string[8];
$PAGE initialize
procedure initialize;

begin
  open (tty);
  rewrite (tty);
  number_buckets := 0;
  new (link_head);
  with link_head^ do begin
    next := nil;
    module_name := '';
    start_pc := minimum (pc_range);
  end (* with link_head^ *);
  last_link := link_head;
end (* initialize *);
$PAGE error
procedure error (input_string : packed array [1..*] of char);

begin
  writeln (tty, '*** Error: ' || input_string);
  stop;
end (* error *);
$PAGE convert_pc
function convert_pc (input_string : string[8]) : pc_range;

begin
  getstring (input_string, convert_pc:8:h);
end (* convert_pc *);
$PAGE get_file_names
procedure get_file_names;

var
  map_file_name : file_name;
  link_file_name : file_name;
  bucket_file_name : file_name;

begin
  write (tty, 'Pascal Map File: ');
  break (tty);
  readln (tty);
  read (tty, map_file_name);
  if map_file_name = '' then
    stop;
  reset (map_file, '.MP ' || map_file_name);
  if iostatus (map_file) <> io_ok then
    error ('Bad map file.');
  write (tty, 'Linker Map File: ');
  break (tty);
  readln (tty);
  read (tty, link_file_name);
  if link_file_name = '' then
    stop;
  reset (link_file, '.LL ' || link_file_name);
  if iostatus (link_file) <> io_ok then
    error ('Bad link file.');
  write (tty, 'Resulting Bucket File: ');
  break (tty);
  readln (tty);
  read (tty, bucket_file_name);
  if bucket_file_name = '' then
    stop;
  rewrite (bucket_file, '.BF ' || bucket_file_name);
  if iostatus (bucket_file) <> io_ok then
    error ('Bad bucket file.');
  write (tty, 'Filename of Program to Monitor: ');
  break (tty);
  readln (tty);
  read (tty, sub_task_filename);
  if sub_task_filename = '' then
    error ('No sub-task filename specified.');
end (* get_file_names *);
$PAGE find_addr
procedure find_addr (var search_file : text;
                     var result_addr : string[8];
                     input_string : string[8]);

var
  local_buffer : string[132];
  local_column : 0 .. 132;

begin
  readln (search_file, local_buffer);
  while (not eof (search_file)) andif
        (index (local_buffer, input_string) = 0) do
    readln (search_file, local_buffer);
  if eof (search_file) then
    error ('Program does not contain a reference to ' ||
           input_string || '.');
  local_column := index (local_buffer, input_string) + 11;
  result_addr := substr (local_buffer, local_column, 8);
end (* find_addr *);
$PAGE get_link_list
procedure get_link_list;

var
  local_buffer : string[132];
  local_link : link;

  procedure find_string (input_start_column : 1 .. 131;
			 input_string : string[20]);

  begin
    readln (link_file, local_buffer);
    while (not eof (link_file)) andif
          (input_start_column <> index (local_buffer, input_string)) do
      readln (link_file, local_buffer);
  end (* find_string *);

begin
  find_string (1, 'Load Map:');
  if eof (link_file) then
    error ('Linker Map does not contain a Load Map.');
  find_string (1, 'Segment SEG0:');
  if eof (link_file) then
    error ('Linked program does not contain a data section.');
  find_addr (link_file, M_RET_AD_address, 'M.RET_AD');
  find_string (1, 'Segment SEG1(R):');
  if eof (link_file) then
    error ('Linked program does not contain any resident code.');
  find_string (9, '    10     ');
  if eof (link_file) then
    error ('Linked program does not contain any Pascal code.');
  if length (local_buffer) < 28 then
    error ('Linked program does not have a starting address.');
  program_start_PC := substr (local_buffer, 20, 8);
  while not eof (link_file) do begin
    if length (local_buffer) < 28 then
      error ('Module does not have a starting address.');
    program_end_PC := substr (local_buffer, 30, 8);
    new (local_link);
    with local_link^ do begin
      next := nil;
      module_name := substr (local_buffer, 1, 8);
      start_pc := convert_pc (substr (local_buffer, 20, 8));
    end (* with local_link^ *);
    last_link^.next := local_link;
    last_link := local_link;
    find_string (9, '    10     ');
  end (* while *);
  close (link_file);
end (* get_link_list *);
$PAGE get_ASR_address
procedure get_ASR_address;

var
  task_link_file : text;
  local_buffer : string[132];

begin
  reset (task_link_file, 'TASK.LL');
  readln (task_link_file, local_buffer);
  while (not eof (task_link_file)) andif
        (index (local_buffer, 'ASR          9     ') = 0) do
    readln (task_link_file, local_buffer);
  if eof (task_link_file) then
    error ('ASR not in link file.');
  if length (local_buffer) < 28 then
    error ('ASR does not have a starting address.');
  ASR_address := substr (local_buffer, 20, 8);
  close (task_link_file);
end (* get_ASR_address *);
$PAGE write_bucket_file
procedure write_bucket_file;

var
  local_link : link;
  local_pc : pc_range;
  local_buffer : string[20];
  local_filename : file_name;
  temp_file : text;
  temp_filename : file_name;

  procedure find_module (input_string : module_string);

  begin
    close (map_file);
    reset (map_file, local_filename);
    if iostatus (map_file) <> io_ok then
      error ('Cannot re-open map file.');
    readln (map_file, local_buffer);
    while (not eof (map_file)) andif
          ((substr (local_buffer, 1, 6) <> 'Module') or
	   ((substr (local_buffer, 1, 6) = 'Module') andif
	    (substr (local_buffer, 8) <> input_string))) do
      readln (map_file, local_buffer);
  end (* find_module *);

begin
  writeln (bucket_file, uppercase (sub_task_filename));
  writeln (bucket_file, ASR_address);
  writeln (bucket_file, M_RET_AD_address);
  writeln (bucket_file, program_start_PC);
  writeln (bucket_file, program_end_PC);
  rewrite (temp_file, temp_file_name ('TM'));
  temp_filename := filename (temp_file);
  local_filename := filename (map_file);
  local_link := link_head^.next;
  while local_link <> nil do begin
    with local_link^ do begin
      find_module (module_name);
      if eof (map_file) then
        error ('Module not found in Map File: ' || module_name);
      writeln (temp_file, local_buffer);
      readln (map_file, local_buffer);
      while (not eof (map_file)) andif
            (substr (local_buffer, 1, 6) <> 'Module') do begin
        number_buckets := number_buckets + 1;
        local_pc := convert_pc (substr (local_buffer, 1, 8)) + start_pc;
        writeln (temp_file, local_pc:8:h, ' ',
                 substr (local_buffer, 10, 3));
        readln (map_file, local_buffer);
      end (* while *);
      if (length (local_buffer) > 11) andif
         (substr (local_buffer, 1, 6) <> 'Module') then begin
        number_buckets := number_buckets + 1;
	local_pc := convert_pc (substr (local_buffer, 1, 8)) + start_pc;
	writeln (temp_file, local_pc:8:h, ' ',
		 substr (local_buffer, 10, 3));
      end;
    end (* with local_link^ *);
    local_link := local_link^.next;
  end (* while *);
  close (temp_file);
  reset (temp_file, temp_filename);
  writeln (bucket_file, number_buckets);
  while not eof (temp_file) do begin
    readln (temp_file, local_buffer);
    writeln (bucket_file, local_buffer);
  end (* while *);
  scratch (temp_file);
  close (bucket_file);
end (* write_bucket_file *);
$PAGE main
begin
  initialize;
  get_file_names;
  get_link_list;
  get_ASR_address;
  write_bucket_file;
end.
 