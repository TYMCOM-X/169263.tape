program directory_check;
$PAGE declarations and includes
$include (pasdev2)query.inc
const
  tab = chr (11b);
  blank = ' ';
  tabs = tab||tab||tab||tab||tab||tab||tab||tab||tab||tab||tab||tab||tab||tab;
  comma = ',';

type
  file_ptr = ^ file_record;
  file_record = record
    fil: text;
    number: integer;
    next: file_ptr;
    directory: string[16];
    name, data: string[16]
  end;

var
  first, last: file_ptr;
  file_count: integer;
  directory_count: integer;
  xrefile: text;
$PAGE initialization
procedure initialization;
begin
  first := nil;
  last := nil;
  file_count := 0;
  directory_count := 0;
  xrefile := nilf;
  rewrite (tty);
  open (tty);
end;
$PAGE get_line
procedure get_line (f: file_ptr);
var s: string;
begin
  with f^ do begin
    readln (fil);
    if eof (fil) then begin
      close (fil);
      fil := nilf;
    end
    else begin
      read (fil,s);
      name := substr (s, 1, 11);
      data := substr (s, 18);
    end;
  end;
end;
$PAGE get_directories
procedure get_directories;
var
  s1, s2: string;
  f: file_ptr;
  infile: text;
  i: integer;
begin
  writeln (tty,'Enter directory names without parentheses.');
  loop
    writeln (tty);
    write (tty,'Directory: '); break;
    readln (tty);
  exit if eoln (tty);
    read (tty,s1);
    write (tty,'File list from: '); break;
    readln (tty);
    read (tty,s2);
    open (infile,'.lst ' || s2);
    if eof (infile) then
      writeln (tty,'Can''t open file.')
    else begin
      new (f);
      with f^ do begin
	directory_count := directory_count + 1;
	number := directory_count;
	fil := infile;
	next := nil;
	infile := nilf;
	if first = nil
	  then first := f
	  else last^.next := f;
	last := f;
	directory := s1;
	for i := 1 to 5 do
	  readln (fil);
	get_line (f);
      end;
    end;
  end;
end;
$PAGE get_output_file
function get_output_file (prompt: string[*]): text;
var
  s: string;
begin
  loop
    write (tty,prompt,' file: '); break;
    readln (tty);
    read (tty,s);
    rewrite (get_output_file, '.lst ' || s);
  exit if eof (get_output_file);
    writeln (tty,'Can''t create file.');
  end;
end;
$PAGE do_headers
procedure do_headers;
const
  init_header = 'Name' || tab || 'ext';
  info_header = tab || 'pages' || '  date';
  xref_header = tab || 'In directories';
var
  ix: integer;
  f: file_ptr;
begin
  f := first;
  write (tab,tab);
  while f <> nil do with f^ do begin
    ix := (14 - length (directory)) div 2;
    write (blank:ix, directory, blank: 16- length(directory) - ix);
    f := next;
  end;
  writeln;
  write (init_header);
  for ix := 1 to directory_count do
    write (info_header);
  writeln;
  if xrefile <> nilf then
    writeln (xrefile,init_header,xref_header);
end;
$PAGE list_files
procedure list_files;
var
  f1, f2: file_ptr;
  lix: integer;
  s: string;
  i, j: integer;
begin
  loop
    f1 := first;
    f2 := nil;
    s := '';
    while f1 <> nil do with f1^ do begin
      if fil <> nilf then
	if (f2 = nil) orif (name < f2^.name) then
	  f2 := f1;
      f1 := next;
    end;
  exit if f2 = nil;
    file_count := file_count + 1;
    write (f2^.name);
    lix := f2^.number;
    write (substr(tabs,1,2*(lix-1)+1));
    write (blank,f2^.data);
    if xrefile <> nilf then
      write (xrefile,f2^.name,tab,f2^.directory);
    f1 := f2^.next;
    while f1 <> nil do begin
      if f1^.name = f2^.name then begin
	write (substr (tabs,1,2*(f1^.number-lix-1)+1));
	write (blank,f1^.data);
	if xrefile <> nilf then
	  write (xrefile,comma,f1^.directory);
	get_line (f1);
	lix := f1^.number;
      end;
      f1 := f1^.next;
    end;
    writeln;
    if xrefile <> nilf then
      writeln (xrefile);
    get_line (f2);
  end;
end;
$PAGE summary
procedure summary;
begin
  writeln (tty);
  writeln (tty,file_count,' distinct filenames were found in',directory_count,
	' directories.');
end;
$PAGE cleanup
procedure cleanup;
var f1, f2: file_ptr;
begin
  close (output);
  if xrefile <> nilf then
    close (xrefile);
  f1 := first;
  while f1 <> nil do begin
    f2 := f1^.next;
    dispose (f1);
  f1 := f2;
  end;
end;
$PAGE mainline
begin
  initialization;
  get_directories;
  output := get_output_file ('Output');
  if query ('Xref') then
    xrefile := get_output_file ('Xref');
  do_headers;
  list_files;
  summary;
  cleanup;
end.
   