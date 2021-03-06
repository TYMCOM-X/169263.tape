module getiofiles;

$SYSTEM cmdutl

public function getiofiles (
	var infile : text;
	    in_ext : packed array [1..3] of char;
	var outfile : text;
	    out_ext : packed array [1..3] of char
	) : boolean;

var line: string [256];
    i: integer;
    in_name, out_name: file_name;

begin
  repeat
    cmd_getline ('*', line, i);
  exit if cmd_eol (line, i) do getiofiles := false;
    getiofiles := cmd_file_name (line, i, true, out_name);
    if getiofiles then begin
      if cmd_eol (line, i) then
	in_name := out_name
      else if cmd_check_punct (line, i, '=') then
	getiofiles := cmd_file_name (line, i, true, in_name) andif cmd_eol (line, i)
      else
	getiofiles := false;
    end;
    if not getiofiles then
      writeln (tty, '?Command syntax is "[output file =] input file".');

    if getiofiles then begin
      reset (infile, '.' || in_ext || ' ' || in_name);
      if iostatus <> io_ok then begin
	writeln (tty, '?Unable to read ', in_name);
	getiofiles := false;
      end;
    end;
    if getiofiles then begin
      rewrite (outfile, '.' || out_ext || ' ' || out_name);
      if iostatus <> io_ok then begin
	writeln (tty, '?Unable to create ', out_name);
	getiofiles := false;
      end;
    end;
  until getiofiles;
end (* getiofiles *).
  