(*  SCAN FILE NAME attempts to extract a file name from the command line.  If
    it fails, then it will signal an error.  WhatFile is the kind of file name
    being scanned (e.g., 'Source', 'Indirect', etc.).  NameRequired is true if
    a full file name is required, and false if any part of a file name (e.g.,
    just an extension) is enough.  *)

function scan_file_name ( what_file: parm_string; name_required: boolean ): file_name;

var ch: char;

type
    charset = set of ' '..'g';

const
    name_chars = ['A'..'Z', '0'..'9', '#'];
$PAGE scan, scan_dir, scan_prot - in scan_file_name
function scan (chars: charset): boolean;
begin
   scan := false;
   cmd_idx := cmd_idx + 1;
   while cmd_idx <= length (command_line) do begin
     ch := uppercase (command_line[cmd_idx]);
     if not (ch in chars) then return;
     scan := true;
     cmd_idx := cmd_idx + 1
   end;
   ch := ' '
end (* scan *);

procedure scan_directory; (* Scan the directory. *)
var cnt: 0..6;
    ok: boolean;
begin
  if scan (['0'..'7']) then ;
  ok := (ch = ',');
  if ok then begin
    if scan (['0'..'7']) then ;
    cnt := 0; (* Scan subfile directories. *)
    while ok and (ch = ',') do begin
      ok := scan (name_chars) and (cnt < 6);
      cnt := cnt + 1
    end;
    ok := ok and (ch = ']');
  end;
  if not ok then
    cmd_error ('BFD', error_context);
  if scan ([]) then ; (* Step over the ']'. *)
end (* scan_directory *);

procedure scan_protection; (* Scan protection code. *)
begin
  if not ( scan (['0'..'7']) andif (ch = '>') ) then
    cmd_error ('BFP', error_context);
  if scan ([]) then ; (* Step over the '>'. *)
end (* scan_protection *);
$PAGE scan_file_name - main routine
var start: integer;

begin
  skip_blanks;
  start := cmd_idx;
  cmd_idx := cmd_idx - 1; (* Ready for Scan. *)

  if scan (name_chars) then begin (* Have device or file name. *)
    if ch = ':' then (* Have device name, get file name. *)
      if scan (name_chars) then ;
  end
  else if name_required then (* File name missing. *)
    cmd_error ('FNE', what_file);

  if ch = '.' then begin (* Get extension. *)
    if scan (name_chars) then ;
  end;

  error_context := 'file ' || uppercase (substr (command_line, start, cmd_idx - start));

  if ch = '[' then begin (* Get directory (and protection). *)
    scan_directory;
    if ch = '<' then
      scan_protection;
  end

  else if ch = '<' then begin (* Get protection (and directory). *)
    scan_protection;
    if ch = '[' then
      scan_directory;
  end;

  scan_file_name := uppercase (substr (command_line, start, cmd_idx - start));
end (* scan_file_name *);
