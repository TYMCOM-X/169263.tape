(*  MAKE HELP FILE is a program to construct the binary help file used by
    the Pascal compiler when processing a /HELP command.  The help file has
    the following format:

	index cursor (integer)
	number of index entries (integer)
	one or more text entries
	index

    A text entry has the format:

	zero or more lines
	end mark

    A line has the format:

	length (integer)
	text (packed array [1..length] of char)

    An end mark is an integer with a value of -1.

    The index is an array [1..number of index entries] of cmd_lookup_record.

    The value in each cmd_lookup_record in the index is the cursor for
    accessing the corresponding text entry.

    The input file from which the help file is constructed has the format:

	text entry for /HELP command with no parameters
	one or more named text entries

    A named text entry is a line "\name" followed by a text entry; a text
    entry is any number of lines which don't begin with "\".  *)

program make_help_file;
$PAGE declarations
$SYSTEM (pasdev2)cmdutl

type
    index_record = record
	next: ^ index_record;
	content: cmd_lookup_record;
    end;

var help_file: file of *;
    index: ^ index_record;
    index_cursor, index_entries: integer;
$PAGE write_line
procedure write_line (line: string [80]);

type
    str = packed array [1..*] of char;

var len: integer;
    text: packed array [1..80] of char;

begin
  len := length (line);
  text := line;
  write (help_file, len, text: size (str, len));
end;
$PAGE open_files
procedure open_files;

var source_file_name: file_name;

begin
  open (tty);
  rewrite (tty);
  writeln (tty, 'Pascal help file builder');
  loop
    write (tty, 'Source file? ');
    break;
    readln (tty);
    read (tty, source_file_name);
    if source_file_name = '' then
      stop;
    reset (input, source_file_name);
  exit if iostatus = io_ok;
    writeln (tty, 'Can''t open ', source_file_name);
  end;
  rewrite (help_file, 'PASCAL.HLP', [seekok]);
  assert (iostatus = io_ok);
end;
$PAGE copy_text
procedure copy_text;

var text_line: string [80];
    last: ^ index_record;

const end_marker: integer = -1;

begin
  new (index);
  last := index;
  last^ := (nil, ('*', 1, cursor (help_file)));
  index_entries := 1;

  while not eof (input) do begin
    readln (input, text_line);
    if (text_line = '') orif (text_line[1] <> '\') then
      write_line (text_line)
    else begin
      write (help_file, end_marker);
      new (last^.next);
      last := last^.next;
      last^ := (nil, (substr (text_line, 2), length (text_line) - 1, cursor (help_file)));
      index_entries := index_entries + 1;
    end;
  end;
  write (help_file, end_marker);
end;
$PAGE make_index
procedure make_index;

var index_table: ^ array [1..*] of cmd_lookup_record;
    i: integer;

begin
  new (index_table, index_entries);
  for i := 1 to index_entries do begin
    index_table^[i] := index^.content;
    index := index^.next;
  end;
  index_cursor := cursor (help_file);
  write (help_file, index_table^: size (index_table^, index_entries));
  writern (help_file, 1, index_cursor, index_entries);
end;
$PAGE make_help_file - main program
begin
  open_files;
  index_cursor := 0;
  index_entries := 0;
  write (help_file, index_cursor, index_entries);
  copy_text;
  make_index;
end.
   