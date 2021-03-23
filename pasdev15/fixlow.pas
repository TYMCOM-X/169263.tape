program fixlow options special (word);

const
  max_loseg := 57777b;

type
  loseg = array [0..max_loseg] of machine_word;
  losegfile = file of *;
  twohalfwords = packed record
    case boolean of
      true: (lh: -1..377777b; rh: -1..377777b);
      false: (fw: machine_word)
    end;

var
  lofile: losegfile;
  ls: loseg;
  control: twohalfwords;
  origin, start, count, last: machine_word;
  s: file_name;

label 1;
$PAGE doit
procedure doit;

(* DOIT does the whole thing.  It first expands the low segment into
   the monstrous array.  Then we compress it by replacing the last
   zero word in a block of zeroes with the control word for the
   following non-zero words (via a backwards scan).  Then we just
   write out every non-zero word. *)

begin
  s := filename (lofile);
  for count := 0 to max_loseg do ls[count] := 0;

  read (lofile, control);
  origin := control.rh;
  start := origin;

  repeat
    count := - (control.lh);
  exit if (count < 0) do begin
    ls [start - origin] := control.fw;  (* copy garbage word *)
    count := 0                          (* as if we read one *)
    end;
  exit if (start-origin+count) > max_loseg do begin
    writeln (tty, '?Loseg too big for program.');
    goto 1
    end;

    start := control.rh + 1;
    read (lofile, ls [start - origin]: count);
    start := start + count;
    read (lofile, control)
  until eof (lofile);

  close (lofile);
  last := start - origin + count - 1;  (* the last word written *)
  count := last + 1;
  rewrite (lofile, s);
  if iostatus (lofile) <> io_ok then
    begin
    writeln (tty, '?Can''t rewrite loseg file.');
    goto 1
    end;

  repeat
    start := last;
    while (start > 0) andif (ls [start] <> 0) do
      start := start - 1;
    control.lh := start - last;
    control.rh := start + origin;
    ls [start] := control.fw;
  exit if start = 0;
    last := start - 1;
    while (last > 0) andif (ls [last] = 0) do
      last := last - 1
  until last = 0;

  for start := 0 to count do
    if ls [start] <> 0 then write (lofile, ls[start]);

  close (lofile)
end (* procedure doit *);
$PAGE main
begin
  rewrite (ttyoutput); open (tty);
  1: loop
    write (tty, '*'); break (tty); readln (tty);
    read (tty, s);
  exit if s = '';
    reset (lofile, '.LOW ' || s);
    if eof (lofile) then writeln (tty, 'Can''t find lofile.')
    else doit
  end
end.
