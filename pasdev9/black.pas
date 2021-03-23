program black;
var
  seed: real;
  i, j, k, rtime: integer;
  encoding: boolean;
  str: string;
$system (pasdev2)cmdutl.typ
$system (pasdev2)query.inc
external function unmask (integer; real): integer;
external function mask (integer; real): integer;

$PAGE get_seed
function get_seed (str: string[*]): real;
var x: real;
    i, j: integer;
begin
  get_seed := 0;
  for i := 1 to length (str) do begin
    get_seed := get_seed + sqrt (i * ord (uppercase (str[i])));
  end;
  get_seed := sqrt (get_seed);
end;
$PAGE process

procedure process (name: string[*]);
var
  x: real;
  f: file of integer;
  fn: file_name;
begin
  fn := name || ' .pas';
  reset (f, fn);
  if iostatus (f) <> io_ok then begin
    writeln (tty,'Can''t open file ',name); break;
    return;
  end;
  close (f);
  update (f, fn);
  if iostatus (f) <> io_ok then begin
    writeln (tty,'Can''t update file ',name); break;
    return;
  end;
  fn := filename (f);
  write (tty,fn); break (tty);
$if ziff  i := index (fn,'>');
$ifnot ziff  i := index (fn,':');
  j := index (substr (fn,i+1),'.');
  fn := substr (fn,i+1,j-1);
  x := seed + get_seed (fn);
  for i := 1 to extent (f) do begin
    seek (f,i);
    x := random (x);
    if encoding
      then f^ := mask (f^,x)
      else f^ := unmask (f^,x);
    put (f);
  end;
  close (f);
  writeln (tty); break;
end;
$PAGE filelist

procedure filelist (var f: text);
var
  f2: text;
  s: string;
begin
  while not eof (f) do begin
    if f = tty then begin
      write (tty,'File: '); break;
    end;
    readln (f);
  exit if eoln (f);
    read (f,s);
  exit if s = '';
    if s[1] = '@' then begin
      open (f2,'.cmd '||substr(s,2));
      if eof (f2) then begin
	writeln (tty,'Can''t open command file ',substr(s,2));
	break;
      end
      else begin
	filelist (f2);
	close (f2);
      end;
    end
    else begin
      process (s);
    end;
  end;
end;
$PAGE mainline
begin
  rewrite (tty); open (tty);
  loop
    write (tty,'Password: '); break;
    readln (tty); read (tty,str);
  exit if str <> '';
    writeln (tty,'What?');
  end;
  seed := get_seed (str);
  encoding := query ('Encode');
  rtime := runtime;
  filelist (tty);
  rtime := runtime - rtime;
  writeln (tty,rtime/1000:7:3,' seconds used.'); break;
end.
