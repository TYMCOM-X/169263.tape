$options sp, noch, main
$include rlb:dtime.typ
$include rlb:dtime.inc
$include rlb:pasdir.typ
$include rlb:pasdir.inc
const tab := chr (11b);
      header := 'FILE    EXT  PROT    SIZE   BLOCKS         CREATED           LAST USED';
var
  s: string; i: integer;
  filecount: integer;
  total_size, total_blocks: integer;
  summary: string;
  heapmark: integer;
  error: dir_errors;
  id: dir_int_id;
  extent: dir_ext_id;
  ch: char;
  outfile: string;
  filename: dir_fname;
  attr: dir_attrs;

type
  nameptr = ^namerec;
  namerec = packed record
    nleft, nright: nameptr;
    attrs: dir_attrs
  end;
  listptr = ^listrec;
  listrec = packed record
    lleft, lright: listptr;
    name: nameptr
  end;
  extptr = ^extrec;
  extrec = packed record
    xleft, xright: extptr;
    ext: packed array[1..3] of char;
    namelist: listptr
  end;

var
  headname: nameptr;
  headext: extptr;

function tostring (n: integer): string;
begin
  tostring := '';
  repeat
    tostring := chr(n mod 10 + ord ('0')) || tostring;
    n := n div 10;
  until n = 0;
end;
procedure addname;
var ln, n: nameptr; newext: packed array[1..3] of char;
    lx, x: extptr; newname: packed array[1..6] of char;
    ll, l: listptr;
    ss: string[10];
begin
  filecount := filecount + 1;
  n := headname; ln := nil;
  dir_attr (error, filename || extent, attr);
  total_size := total_size + attr.size;
  total_blocks := total_blocks + (attr.size+127) div 128;
  while n <> nil do with n^ do begin
    ln := n;
    if attrs.name > attr.name
      then n := nleft
      else n := nright;
  end;
  new (n);
  with n^ do begin
    nleft := nil;
    nright := nil;
    attrs := attr;
  end;
  if ln = nil then headname := n
  else if ln^.attrs.name > attr.name then ln^.nleft := n
  else ln^.nright := n;

  x := headext; lx := nil;
  newext := substr (attr.name,7,3);
  while x <> nil do with x^ do begin
    lx := x;
  exit if x^.ext = newext;
    if x^.ext > newext then x := xleft
    else x := xright;
  end;
  if x = nil then begin
    new (x);
    with x^ do begin
      xleft := nil;
      xright := nil;
      ext := newext;
      namelist := nil;
    end;
    if lx = nil then headext := x
    else if lx^.ext > newext then lx^.xleft := x
    else lx^.xright := x;
  end;
  l := x^.namelist; ll := nil;
  newname := substr (attr.name,1,6);
  while l <> nil do with l^ do begin
    ll := l;
    if substr (name^.attrs.name,1,6) > newname then l := lleft
    else l := lright;
  end;
  new (l);
  with l^ do begin
    lleft := nil;
    lright := nil;
    name := n;
  end;
  if ll = nil then x^.namelist := l
  else if substr (ll^.name^.attrs.name,1,6) > newname then ll^.lleft := l
  else ll^.lright := l;
end;

procedure detail (n: nameptr);
var ss: string[30];
begin
  with n^.attrs do begin
    write (substr(name,1,6),tab,substr(name,7,3),'  <',protect,'>',size:7,' ',(size+127)div 128:6,' ':4);
    ss := ns_d2 (extr_date(creation));
    if ss[6] = ',' then ss := substr(ss,1,4)||' '||substr(ss,5)
    else ss := ss || ' ';
    write (ss);
    ss := ns_t1(extr_time(creation));
    if ss[1] = '0' then ss[1] := ' ';
    ss := substr(ss,1,5)|| lowercase(substr(ss,9));
    write (ss,' ':3);
    ss := ns_d2 (extr_date(accessed));
    if ss[6] = ',' then ss := substr(ss,1,4)||' '||substr(ss,5);
    writeln (ss);
  end;
end;

procedure print_alpha_names (n: nameptr);
begin
  if n <> nil then with n^ do begin
    print_alpha_names (nleft);
    detail (n);
    print_alpha_names (nright);
  end;
end;

procedure print_list (l: listptr);
begin
  if l <> nil then begin
    print_list (l^.lleft);
    detail (l^.name);
    print_list (l^.lright);
  end;
end;

procedure print_extensions (x: extptr);
begin
  if x <> nil then begin
    print_extensions (x^.xleft);
    print_list (x^.namelist);
    print_extensions (x^.xright);
  end;
end;

begin
  rewrite (tty); open (tty);
  loop
    write (tty,'PPN: '); break;
    readln (tty);
    if eoln (tty) then extent := '[,]'
    else begin
      extent := '';
      while not eoln (tty) do begin
	read (tty,ch);
	extent := extent || uppercase (ch);
      end;
    end;
    dir_open (error,id,extent);
    if error <> dir_ok then writeln (tty,'Can''t open directory ',extent)
    else begin
      write (tty,'Output to: '); break; readln (tty);
      if eoln (tty) then stop;
      outfile := 'FILES.DIR[,]';
      while not eoln (tty) do begin
	read (tty,ch);
	outfile := outfile || uppercase (ch);
      end;
      rewrite (output,outfile);
      headname := nil;
      headext := nil;
      total_size := 0; total_blocks := 0;
      filecount := 0;
      mark (heapmark);
      loop
	dir_next (error,id,filename);
      exit if error = dir_eof;
	addname;
      end;
      write ('Directory ',extent,' on ');
      write (ns_d2 (extr_date (daytime)),' at ');
      s := ns_t1 (extr_time (daytime));
      if s[1] = '0' then i := 2 else i := 1;
      writeln (substr(s,i,6-i),lowercase(substr(s,9)));
      writeln;
      writeln (header);
      writeln;
      print_alpha_names (headname);
      writeln;
      summary := 'Total of '||tostring(filecount)||' files in '||tostring(total_blocks)||' blocks ('||tostring(total_size)||' words)';
      writeln (summary);
      page (output);
      writeln (header);
      writeln;
      print_extensions (headext);
      writeln;
      writeln (summary);
      writeln (tty,summary);
      dir_close (error,id);
      close (output);
      release (heapmark);
    end;
  end;
end.
