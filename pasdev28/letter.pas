program letter options special, nocheck;
$PAGE includes, declarations
$system dtime.typ[31024,320156]
$system dtime.inc[31024,320156]
$system cmdutl.typ[31024,320156]
$system query.inc[31024,320156]
const
  separation = 3;
  indent = 32;
  plus = '+';
  bar = '|';
  blank = ' ';
  dashes = '-----------------------------------------------------------------';
type
  line_ptr = ^ line_record;
  line_record = record
    next: line_ptr;
    line: string[*]
  end;
public var
  greeting, salutation, body, closing, pt, ptr0, ptr1, ptr2: line_ptr;
  auto_label: boolean;
  str: string;
  i, j, k, first, last: integer;
label 1;

type
  auto_ptr = ^auto_rec;
  auto_rec = packed record
    greet, sal: line_ptr;
    next: auto_ptr
  end;
var
  first_auto, auto1, auto2: auto_ptr;
  file_count, com_count, orig_count, len: integer;
  auto: boolean;

const
  company = 'Stephens'' Nursery, Inc.';
  name = 'Stephen E. Medlin';
  name2 = 'Stephen R. Willyerd';
  street = '13095 Petersburg Road';
  town = 'Milan, Michigan  48160';
  phone = '(313) 439-7295';
$PAGE section
function section (var f: text; multiple: boolean; prompt: string[*]): line_ptr;
var
  l1, l2: line_ptr;
  l3: string [1000];
  ch: char;
  f2: text;
begin
  l1 := nil;
  section := nil;
  if (prompt <> '') andif (f = tty) then begin
    write (tty,prompt,': ');
    break;
  end;
  readln (f);
  while not eoln (f) do begin
    read (f,l3);
    if l3[1] = '@' then begin
      open (f2, substr (l3,2));
      if eof (f2) then begin
	writeln (tty,'?Can''t open file ', substr (l3, 2));
	break (tty);
      end
      else begin
	l2 := section (f2, multiple, prompt);
	close (f2);
      end;
    end
    else begin
      if l3 = '$' then begin
	new (l2, 6);
	l2^.line := '$skip';
	if l1 = nil
	  then section := l2
	  else l1^.next := l2;
	l1 := l2;
	l3 := '$par 5';
      end;
      new (l2, length (l3) + 1);
      l2^.line := l3;
      l2^.next := nil;
    end;
    if l1 = nil
      then section := l2
      else l1^.next := l2;
    l1 := l2;
  exit if not multiple;
    if f = tty then begin
      write (tty,': '); break (tty);
    end;
    readln (f);
  end;
end;

$PAGE get_section

function get_section (nil_ok: boolean; multiple: boolean; prompt: string[*]): line_ptr;
begin
  loop
    get_section := section (input, multiple, prompt);
  exit if nil_ok orif (get_section <> nil);
    writeln (tty,'"',prompt,'" is not optional.');
    break (tty);
  end;
end;
$PAGE error
procedure error (message: string[*]);
begin
  writeln (tty,'?',message,'.');
  goto 1;
end;

$PAGE put_date

procedure put_date;
var s1, s2: string;
begin
  s2 := ns_d2 (extr_date (daytime));
  case lowercase (s2[3]) of
  'n': if lowercase (s2[2]) = 'a'
    then s1 := 'January'
    else s1 := 'June';
  'b': s1 := 'February';
  'y': s1 := 'May';
  'l': s1 := 'July';
  'g': s1 := 'August';
  'p': s1 := 'September';
  't': s1 := 'October';
  'v': s1 := 'November';
  'c': s1 := 'December';
  'r': if lowercase (s2[1]) = 'm'
    then s1 := 'March'
    else s1 := 'April'
  end;
  writeln (s1, substr (s2,4));
end;
$PAGE put_section
procedure put_section (p: line_ptr; delete: boolean);
var l1, l2: line_ptr;
begin
  if p <> nil then begin
    l1 := p;
    repeat
      writeln (l1^.line);
      l2 := l1^.next;
      if delete then
	dispose (l1);
      l1 := l2;
    until l1 = nil;
  end;
end;

$PAGE do_box

procedure do_box (salutation: line_ptr);
var i, j, k: integer;
begin
  if salutation <> nil then begin
    i := 0;
    pt := salutation;
    repeat
      with pt^ do begin
	first := verify (line, ['/',' ']);
	last := length (line);
	while (last > 0) andif (line[last] = blank) do
	  last := last - 1;
	if first = 0
	  then line := ''
	  else line := substr (line, first, last-first+1);
	i := max (i, length (line));
	pt := next;
      end;
    until pt = nil;
    for  j := 1 to 3 do writeln (tty);
    writeln (tty,plus,substr(dashes,1,i+4),plus);
    writeln (tty,bar,blank:i+4,bar);
    pt := salutation;
    repeat
      with pt^ do begin
	k := (i+4-length(line)) div 2;
	writeln (tty,bar,blank:k,line,blank:i+4-k-length(line),bar);
	pt := next;
      end;
    until pt = nil;
    writeln (tty,bar,blank:i+4,bar);
    writeln (tty,plus,substr(dashes,1,i+4),plus);
    for j := 1 to 3 do writeln (tty);
    break (tty);
  end;
end;
$PAGE do_letter

procedure do_letter (salutation, greeting: line_ptr);
begin
  writeln ('$num off');
  writeln ('$ind ',indent:2);
  writeln ('$ver');
  writeln (company);
  writeln (street);
  writeln (town);
  writeln (phone);
  put_date;
  writeln ('$ind');
  if salutation <> nil then begin
    writeln ('$skip ',separation:2);
    put_section (salutation, true);
  end;
  writeln ('$skip ',separation:2);
  if search (greeting^.line, [':']) = 0 then
    greeting^.line := greeting^.line || ':';
  put_section (greeting, true);
  writeln ('$jus');
  writeln ('$skip');
  writeln ('$par 5');
  put_section (body, not auto);
  writeln ('$skip ',separation:2);
  writeln ('$ind ',indent:2);
  put_section (closing, not auto);
  writeln ('$skip ',separation:2);
  writeln ('$ver');
  writeln (name);
  if name2 <> '' then
    writeln (name2);
  writeln ('$jus');
  if output <> ttyoutput then
    close (output);
end;
$PAGE tostr

function tostr(int: integer): string;
var i, j: integer;
begin
  i := int;
  j := 0;
  repeat
    j := j + 1;
    i := i div 10;
  until i = 0;
  putstring (tostr,int:j);
end;
$PAGE mainline
begin
  rewrite (tty); open (tty);
  file_count := 0;
  com_count := 0;
  auto_label := false;
  if query ('Return address labels') then begin
    new (ptr0, length (name));
    new (ptr1, length (street));
    new (ptr2, length (town));
    ptr0^.line := name;
    ptr0^.next := ptr1;
    ptr1^.line := street;
    ptr1^.next := ptr2;
    ptr2^.line := town;
    ptr2^.next := nil;
    auto_label := query ('Automatic');
    if not auto_label then begin
      write (tty,'Number labels required: ');
      break;
      readln (tty);
      read (tty,j);
      for i := 1 to j do
	do_box (ptr0);
      dispose (ptr0);
      dispose (ptr1);
      dispose (ptr2);
    end;
  end;
  loop
    1:
    auto := false;
    write (tty,'Source: '); break;
    readln (tty);
    if eoln (tty) then
      input := tty
    else begin
      read (tty,str);
      if uppercase (str) = 'TTY:' then
	input := tty
      else if uppercase (str) = 'AUTO' then begin
	auto := true;
	orig_count := file_count;
	loop
	  write (tty,'Auto file: '); break; readln (tty);
	  read (tty,str);
	  open (input,'.cmd '||str);
	exit if not eof (input);
	  writeln (tty,'Can''t open file ',str);
	end;
      end
      else begin
	open (input,'.let '||str);
	if eof (input) then
	  error ('Can''t open source file');
      end;
    end;
    if not auto then begin
      write (tty,'List on: '); break;
      readln (tty);
      if eoln (tty) then
	output := ttyoutput
      else begin
	read (tty,str);
	if uppercase (str) = 'TTY:' then
	  output := ttyoutput
	else begin
	  rewrite (output,'.tmp '||str);
	  if not eof (output) then begin
	    if input <> tty then
	      close (input);
	    error ('Can''t open output file');
	  end;
	end;
      end;
    end;
    new (first_auto);
    with first_auto^ do begin
      sal := get_section (true, true, 'To');
      greet := get_section (false, false, 'Salutation');
      next := nil;
    end;
    if auto then begin
      auto2 := first_auto;
      loop
	salutation := get_section (true, true, 'To');
      exit if salutation = nil;
	new (auto1);
	auto1^.sal := salutation;
	auto1^.greet := get_section (false, false, 'Salutation');
	auto1^.next := nil;
	auto2^.next := auto1;
	auto2 := auto1;
      end;
    end;
    if auto then begin
      close (input);
      input := tty;
    end;
    body := get_section (false, true, 'Body');
    closing := get_section (true, false, 'Closing');
    if closing = nil then begin
      new (closing, 16);
      closing^.next := nil;
      closing^.line := 'Sincerely yours,';
    end;
    auto1 := first_auto;
    while auto1 <> nil do with auto1^ do begin
      if sal <> nil then begin
	if auto_label then
	  do_box (ptr0);
	do_box (sal);
      end;
      if (sal = nil) orif (sal^.line[1] <> '/') then begin
	if auto then begin
	  file_count := file_count + 1;
	  putstring (str,file_count,'.tmp');
	  rewrite (output,str);
	end;
	do_letter (sal, greet);
      end
      else begin
	if sal <> nil then
	  dispose (sal);
	dispose (greet);
      end;
      auto2 := next;
      dispose (auto1);
      auto1 := auto2;
    end;
    if auto then begin
      dispose (body);
      dispose (closing);
      com_count := com_count + 1;
      putstring (str,'com',tostr(com_count),'.tmp');
      rewrite (output,str);
      writeln ('do scribe');
      len := 0;
      for i := orig_count + 1 to file_count do begin
	if len >= 450 then begin
	  writeln;
	  writeln ('do com');
	  com_count := com_count + 1;
	  putstring (str,'com',tostr(com_count),'.tmp');
	  writeln (str); writeln ;
	  close (output);
	  rewrite (output,str);
	  writeln ('do scribe');
	  len := 0;
	end;
	putstring (str,tostr(i),'.lst=',tostr(i),'.tmp');
	writeln (str);
	len := len + length (str) + 2;
      end;
      writeln;
      close (output);
    end;

    if input <> tty then
      close (input);
  end;
end.
    