program q;
const
  plus := '+';
  minus := '-';
  lparen := '(';
  rparen := ')';
  times := '';
  slash := '/';
  aa := '(3(ac-4d)+b**2)/3';
  bb := '(-2b**3+9b(ac-4d)-27(a**2d-4bd+c**2))/27';

public var
  str: string;
  linelen, maxlen: integer;
  ch: char; chars_out: integer;
  lastch: string[1];

procedure out (str: string);
var s: string;
begin
  if length (str) = 0 then begin
    s := lastch;
    lastch := '';
  end
  else begin
    if (lastch = '+') andif (str[1] = '-') then
      s := substr (str,1,length(str)-1)
    else s := lastch || substr (str,1,length(str)-1);
    lastch := str[length(str)];
  end;
  chars_out := chars_out + length (s);
  if linelen + length (s) > maxlen then begin
    writeln (substr (s,1,maxlen-linelen));
    write (substr (s,maxlen-linelen+1));
    linelen := linelen + length (s) - maxlen;
  end
  else begin
    write (s);
    linelen := linelen + length (s);
  end;
end;
procedure write_ab (sign: char);
begin
  out ('(');
  out (bb);
  out ('/2');
  out (sign);
  out ('((');
  out (bb);
  out (')**2/4+(');
  out (aa);
  out (')**3/27)**(1/2))**(1/3)');
end;
procedure write_y;
begin
  out ('-(');
  write_ab (plus);
  out (plus);
  write_ab (minus);
  out (')/2+(');
  write_ab (plus);
  out (minus);
  write_ab (minus);
  out (')/2(-3)**(1/2)');
end;
procedure write_r;
begin
  out ('(a**2/4-b+');
  write_y;
  out (')**(1/2)');
end;
procedure write_de (nonzero: boolean; sign: char);
begin
  out ('(3a**2-2b');
  out (sign);
  if nonzero then begin
    out ('(4ab-8c-a**3)/4');
    write_r;
  end
  else begin
    out ('2((');
    write_y;
    out (')**2-4d)**(1/2)');
  end;
  out (')**(1/2)');
end;
procedure write_roots (nonzero: boolean);
var i: integer;
begin
  for i := 1 to 4 do begin
    writeln;
    linelen := 0;
    out ('x ('|| chr(ord(i)+ord('0')) ||') = -a/4');
    if i <= 2 then out(plus) else out(minus);
    write_r;
    out ('/2');
    if odd (i) then out (plus) else out (minus);
    if i <= 2 then write_de (nonzero,plus) else write_de (nonzero,minus);
    out ('/2');
    writeln;
  end;
end;

begin
  rewrite (tty); open (tty);
  write (tty,'Output to: ');
  break;
  readln (tty);
  if eoln (tty) then stop; str := '';
  while not eoln (tty) do begin
    read (tty,ch);
    str := str || ch;
  end;
  rewrite (output,'.lst '||str);
  if not eof (output) then stop;
  write (tty,'Max line length: ');
  break;
  readln (tty);
  if eoln (tty) then maxlen := 80
  else read (tty,maxlen);
  writeln ('There are four roots to the quadratic equation');
  writeln;
  writeln ('    x**4+a*x**3+b*x**2*c*x*d = 0');
  writeln;
  linelen := 0; lastch := '';
  out ('If ');
  write_r;
  out ('');
  writeln;
writeln;
writeln ('is non-zero, then the roots are ');
writeln;
linelen := 0; lastch := '';
  write_roots (true);
  out ('');
  writeln;
  linelen := 0; lastch := '';
  out ('If ');
  write_r;
  out ('');
  writeln;
  writeln;
  writeln ('is zero, then the roots are ');
  writeln;
  linelen := 0; lastch := '';
  write_roots (false);
  out ('');
  writeln;
  close (output);
end.
 