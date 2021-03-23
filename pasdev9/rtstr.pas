program prog;
const tab = chr (11b);
      comma = ''',';
var s: string;
    i: integer;
begin
  reset (input,'rtstr.typ');
  rewrite (output,'rtstr.nam');
  repeat
    read (s);
    i := index (s, 'rt_');
    s := uppercase (substr (s, i) );
    write (tab,'''C.');
    writeln (substr (s, 4, 2), substr (s, 7, length (s) - 7 ), comma);
    readln;
  until eof;
end.
