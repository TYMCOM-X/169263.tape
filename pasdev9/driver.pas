program test options special (ptr, coer);

$include hexrea.inc

var
  str1, str2: string;
  r: real;
  d: d_real;

begin
  rewrite (tty); open (tty);
  input := tty;
  output := ttyoutput;
  loop
    write ('Test value: '); break; readln; read (str1);
  exit if str1 = '';
    getstring (str1,r);
    getstring (str1,d);
    writeln;
    writeln ('  Single precision:');
    writeln;
    writeln (hexreal(address(r),s_format));
    writeln;
    writeln ('  Double precision:');
    writeln;
    writeln (hexreal(address(d),d_format));
  end;
end.
