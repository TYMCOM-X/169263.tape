program whatsup;

(* This program is intended to find any differences between the function
   of QREAD as it was when I got it and the function of QREAD as I have
   modified it to used the entry points in QTERM. *)

external function qread : qstring;

var
  line : qstring;
  index :  1..256;

begin
repeat
  begin
  line := qread;
  for index := 1 to length (line) do
    begin
    if line [index] < ' ' then
      writeln (ttyoutput, '&', ord (line [index]):1:h)
    else
      writeln (ttyoutput, line [index]);
    end;
  break (ttyoutput);
  end
until line = 'quit'
end.
 