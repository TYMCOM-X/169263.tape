program split;

var
   i:integer;
   str:string;
   exe017:text;
   exe1:text;
   exe2:text;

begin
   reset(exe017,'exe017.lst');
   rewrite(exe1);
   rewrite(exe2);
   for i:=1 to 6000 do
   begin
      readln(exe017,str);
      writeln(exe1,str);
   end;
   readln(exe017,str);
   while not(eof(exe017)) do
   begin
      writeln(exe2,str);
      readln(exe017,str);
   end;
end.
  