program eopage_stuff;

var
   text_file : text;
   text_file_name : file_name;

procedure write_info;
begin

writeln (ttyoutput,
   'Status = ',
   ord (iostatus (text_file)),
   ', buffer = ',
   ord (text_file^),
   ', eof = ',
   eof (text_file),
   ', eopage = ',
   eopage (text_file),
   ', eoln = ',
   eoln (text_file));
break (ttyoutput);
end;

begin
open (tty); rewrite (ttyoutput);

rewrite (text_file, 'test.tmp');
text_file_name := filename (text_file);
writeln (text_file);
page (text_file);
close (text_file);

reset (text_file, text_file_name);
write_info;
while not eof (text_file) do
   begin
   get (text_file);
   write_info;
   end;
writeln (ttyoutput, 'End of file.');

reset (text_file, text_file_name, [ascii]);
write_info;
while not eof (text_file) do
   begin
   get (text_file);
   write_info;
   end;
end.
  