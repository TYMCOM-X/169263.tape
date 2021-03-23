program writer;

var
  file_var : text;
  counter : integer;

begin
  rewrite (file_var, 'WRITER.TX');
  for counter := 1 to 1000000 do
    writeln (file_var, counter);
end.
