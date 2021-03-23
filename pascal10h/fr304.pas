program fr304;
  
var
  c: array [1..10] of boolean := (true, false, true, false, true, false, true, false, true, false);
  i: integer;
  
begin
  rewrite (ttyoutput);
  i := 1;
  c[i] := not c[i];
  if c[1] then
    writeln (ttyoutput, 'fr304 fails')
  else
    writeln (ttyoutput, 'ok');
end.
