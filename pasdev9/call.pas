module test;
external procedure p;
public procedure p1 (p2: procedure; var p3: procedure);
var p4: procedure;
begin
  p;
  p2;
  p3;
  p4;
  p1 (p2, p3);
  p1 (p3, p4);
  p1 (p, p3);
end.
    