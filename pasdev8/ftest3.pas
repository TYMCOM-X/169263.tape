program ftest3;

label 10, 20, 100, 200, 300;

procedure proc1;
begin goto 100 end;

procedure proc2;
begin goto 200 end;

procedure proc3;
begin goto 300 end;

var p1, p2, p3, p4, p5, p6: boolean;
    a, a1, a2, a3, a4, a5, a6, a7: integer;

begin
  if p1 then
    a := a1
  else begin
    a := a2;
    if p2 then goto 300;
  end;
  if p3 andif p4 then stop;
  a := a3;
  stop;

100:
  if p5 then goto 20;
10:
  a := a4;
20:
  a := a5;
300:
  a := a6;
  if p6 then goto 10;
  stop;

200:
  if p3 then goto 10;

end.
 