program test;

external procedure abc (var integer; array[1..*] of boolean; integer);

external procedure def (-1.0..2.0 prec 14; real; integer; char; boolean; procedure);

external procedure ghi;

external procedure xyz (var integer; integer) options fortran;

var
  i, j: integer;
  b: array[1..4] of boolean;

begin
 abc (i, b, j+j);
 def (3.4, 2.3, j, 'A', false, ghi);
 xyz (i, j+j);
end.
  