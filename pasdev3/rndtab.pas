(* RNDTAB - a quicky to output a table of powers of 10 for the 2
   operand round runtime routine.  *)

program rndtab options nocheck;

type
  double = minimum(real)..maximum(real) prec 16;

var
  bias,dreal: double;
  i: 0..55;

procedure output ( dreal: double; i: integer );

type
  template = record
    case boolean of
      true: (r: double);
      false: (i1: integer; i2: integer)
    end;

var 
  temp: template;

begin
  temp.r := dreal;
  write ( ttyoutput, '	.long	', temp.i1:8:h, ', ', temp.i2:8:h );
  writeln ( ttyoutput, '		; 10 **', i );
end;


begin
  rewrite ( ttyoutput );
  
  bias := (2.00000000 ** -64.00000000) * (10.0000000 ** 38.00000000);
  for i := 0 to 55 do begin
    if i > 38
      then dreal := bias * (10.000000000 ** ( i - 38.000000000 ) )
      else dreal := 10.000000000 ** i;
    output ( dreal, i );
  end;

end.
    