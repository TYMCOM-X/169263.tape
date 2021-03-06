program int;
  
const
  payment = 142.53;
  principle = 4000.00;
  interest_rate = 0.1700 / 12.0;
  
var
  payment_interest, payment_principle,
  total_payed_interest, total_payed_principle: real;
  i: integer;
  
begin
  total_payed_interest := 0.0;
  total_payed_principle := 0.0;
  rewrite (output, 'int.lpt');
  page (output);
  writeln (output, '                  amount applied to       total payed on    remaining');
  writeln (output, '  #     amount   interest  principle   interest  principle  principle');
  writeln (output, ' ---    ------   --------  ---------   --------  ---------  ---------');
  writeln (output, ' ':58, principle:11:2);
  
  for i := 1 to 150 do begin
    payment_interest := round (interest_rate * (principle - total_payed_principle), -2);
    payment_principle := payment - payment_interest;
    total_payed_interest := total_payed_interest + payment_interest;
    total_payed_principle := total_payed_principle + payment_principle;
    writeln (output, i:3, payment:11:2, payment_interest:11:2, payment_principle:11:2,
		  total_payed_interest:11:2, total_payed_principle:11:2,
		  principle - total_payed_principle:11:2);
   exit if total_payed_principle >= principle
  end;
  
  close (output)
end.
  