procedure parse_error;
begin
  writeln (tty, '** ERROR IN STATE ', state[stk_ind]:3, ' ON INPUT ', insymbol:3);
end;
    