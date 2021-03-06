program procedure_var_test;
 
type
     proc_var_1 = procedure;
     proc_var_2 = procedure ( var procedure_var : proc_var_1 );

var  A : proc_var_1;
     B : proc_var_1;
     C : proc_var_2;

procedure write_stars;
  begin
    writeln (tty, '*****');
    break (tty);
  end;

procedure write_plus;
  begin
    writeln (tty, '+++++');
    break (tty);
  end;

procedure reassign_procedure_var ( var procedure_var : proc_var_1 );
  begin
    procedure_var := write_plus;
  end;

begin
  A := write_stars;
  B := write_plus;
  C := reassign_procedure_var;

  A;                (* Should write '*****' *)
  B;                (* Should write '+++++' *)
  C ( A );          (* Change A from write_stars to write_plus *)
                    
  A;                (* Now, A should write '+++++' *)
end.
  