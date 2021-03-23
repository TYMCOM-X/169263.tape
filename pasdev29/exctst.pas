Program except_test;


var int: integer;
    recurse_level : integer;
    nest_level : integer;
$PAGE recurse

procedure recurse;

var int : integer;

begin

  recurse_level := recurse_level + 1;

  writeln ( tty , 'RECURSE at level: ' , recurse_level , '.' );

  break ( tty );
  readln ( tty );
  read ( tty , int );
  if int = 1 then recurse;

  recurse_level := recurse_level - 1

end;
$PAGE next_cond
procedure nest_cond;


var int : integer;

begin
  nest_level := nest_level + 1;
  writeln ( tty , 'NESTED interrupt handler, 0 returns to previous level.');

  loop
    begin
    write ( tty , 'Enter test number: ' );
    break ( tty );
    readln ( tty );
    read ( tty , int );

  exit if int = 0;

    case int of
      1: mask ( attention );
      2: unmask ( attention );
      3: recurse;
      4: begin
	   if masked ( attention )
	     then writeln ( tty , 'Attentions are masked.' )
	   else writeln ( tty , 'Attentions are not masked.' )
	 end;
      5: if pending ( attention )
	   then writeln ( tty , 'Attentions are pending.' )
	 else writeln ( tty , 'Attentions are not pending.' );
      6: nest_cond;
      others: writeln ( tty ,'0..6 only!' )
    end;

    nest_level := nest_level - 1;

    exception
      attention : writeln ( tty , 'Attention at level: ', nest_level );
      allconditions : begin
			writeln ( tty , 'Allcondition triggered in nested routine.' );
			EXCEPTION_MESSAGE ();
		    end;

    end;		(* Begin *)
  end;		(* loop *)
end;
$PAGE main
begin
  open ( tty );
  rewrite ( tty );
  writeln ( tty , 'Attention test program.' );

  loop
    begin
    write ( tty , 'Enter test routine number: ' );
    break ( tty );
    readln ( tty );
    read ( tty , int );

    recurse_level := 0;
    nest_level := 0;
    case int of
      0: stop;
      1: mask ( attention );
      2: unmask ( attention );
      3: recurse;
      4: if masked ( attention )
	   then writeln ( tty , 'Attentions are masked.' )
	 else writeln ( tty , 'Attentions are not masked.' );
      5: if pending ( attention )
	   then writeln ( tty , 'Attentions are pending.' )
	 else writeln ( tty , 'Attentions are not pending.' );
      6: nest_cond;
      others : writeln ( tty , '0..6 only!' )
    end;

    exception
      attention : begin
	  writeln ( tty , 'Attention trapped at level 0.' );
	  recurse_level := 0;
	end;
      allconditions: begin
	  writeln ( tty , 'Allconditions trapped at level 0.' );
	  exception_message ();
	end;
    end;
  end;		(* loop *)

end.
