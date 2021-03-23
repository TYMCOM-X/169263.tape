program ontest;

type bigrec = record a : array [ 1..300 ] of integer end;
static var push_count : integer := 0;
static var recurse_level : integer := 0;
var int : integer;
var recptr : ^bigrec;

external procedure mask;
external procedure unmask;
external procedure escoff;
external procedure escpop;
external procedure firescape;
external function onescape : boolean;
external function pushescape : boolean;
external function on_heap_overflow : boolean;
$PAGE recurse
procedure recurse;

var int : integer;

begin
  recurse_level := recurse_level + 1;
  writeln ( tty , 'RECURSE at level: ' , recurse_level , '.' );
  write ( tty , 'Enter "1" to recurse some more; all else returns: ' );

  break ( tty );
  readln ( tty );
  read ( tty , int );
  if int = 1 then recurse;

  recurse_level := recurse_level - 1
end;
$PAGE nest_int
procedure nest_int;

var int : integer;
  push_count : integer;
  recurse_level : integer;

begin
  writeln ( tty , 'NESTED interrupt handler, 0 returns to previous level.' );
  push_count := 0;
  loop
    write ( tty , 'Enter test number: ' );
    break ( tty );
    readln ( tty );
    read ( tty , int );

    recurse_level := 0;
  exit if int = 0;
    case int of
	1: begin
		push_count := 0;
		if onescape
		  then begin
		    writeln ( tty );
		    writeln ( tty , 'Nested ONESCAPE has been called.' )
		  end
	   end;
	2: begin
		push_count := push_count + 1;
		if pushescape
		  then begin
		    writeln ( tty );
		    writeln ( tty , 'Nested PUSHESCAPE has been called at level: ',push_count)
		  end
	   end;
	3: begin
		push_count := 0;
		escoff;
	   end;
	4: begin
		push_count := push_count -1;
		escpop
	   end;
	5: mask;
	6: unmask;
	7: firescape;
	8 : recurse;
	9: begin
		nest_int;
		writeln ( tty , 'POPPING "1" leel of nested routines.' )
	   end;
	10: begin
		writeln ( tty , 'ON_HEAP_OFERFLOW Setup.' );
		if on_heap_overflow
		  then writeln ( tty , 'The heap has overflowed.' )
	    end;
	11: begin
		loop
		  new ( recptr )
		end
	    end;
	12: begin loop end; end;
	others: Writeln ( tty , 'try again. 0-12 only.' )
    end
  end
end;
$PAGE MAIN
begin
  reset ( tty );
  rewrite ( tty );

  loop
    writeln ( tty , 'Enter test routine number: ' );
    break ( tty );
    readln ( tty );
    read ( tty , int );

    recurse_level := 0;
    case int of
	0: stop;
	1: begin
		push_count := 0;
		if onescape
		  then begin
		    writeln ( tty);
		    writeln ( tty , 'ONESCAPE has been called' );
		  end
	   end;
	2: begin
		push_count := push_count + 1;
		if pushescape
		  then begin
		    writeln ( tty );
		    writeln ( tty , 'PUSHESCAPE has been called at level: ',push_count)
		  end
	   end;
	3: begin
		push_count := 0;
		escoff;
	   end;
	4: begin
		push_count := push_count - 1;
		escpop
	   end;
	5: mask;
	6: unmask;
	7: firescape;
	8: recurse;
	9: begin
		nest_int;
		writeln ( tty, 'Level MAIN, after popping nested routines.' )
	   end;
	10: begin
		writeln ( tty , 'ON_HEAP_OVERFLOE setup.' );
		if on_heap_overflow
		  then writeln ( tty , 'The heap has overflowed at level MAIN' )
	    end;
	11: begin
			loop
		  new ( recptr )
		end
	    end;
	12: begin loop end end;
	others: writeln ( tty , 'try again. 0-12 only' )
    end
  end
end.
