(* DMTEST - test driver for DIR_MATCH.  *)

program dmtest;

$SYSTEM dtime.typ
$SYSTEM pasdir.typ
$SYSTEM pasdir.inc

var
  pattern: string;
  target: string;

begin
  open ( tty );
  rewrite ( ttyoutput );

  loop
    write ( ttyoutput, 'Enter pattern: ' );
    break ( ttyoutput );
    if eoln ( tty ) then readln ( tty );
    read ( tty, pattern );
  exit if pattern = '';
    loop
      write ( ttyoutput, 'Enter target: ' );
      break ( ttyoutput );
      if eoln ( tty ) then readln ( tty );
      read ( tty, target );
    exit if target = '';
      writeln ( ttyoutput, pattern, '  ', target, '  ', 
	dir_match ( substr(target,2,length(target)-2),
		    substr(pattern,2,length(pattern)-2) ) );
    end;
  end;
end.
