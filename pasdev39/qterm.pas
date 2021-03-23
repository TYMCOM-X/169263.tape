(* QTERM consolidates many tty I/O functions, so that other users of
   the QEDLIB subroutines may provide their own controllers for whatever
   hardware they want to use for the tty.

   Module begun 29 July 1982 by WNH.  *)

module qterm;

public procedure ttput;
begin
put (ttyoutput);
end;

public procedure ttwrite (line : string [*]);
begin
write (ttyoutput, line);
end;

public procedure ttwtln;
begin
writeln (ttyoutput);
end;

public procedure tticlr;
begin
clear (tty);
end;

public procedure ttoclr;
begin
clear (ttyoutput);
end;

public procedure ttbrk;
begin
break (ttyoutput);
end;

public procedure ttget;
begin
get (tty);
end;

public procedure ttread (var line: string [*]);
begin
read (tty, line);
end;

public procedure ttrdln;
begin
readln (tty);
end;

public procedure ttpage;
begin
page (ttyoutput);
end;

public procedure ttisetbuf (letter : char);
begin
tty^ := letter;
end;

public procedure ttosetbuf (letter : char);
begin
ttyoutput^ := letter;
end;


public function ttieof : boolean;
begin
ttieof := eof (tty);
end;

public function ttoeof : boolean;
begin
ttoeof := eof (ttyoutput);
end;

public function ttieol : boolean;
begin
ttieol := eoln (tty);
end;

public function ttirefbuf : char;
begin
ttirefbuf := tty^;
end;

public function ttorefbuf : char;
begin
ttorefbuf := ttyoutput^;
end;

public function ttist : io_status;
begin
ttist := iostatus (tty);
end;

public function ttost : io_status;
begin
ttost := iostatus (ttyoutput);
end;

end. (* qterm *)
