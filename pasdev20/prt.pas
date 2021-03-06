$TITLE pretty -- pascal pretty printing main program
$LENGTH 43

program pretty;

external procedure getinit;
external procedure stkinit;
external procedure putinit;
external procedure format;

label (* restart *) 100;

public procedure error ( msg: string );
begin
  writeln (tty,msg);
  goto (* restart *) 100;
end;
$PAGE getiofiles

(*  GETIOFILES is a routine for reading command lines.  It assumes
    that the calling program does input and output on the standard
    files INPUT and OUTPUT, that the command lines have the format:
	<output file>=<input file>
    and that an empty command line indicates a request to terminate
    the program.  *)

public procedure getiofiles;

var
    line: string [80];
    ind: 0 .. 80;

label (* try_again *) 100;

begin

(* try_again *) 100:

  (*  Read a command line from the tty.  *)

  write (tty,'*');
  break;
  readln (tty);
  line := '';
  while not eoln(tty) do begin
    line := line || tty^;
    get (tty);
  end;

  (*  An empty command line means stop.  *)

  if line = '' then stop;

  (*  Find the input and output file names.  *)

  ind := search(line,['=']);
  if ind = 0 then begin
    writeln (tty,'USAGE: <OUTPUT FILE>=<INPUT FILE>');
    goto (* try_again *) 100;
  end;

  (*  Open and check the input file.  *)

  reset (input,'.PAS '||substr(line,ind+1));
  if eof(input) then begin
    writeln (tty,'FILE ',substr(line,ind+1),' EMPTY OR MISSING');
    goto (* try_again *) 100;
  end;

  (*  Open and check the output file.  *)

  rewrite (output,'.PAS '||substr(line,1,ind-1));
  if not eof(output) then begin
    writeln (tty,'BAD OUTPUT FILE ',substr(line,1,ind-1));
    goto (* try_again *) 100;
  end;
end (* getiofiles *);
$PAGE pretty:  main program

begin
  rewrite (tty);
  writeln (tty,'PRETTY, Version 1.0');
  writeln (tty);
  open (tty);
(* restart *) 100:
  loop
    getiofiles;
    getinit;					(* ^the order of these calls must not be changed. *)
    stkinit;
    putinit;
    format;
    close (output);
  end;
end.
  