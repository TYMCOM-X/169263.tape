program libsize;
var i, j, k: integer; status: io_status;
    highsize, lowsize, hsize, lsize, modules: integer;
    str: string;
begin
  rewrite (tty);
  open (tty);
  loop
    write (tty,'File: '); break; readln (tty);
  exit if eoln (tty);
    read (tty,str);
    reset (input,'paslib .lst '||str);
    if eof (input) then begin
      writeln (tty,'Can''t open file.');
    end
    else begin
      highsize := 0; modules := 0;
      lowsize := 0;
      repeat
	modules := modules + 1;
	read (str);
	i := index (str,' ');
	j := verify (substr (str,i),[' '])+i-1;
	getstring (substr (str,j),hsize:6:o,lsize:8:o); status := iostatus;
	if status = io_ok then begin
	  highsize := highsize + hsize - 400000;
	  lowsize := lowsize + lsize;
	end
	else begin
	  getstring (substr (str,j), hsize:6:o);
	  if hsize >= 400000b
	    then highsize := highsize + hsize - 400000b
	    else lowsize := lowsize + hsize;
	end;
	readln;
      until eof (input);
      close (input);
      writeln (tty,modules,' modules');
      writeln (tty,'Highseg: ',highsize:6:o,' (',(highsize+511)div 512,'P)');
      writeln (tty,'Lowseg:  ',lowsize:6:o,' (',(lowsize+511)div 512,'P)');
    end;
  end;
end.
    