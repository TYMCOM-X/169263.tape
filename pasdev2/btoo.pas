program b_to_o;

$SYSTEM getiof

var line: string [256];
    ixout, ix, ix1: integer;

begin
  rewrite (tty);
  open (tty);
  while getiofiles (input, 'PAS', output, 'PAS') do begin
    while not eof do begin
      readln  (line);
      ixout := 1;
      ix := 1;
      while ix < length (line) do begin
	ix := ix + search (substr (line, ix), ['0'..'9'], length (line)-ix+2) - 1;
      exit if ix >= length (line);
	ix1 := verify (substr (line, ix), ['0'..'9']);
      exit if ix1 = 0;
	if uppercase (line [ix+ix1-1]) = 'B' then begin
	  write (substr (line, ixout, ix-ixout), '#O', substr (line, ix, ix1-1));
	  ixout := ix + ix1;
	end;
	ix := ix + ix1;
      end;
      writeln (substr (line, ixout));
    end;
    close (input);
    close (output);
  end;
end.
    