program retab;
$include (pasdev27)getiof.inc
const tab := chr(9);
var inf,outf: text;
    line: string[132];
    i,j: integer;
begin
open(tty); rewrite(tty);
loop
    getiofiles(inf,outf,'TXT','TXT');
    while not eof(inf) do begin
	readln(inf,line);
	i := length(line);
	while (i > 0) andif (line[i] = ' ') do i := i - 1;
	line := substr(line,1,i);
	for i := (length(line) - 1) div 8 downto 1 do begin
	    j := i*8 + 1;
	    while (j > (i*8 - 7)) andif (line[j-1] = ' ') do j := j - 1;
	    if j < i*8 then begin
		line[j] := tab;
		line := substr(line,1,j) || substr(line,i*8 + 1)
		end
	    end;
	writeln(outf,line)
	end;
    close(inf); close(outf)
    end
end.
    