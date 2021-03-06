program dumtyp;
(* file typer for ADM-3A *)

type
  threechars = packed array[1..3] of char;

const
  escseq: threechars := chr(27)||'=7';
  cr: char := chr(13);
  lf:char := chr(10);
  bs: char := chr(8);
  tab: char := chr(9);

  maxlinelen := 132;

var
  f: text;	(* the file to be typed *)
  fname: string;	(* its name *)
  line: string[maxlinelen];	(* a line of file f *)
  tline: string[maxlinelen];	(* a temporary string *)
  idx, tidx: 1..maxlinelen+1;	(* indices in to the above lines *)
  startstring: string[maxlinelen];	(* string appearing in first line typed *)
  started: boolean;	(* whether we've seen startstring yet *)
  linepart: string[80];	(* a chunk of a file line that fits *)
  firstcol,lastcol: 1..maxlinelen;	(* user-specified range of file columns *)
  startcol,endcol,curcol: 0..81;	(* terminal columns *)
  skippoint: 1..maxlinelen;	(* index of next piece of linepart to do *)


function col(x: 1..80): char;
begin col := chr(x+31) end;

begin
  open(tty); rewrite(tty);
  loop	(* command loop *)
    write(tty,'File: '); break; readln(tty);
  exit if eoln(tty);
    read(tty,fname);
    repeat	(* get good column values *)
      firstcol := 1; lastcol := maxlinelen;
      write(tty,'Columns: '); break; readln(tty);
      while (not eoln(tty)) andif (tty^ = ' ') do get(tty);
      if not eoln(tty) then begin
	read(tty,firstcol);
	while (not eoln(tty)) andif (tty^ = ' ') do get(tty);
	if not eoln(tty) then read(tty,lastcol)
	end
      until firstcol <= lastcol;

    write(tty,'Start string: '); break; readln(tty); read(tty,startstring);
    started := false;
    open(f,'.TXT '||fname, [ASCII]);

    curcol := 1;
    write(tty,escseq,col(1)); break;
    while not eof(f) do begin	(* loop once per line *)
      readln(f);
    exit if eof(f);
      read(f,tline);
      started := started or (index(line,startstring) <> 0);
      if started then begin
	tidx := 1;
	idx := 1;
	(* first, we expand all tabs into spaces *)
	while tidx <= length(tline) do begin
	  if tline[tidx] = tab then
	    repeat
	      line := line || ' ';
	      idx := idx + 1
	    until (idx mod 8) = 1
	  else begin
	    line := line || tline[tidx];
	    idx := idx + 1
	  end;
	  tidx := tidx + 1
	end;

	line := substr(line,min(firstcol,length(line)+1),	(* get desired slice of line *)
			min(length(line),lastcol) -
			min(firstcol,length(line)+1) + 1);
	repeat	(* loop once per 80-char chunk of line *)
	  endcol := min(length(line),80);
	  linepart := substr(line,1,endcol);
	  line := substr(line,endcol+1);

	  startcol := verify(linepart,[' ']);
	  if startcol > 0 then begin	(* there's a nonblank char in this chunk *)
	    while ord(linepart[endcol]) <= ord(' ') do endcol := endcol-1;
	    linepart := substr(linepart,startcol,endcol-startcol+1);
	    if startcol < curcol then begin	(* move cursor back fastest way *)
	      if (startcol <= 4) and (startcol <= (curcol div 2))
	      then write(tty,cr,'   ':startcol-1)
	      else begin
		if (curcol-startcol) <= 4
		then write(tty,(bs||bs||bs||bs):curcol-startcol)
		else write(tty,escseq,col(startcol))
		end
	      end
	    else begin	(* move cursor forward fastest way *)
	      if (startcol-curcol) <= 4
	      then write(tty,'    ':startcol-curcol)
	      else write(tty,escseq,col(startcol))
	      end;

	    curcol := startcol;
	    loop	(* output text,blanks,text,...,text *)
	      skippoint := index(linepart,'     ',length(linepart)+1);
	      curcol := curcol + skippoint - 1;
	      write(tty,substr(linepart,1,skippoint-1));
	      linepart := substr(linepart,skippoint);
	    exit if length(linepart) = 0;
	      skippoint := verify(linepart,[' ']);
	      curcol := curcol + skippoint - 1;
	      write(tty,escseq,col(curcol));
	      linepart := substr(linepart,skippoint)
	      end;

	    assert(curcol=endcol+1)

	    end;	(* chunk is done *)
	  write(tty,lf); break	(* kick line and break *)
	  until length(line) = 0	(* iterate until line is done *)
	  end	(* line is done *)
      end;	(* iterate until file is done *)

    write(tty,cr)

    end	(* command loop *)
  end.
 