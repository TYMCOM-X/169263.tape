$TITLE DUMTYP -- File Typing Program for ADM-3A
program dumtyp
  options special;

$SYSTEM uuocal
$PAGE auto_cr_lf
(*  AUTO CR LF will turn on or off the operating system switch which
    controls the automatic generation of a cr-lf following the 80-th
    character of each line.  It returns the old value of the switch.  *)

function auto_cr_lf ( on: boolean ): boolean;

type
    characteristics =
      ( lc_dm1, lc_dm2, lc_ecs, lc_pss, lc_obs, lc_hdx, lc_esc, lc_crd, lc_dfr,
	lc_nop, lc_nfc, lc_brk, lc_axc, lc_ncm, lc_hht, lc_lcp, lc_ptm, lc_hff );
    char_set = set of characteristics;

    line_char_record = packed record
      case boolean of
	false: ( bits: char_set;
		 port: 0 .. #o777777 );
	true:  ( value: machine_word );
    end;

var chars: line_char_record;
    x: machine_word;

begin
  chars.value := -1;
  if uuo_call (#o51, 6, 0, address (chars), x) then ;
  auto_cr_lf := not (lc_nfc in chars.bits);
  if auto_cr_lf <> on then begin
    if on
      then chars.bits := chars.bits - [lc_nfc]
      else chars.bits := chars.bits + [lc_nfc];
    if uuo_call (#o51, 7, 0, address (chars), x) then ;
  end;
end;
$PAGE dumtyp - main program
const
  cr = chr(13);
  lf = chr(10);
  bs = chr(8);
  esc = chr(27);
  maxlinelen = 256;

var
  old_auto_cr_lf: boolean;
  f: text;	(* the file to be typed *)
  fname: file_name;	(* its name *)
  line: string[maxlinelen];	(* a line of file f *)
  startstring: string[maxlinelen];	(* string appearing in first line typed *)
  started: boolean;	(* whether we've seen startstring yet *)
  linepart: string[80];	(* a chunk of a file line that fits *)
  firstcol,lastcol: 1..maxlinelen;	(* user-specified range of file columns *)
  startcol,endcol,curcol: 0..81;	(* terminal columns *)
  skippoint: 1..maxlinelen;	(* index of next piece of linepart to do *)

exception
  finished;


procedure set_column (col: 1 .. 80);
const
  escseq = esc||'=7';
begin
  write (tty, escseq, chr (col + 31));
end;


begin
  old_auto_cr_lf := auto_cr_lf (false);
  open(tty); rewrite(tty);
  loop	(* command loop *)
    write(tty,'File: '); break; readln(tty);
    IF EOLN (TTY) THEN
      SIGNAL (FINISHED);
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
    reset(f,'.TXT '||fname);

    curcol := 1;
    set_column (1); break;
    while not eof(f) do begin	(* loop once per line *)
      readln(f,line);
      started := started or (index(line,startstring) <> 0);
      if started then begin
	line := substr(line,min(firstcol,length(line)+1),	(* get desired slice of line *)
			min(length(line),lastcol) -
			min(firstcol,length(line)+1) + 1);
	repeat	(* loop once per 80-char chunk of line *)
	  endcol := min(length(line),80);
	  linepart := substr(line,1,endcol);
	  line := substr(line,endcol+1);

	  startcol := verify(linepart,[' ']);
	  if startcol > 0 then begin	(* there's a nonblank char in this chunk *)
	    while linepart[endcol] = ' ' do endcol := endcol-1;	(* kill trailers *)
	    linepart := substr(linepart,startcol,endcol-startcol+1);
	    if startcol < curcol then begin	(* move cursor back fastest way *)
	      if (startcol <= 4) and (startcol <= (curcol div 2))
	      then write(tty,cr,'   ':startcol-1)
	      else begin
		if (curcol-startcol) <= 4
		then write(tty,(bs||bs||bs||bs):curcol-startcol)
		else set_column (startcol)
		end
	      end
	    else begin	(* move cursor forward fastest way *)
	      if (startcol-curcol) <= 4
	      then write(tty,'    ':startcol-curcol)
	      else set_column (startcol)
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
	      set_column (curcol);
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

  exception
    attention, finished:
      BEGIN
	CLEAR (TTYOUTPUT);
	if auto_cr_lf (old_auto_cr_lf) then ;
      END

  end.
