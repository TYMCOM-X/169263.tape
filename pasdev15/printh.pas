module prheap options special, nocheck, noopt;
$LENGTH 44

type
  HW = 0..777777B;
  FW = packed record
    HWL: HW; 
    HWR: HW 
  end;

  ADR = ^FW;

var
  WORD: FW;
  HADR, HSTART, HEND, HCUR: ADR;
  HFSTART, HFEND, HROVER: ADR;
  HCOUNT: HW;
  FREE: BOOLEAN;

(*
external var CNNEW,
CNSPLI,
CNDISP,
CNCOAL,
CNEXT,
CNEXAM: FW;
*)

public procedure PRINT_HEAP;

  procedure W_TICK(CH:CHAR);

  begin
    WRITE(TTY,CH);
    HCUR := PTR(ORD(HCUR)+1);
    HCOUNT := HCOUNT -1
  end;


begin
  HSTART := ADR ( ADR ( ord ( ADR (15b)^.HWR ) + 17b )^.HWR );
  WRITELN(TTY);
  if ORD(HSTART) = 0 then
  begin
    WRITELN(TTY,' No heap currently active.');
    return
  end;
  WRITELN(TTY,'HEAP at ',ORD(HSTART):6:O,'  previous at ',
    HSTART^.HWL:6:O,'   tail at ',HSTART^.HWR:6:O);
  HEND := PTR (ORD (HSTART) + 1);
  HFSTART := PTR(ORD(HEND^.HWR));
  HFEND := PTR(ORD(HEND^.HWL));
  WRITELN(TTY,'  Free list head ',ORD(HFSTART):6:O,
    '  Free list tail ', ORD(HFEND):6:O);
  HEND := PTR (ORD(HEND) + 1);
  HROVER := PTR(ORD(HEND^.HWR));
  WRITELN(TTY, '  Rover pointer at ',ORD(HROVER):6:O);
  WRITELN(TTY);
(*
  writeln(tty, 'New. calls=',CNNEW.HWR:6,'   dspos. calls=',CNDISP.HWR:6);
  writeln (tty,'Coalesce=',CNCOAL.HWR:6,'  extend=',CNEXT.HWR:6,
    '.');
  writeln(tty,'Exam=',CNEXAM.HWR:6,'  splits=',CNSPLI.HWR:6, '.');
  writeln(tty);
*)
  HEND := PTR (ORD(HSTART^.HWR));
  HSTART := PTR (ORD(HSTART) + 4);
  HCUR := HSTART;
  HCOUNT := 0;
  loop
    HADR := PTR (ORD(HCUR)+64);
    if ORD(HADR) >= ORD(HEND) then
      HADR := PTR (ORD(HEND));
    WRITE(TTY,ORD(HCUR):6:O,'  ');
    while ORD(HCUR) < ORD(HADR) do begin
      if HCOUNT = 0 then begin
	FREE := (HCUR^.HWL = 0);
	if not FREE then
	  HCOUNT := 1000000B - HCUR^.HWR
	else HCOUNT := HCUR^.HWR;
	W_TICK('$')
      end
      else if FREE then
	W_TICK('_')
      else W_TICK('^');
    if (( ord (HCUR) - ord (HSTART)) mod 20b) = 0 then write(tty,' ')
    end;
    WRITELN(TTY);
  exit if ORD(HCUR) >= ORD(HEND) - 1;
  end;
  WRITELN(TTY); 
  if ORD(HFSTART) < ORD(HSTART) then
    WRITELN(TTY, 'No free list in current heap.')
  else begin
    WRITELN(TTY, 'Walk of free list, sizes in octal words');
    WRITELN(TTY);
    HCUR := HFSTART;
    repeat
      if HCUR = HROVER then
	WRITE(TTY, '* ')
      else WRITE(TTY,'  ');
      if ord (HCUR) < ord (HSTART) then
	writeln(tty, '          Heap header block')
      else writeln(tty,ord(HCUR):6:o,'    size ',ord(HCUR^.HWR):6:o,
	'B');
      HCUR := PTR (ORD(HCUR) + 1);
      HCUR := PTR (ORD(HCUR^.HWR));
    until HCUR = HFSTART
  end;
  WRITELN(TTY)
end.
