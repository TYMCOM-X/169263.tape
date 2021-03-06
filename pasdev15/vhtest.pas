$LENGTH 44
$TITLE VHTEST A test program for area manipulation
program VHTEST;

type
  AREA_ID = 0..777B;
  FNSTR = string [30] ;
  ATAB = array [1..20] of AREA_ID;
  TRPTR = ^TREE;
  TREE = record 
    LEFT, RIGHT: TRPTR; 
    LEN: 0..255;
    TXT: packed array [1..255] of CHAR 
  end;

$INCLUDE RLB:QUERY.INC


(*$X10
type
  BIGSTUFF= 0..377777777777b;

external var
  C$NEW, C$DISP, C$COAL, C$SPLI, C$EXAM, C$XTND,
  C$VTRN, C$FTRN, C$FXLT, C$VMAC, C$PTIN, C$VPIN: BIGSTUFF;
*)

public var
  TPTR: 0..377777777777B := 0;
  TABLE: ATAB := (
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    0, 0, 0, 0, 0 );
  TR: TRPTR := NIL;
  FN: FNSTR := '';
  CH: CHAR;

function GETIDX: BOOLEAN;

begin
  GETIDX := FALSE;
  WRITE(TTY, 'Index -- ');
  BREAK(TTY);
  READLN(TTY);
  if not EOLN(TTY) then begin
    READ(TTY, TPTR); 
    GETIDX := true;
    TPTR := TPTR
  end
end;

function NEWSTR (COPY: TRPTR): TRPTR;

  var
    TMPTR: TRPTR;

begin
  NEW (TPTR , TMPTR: COPY^.LEN);
  TMPTR^.LEN := COPY^.LEN;
  TMPTR^.TXT [1: TMPTR^.LEN] := COPY^.TXT [1: COPY^.LEN] ;
  if COPY^.LEFT <> NIL then
    TMPTR^.LEFT := NEWSTR (COPY^.LEFT)
  else TMPTR^.LEFT := NIL;
  if COPY^.RIGHT <> NIL then
    TMPTR^.RIGHT := NEWSTR (COPY^.RIGHT)
  else TMPTR^.RIGHT := NIL;
  NEWSTR := TMPTR
end;

function GETSTR: TRPTR;

  var 
    TSTR: string [255] ;
    TEMPTR: TRPTR;

begin 
  WRITE(TTY, 'Type string: ');
  BREAK(TTY);
  READLN(TTY);
  TSTR := ''; 
  while not EOLN(TTY) do begin
    TSTR := TSTR || TTY^; 
    GET(TTY) 
  end;
  NEW (TPTR , TEMPTR: LENGTH(TSTR) );
  with TEMPTR^ do begin
    LEN := LENGTH(TSTR); 
    TXT [1: LENGTH(TSTR)] := SUBSTR(TSTR, 1, LENGTH(TSTR));
    RIGHT := NIL; 
    LEFT := NIL 
  end;
  GETSTR := TEMPTR
end;

procedure COERCE (T: TRPTR);

begin
  with T^ do begin
    if LEFT <> NIL then begin
      LEFT := POINTER (AREAID(T), LEFT);
      COERCE (LEFT)
    end;
    if RIGHT <> NIL then begin
      RIGHT := POINTER (AREAID(T), RIGHT);
      COERCE (RIGHT)
    end
  end
end;

procedure WALK (T: TRPTR);

begin 
  if T^.LEFT <> NIL then WALK(T^.LEFT);
  TPTR := TPTR + 1;
  if T^.RIGHT <> NIL then WALK(T^.RIGHT) 
end;

begin 
  OPEN(TTY);
  REWRITE(TTYOUTPUT);
  loop 
    WRITE(TTY, '%- ');
    BREAK(TTY);
    READLN(TTY);
    if EOLN(TTY) then stop 
    else READ(TTY, CH);
    case UPPERCASE(CH) of
      'C':
	begin 
	  TPTR := AREA_CREATE;
	  writeln (tty, 'Area = ', TPTR:2)
	end;
      'D':
	begin 
	  if GETIDX then begin
	    AREA_DELETE(TPTR );
	    TPTR := 0 
	  end 
	end;
      'W':
	begin
	  if GETIDX then begin
	    TR := ROOT(TPTR );
	    if TR = NIL then WRITELN(TTY, 'No tree in area.')
	    else begin
	      TPTR := 0;
	      WALK (TR);
	      WRITELN (TTY, TPTR, ' nodes in area.')
	    end
	  end 
	end;
      'L':
	begin 
	  if GETIDX then begin
	    FN := '';
	    WRITE(TTY, 'File name: ');
	    BREAK(TTY);
	    READLN(TTY);
	    while not EOLN(TTY) do begin
	      READ(TTY, CH); 
	      FN := FN||CH 
	    end;
	    AREA_LOAD (TPTR , FN) ;

	    TR := ROOT (TPTR );
	    TR := POINTER (TPTR , TR);
	    SETROOT ( AREAID (TR), TR );
	    if TR = NIL then
	      WRITELN (TTY, 'No coercion needed.')
	    else COERCE (TR)

	  end 
	end;
      'S':
	begin 
	  if GETIDX then 
	    if ROOT (TPTR ) = NIL
	    then WRITELN(TTY, 'No root in area ', TPTR : 3)
	    else begin
	      FN := '';
	      WRITE(TTY, 'File name: ');
	      BREAK(TTY);
	      READLN(TTY);
	      while not EOLN(TTY) do begin 
		READ(TTY, CH);
		FN := FN||CH 
	      end;
	      AREA_SAVE (TPTR , FN) 
	    end 
	end;
      'R':
	begin
	  if GETIDX then begin
	    SETROOT (TPTR , GETSTR)
	  end;
	end;

      (* other cases should be added here *)

      'P': begin

	   (*$Y10
	   writeln (tty, 'Print trace not available.')
	   *)
	   (*$X10
	   writeln (tty, 'Heap data: ');
	   writeln(tty, 'New calls:',C$NEW,'       Dispose calls:',C$DISP);
	   writeln(tty, 'Coalesces:',C$COAL,'       Blocks split: ',C$SPLI);
	   writeln(tty, 'Examined: ',C$EXAM,'       Extensions:   ',C$EXAM);
	   writeln(tty); writeln(tty, 'Translation data:');
	   writeln(tty, 'VTRNS. calls:',C$VTRN,'      FTRNS. calls:',C$FTRN);
	   writeln(tty, 'F xlations:  ',C$FXLT,'      VMACC. calls:',C$VMAC);
	   writeln(tty, 'Page table in:',C$PTIN,'     Page in:     ',C$VPIN);
	   *)

	   end;

      'A':
	begin 
	  if GETIDX then AREA_IN(TPTR ) 
	end;
      'O':
	begin 
	  if GETIDX then AREA_OUT(TPTR ) 
	end;
      'T':
	begin
	  FN := '';
	  write (tty, 'File name:');
	  break (tty); readln (tty);
	  while not eoln (tty) do begin
	    read (tty, CH);
	    FN := FN || CH
	    end;
	  HEAP_LOAD (FN, QUERY ('End preserve'))
	end;
      'B':
	begin
	  FN := '';
	  write (tty, 'File name:');
	  break (tty); readln (tty);
	  while not eoln (tty) do begin
	    read (tty, CH);
	    FN := FN || CH
	    end;
	  HEAP_CREATE (FN, QUERY ('End preserve'))
	end;
      'F': 
	begin
	  FN := '';
	  write (tty, 'File name:');
	  break (tty); readln (tty);
	  while not eoln (tty) do begin
	    read (tty, CH);
	    FN := FN || CH
	    end;
	  HEAP_SAVE (FN)
	end;
      'I':
	begin 
	  if GETIDX then begin 
	    TR := ROOT (TPTR );
	    if TR = NIL then WRITELN(TTY, 'No root node in area.') 
	    else 
	      repeat 
		WRITELN(TTY, TR^.TXT: TR^.LEN);
		if TR^.LEFT = NIL 
		then WRITE(TTY, '  [ AL, ')
		else WRITE(TTY, '  [ L, ');
		if TR^.RIGHT = NIL 
		then WRITE(TTY, 'AR ]')
		else WRITE(TTY, 'R ]');
		BREAK(TTY);
		READLN(TTY);
	      exit if EOLN(TTY) do TR := NIL;
		READ (TTY, CH);
		if UPPERCASE (CH) = 'L' then begin
		  if TR^.LEFT = NIL then
		    begin
		    if UPPERCASE (TTY^) = 'A' then
		      TR^.LEFT := NEWSTR (ROOT (TPTR ))
		    else TR^.LEFT := GETSTR
		  end
		  else TR := TR^.LEFT
		end
		else if UPPERCASE (CH) = 'R' then begin
		  if TR^.RIGHT = NIL then
		    begin
		    if UPPERCASE (TTY^) = 'A' then
		      TR^.RIGHT := NEWSTR (ROOT (TPTR ))
		    else TR^.RIGHT := GETSTR
		  end
		  else TR := TR^.RIGHT
		end
		else WRITELN(TTY, 'Please pick one, or <cr> to stop insert.')
	      until TR = NIL 
	  end 
	end
    end (* case *);
  end (* loop *);
end (* program *).
  