module PASDIR
  OPTIONS SPECIAL, NOCHECK, NOLIBRARY;
$LENGTH 44
$TITLE PASDIR -- Directory Manipulation Routines
$PAGE various declarations

$SYSTEM dtime.typ[31024,320156]
$SYSTEM dtime.inc[31024,320156]
$SYSTEM pasdir.typ[31024,320156]

type
  SIXBIT = 0..77B;
  SIXARRAY = packed array [1..6] of 
    SIXBIT;
  HALFWORD = 0..777777B;

  LOOKUPBLOCKADDRESS = ^FILEWORD;
  FILEWORD = packed record
    case BOOLEAN of
      TRUE : (FILENAME: SIXARRAY);
      FALSE: (PROJNUM: HALFWORD;
	      PROGNUM: HALFWORD)
  end;

  DIRECTENTRY = packed record
    FILENM: SIXARRAY;
    XTENSION: SIXARRAY
  end;

static var
  DIR_FILES: array [1..15] of file of DIRECTENTRY;
  DIR_FREE: array [1..15] of BOOLEAN := (
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE );

  TEMPFILE: TEXT;

external procedure DIR_DIE;

$PAGE CLOSE_DIR to close a directory file pointer
procedure CLOSE_DIR (I: HALFWORD);

  (* CLOSE_DIR will close the I-th directory file and mark it as free. *)

begin
  if ((I >= 1) and (I <= 15)) andif not DIR_FREE [I] then
  begin
    CLOSE (DIR_FILES [I]);
    DIR_FREE [I] := TRUE
  end 
end;
$PAGE GET_DIR to assign a directory array index
procedure GET_DIR (var I: HALFWORD);

  (* GETDIR assigns a free index in the directory file array. *)

     var
       IDX: HALFWORD;

     procedure DIR_TOO_MANY;

     (* a helper to abort the program if too many directories are opened. *)

  begin 
    WRITELN(TTY);
    WRITELN(TTY, '?  Too many directories open.');
    BREAK(TTY);
    DIR_DIE 
  end;

begin
  I := 0;
  for IDX := 1 to 15 do
  exit if DIR_FREE [IDX] do I := IDX;

  if I = 0 then DIR_TOO_MANY;
  DIR_FREE [I] := FALSE
end;
$PAGE DIR_OPEN to open a directory file
public procedure DIR_OPEN (var ERR: DIR_ERRORS;
			   var INT_ID: DIR_INT_ID;
			   EXT_ID: DIR_EXT_ID);

  (* DIR_OPEN opens the directory file of the PPN given by EXT_ID. If the
     PPN given is invalid, orif the directory file cannot be opened, an error
     code is returned, and the file record is returned to the available
     pool. At present, it is not disposed, but its filename field is set to
     null (zero). *)

  var
    IDX: integer;				(* for SFD hack *)

begin
  GET_DIR (INT_ID);                               (* allocate a file block *)
  ERR := DIR_OK;
  IDX := index (EXT_ID, ',', length (EXT_ID) );
  IDX := index ( substr (EXT_ID, IDX + 1), ',' );

  (* figures out if an SFD specification may be present (two or more commas) *)

  if IDX = 0 then
    reset (DIR_FILES [INT_ID], EXT_ID || '.UFD')
  else begin
    IDX := length (EXT_ID);		(* search for rightmost comma *)
    while (IDX > 0) andif (EXT_ID [IDX] <> ',') do
      IDX := IDX -1;
    if IDX > 0 then			(* assume last SFD name right of comma *)
      reset (DIR_FILES [INT_ID], substr (EXT_ID, 1, IDX -1) || '].SFD ' ||
	substr (EXT_ID, IDX +1,
	  index (substr (EXT_ID, IDX + 1), ']') - 1 ) (* length *) )

	(* the above verbiage is an expression giving 'FOO.SFD[junk]'
	   taken from the string '[junk,FOO]' *)

    else reset (DIR_FILES [INT_ID], EXT_ID || '.UFD')
    end;
  if EOF(DIR_FILES [INT_ID]) then begin
    ERR := DIR_NO_OPEN;
    CLOSE_DIR(INT_ID)
  end
end;
$PAGE DIR_NEXT to get next file in directory
public procedure DIR_NEXT (var ERR: DIR_ERRORS;
			   INT_ID: DIR_INT_ID;
			   var FNAME: DIR_FNAME);

  (* DIR_NEXT returns the name of the next file in the directory associated
     with the internal pointer INT_ID. If there are no more fils, ERR is set
     to DIR_EOF, and FNAME is set to the null string. *)

  var 
    WHICHF: file of DIRECTENTRY;
    I: HALFWORD;

begin 
  ERR := DIR_OK;
  FNAME := '';
  if (INT_ID < 1) or
  (INT_ID > 15) then
    ERR := DIR_BAD_INT_ID
  else begin
    if DIR_FREE [INT_ID] then
      ERR := DIR_NOT_OPEN 
    else begin
      WHICHF := DIR_FILES [INT_ID] ;
      while (WHICHF^.FILENM [1] = 0) andif not EOF(WHICHF) do
	GET(WHICHF);                           (* get past zero entries *)
      if EOF(WHICHF) then
	ERR := DIR_EOF 
      else begin
	for I := 1 to 6 do
	  if WHICHF^.FILENM [I] <> 0 then
	    FNAME := FNAME || CHR((WHICHF^.FILENM [I] ) + 40B);
	FNAME := FNAME || '.';
	for I := 1 to 3 do
	  if WHICHF^.XTENSION [I] <> 0 then
	    FNAME := FNAME || CHR((WHICHF^.XTENSION [I] ) + 40B);
	GET(WHICHF)
      end 
    end 
  end 
end;
$PAGE DIR_CLOSE to close a directory file
public procedure DIR_CLOSE ( var ERR: DIR_ERRORS;
			     INT_ID: DIR_INT_ID);

begin
  if ((INT_ID >= 1) and (INT_ID <= 15)) 
  then begin
    if not DIR_FREE [INT_ID] then
    begin
      CLOSE (DIR_FILES [INT_ID]);
      DIR_FREE [INT_ID] := TRUE;
      ERR := DIR_OK
    end
    else ERR := DIR_NOT_OPEN
  end
  else ERR := DIR_BAD_INT_ID
end.
