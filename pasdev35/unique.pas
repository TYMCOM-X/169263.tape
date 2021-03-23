PROGRAM UNIQUE;

CONST STR_SIZE = 256;
      END_FILE = CHR(28);

TYPE STR_PARM = STRING [STR_SIZE];

VAR S1, S2: STR_PARM;

PROCEDURE READ_STRING ( VAR S: STR_PARM );
VAR BUF: PACKED ARRAY [1..STR_SIZE] OF CHAR;
    IND: 0 .. STR_SIZE;
BEGIN
  IF EOF(INPUT) THEN
    S := END_FILE
  ELSE BEGIN
    IND := 0;
    WHILE NOT EOLN(INPUT) DO BEGIN
      IF IND <> STR_SIZE THEN BEGIN
	IND := IND + 1;
	BUF [IND] := INPUT^;
      END;
      GET (INPUT);
    END;
    READLN (INPUT);
    S := SUBSTR(BUF,1,IND);
  END;
END (* READ_STRING *);


$PAGE getiofiles
(* GETIOFILES is a routine for reading command lines.  It assumes
   that the calling program does input and output on the standard
   files INPUT and OUTPUT, that the command lines have the format:
       <output file>=<input file>
   and that an empty command line indicates a request to terminate
   the program.  *)


Public Procedure GETIOFILES;

Var
  LINE : String[80];
  IND : 0..80;

Label (* try again *)
    100;

Begin

(* try again *)
  100 :

(* Read a command line from the tty.  *)

  Write (Tty, '*');
  Break;
  Readln (Tty);
  LINE := '';
  While Not Eoln(Tty) Do Begin
    LINE := LINE || TTY^;
    Get (Tty);
  End;

(*  An empty command line means stop.  *)

  If line = '' Then
    Stop;

(*  Find the input and output file names.  *)

  IND := Search(LINE,['=']);
  If IND = 0 Then Begin
    Writeln (Tty, 'USAGE: <OUTPUT FILE>=<INPUT FILE>');
    Goto (* try again *) 100;
  End;

(*  Open and check the input file.  *)

Reset (Input, '.PAS ' || Substr(LINE, IND + 1));
  If Eof(Input) Then Begin
    Writeln (Tty, 'FILE', Substr(LINE, IND + 1), 'EMPTY OR MISSING');
    Goto (* try again *)100;
  End;

(*  Open and check the output file.  *)

  Rewrite (Output, '.PAS ' || Substr(LINE, 1, IND - 1));
  If Not Eof(Output) Then Begin
    Writeln (Tty, 'BAD OUTPUT FILE ', Substr(LINE, 1, IND - 1));
    Goto (* try again *) 100;
  End;
End (* getiofiles *);

$PAGE Main
BEGIN
  LOOP
    OPEN (TTY);
    REWRITE (TTY);
    GETIOFILES;
    READ_STRING (S1);
    REPEAT
      WRITELN (OUTPUT,S1);
      REPEAT
	READ_STRING (S2);
      UNTIL S2 <> S1;
      S1 := S2;
    UNTIL S1 = END_FILE;
    CLOSE (OUTPUT);
  END;
END (* UNIQUE *).
