(*$&+ *)

LABEL 1;

VAR
  SIXOVERPI, X, Y, Z : REAL;
  I, J, K, XL, XH : INTEGER;
  ANGLE : ARRAY[-9..9,-9..9] OF REAL;
  PLACES : INTEGER;

 PROCEDURE MATRIX;

 BEGIN
  PAGE;
  WRITE (' ':2);
  FOR I := XL TO XH DO
    WRITE (I:7);
  WRITELN; WRITELN;
  WRITE (' ':5);
  FOR I := 1 TO 70 DO
    WRITE ('-');
  WRITELN; WRITELN;
  FOR J := 9 DOWNTO -9 DO BEGIN
    WRITE (J:2,' :');
    FOR I := XL TO XH DO BEGIN
      IF (I=0) AND (J=0) THEN 
	WRITE ('   ****')
      ELSE WRITE (ANGLE[I,J]:7:PLACES)
    END;
    WRITELN (' : ',J:2);
    WRITELN
  END;
  WRITE (' ':5);
  FOR I := 1 TO 70 DO
    WRITE ('-');
  WRITELN; WRITELN;
  WRITE (' ':2);
  FOR I := XL TO XH DO
    WRITE (I:7);
  WRITELN
 END;

BEGIN
  SIXOVERPI := 6.0/3.14159;
  X := 0.0;
  FOR I := 1 TO 9 DO BEGIN
    X := X + 1.0;
    Y := 0.0;
    ANGLE[0,I] := 0.0;
    ANGLE[I,0] := 3.0;
    ANGLE[0,-I] := 6.0;
    ANGLE[-I,0] := 9.0;
    FOR J := 1 TO 9 DO BEGIN
      Y := Y + 1.0;
      Z := ARCTAN(X/Y)*SIXOVERPI;
      ANGLE[I,J] := Z;
      ANGLE[I,-J] := 6.0-Z;
      ANGLE[-I,-J] := 6.0+Z;
      ANGLE[-I,J] := 12.0-Z
    END
  END;

  REWRITE (TTYOUTPUT);
  REWRITE (OUTPUT,'MATRIXOUT');
  %1: WRITE (TTY,' Two decimal places or three? ');
  BREAK;
  READLN (TTY);
  READ (TTY,PLACES);
  IF (PLACES<2) OR (PLACES>3) THEN BEGIN
    WRITE (TTY,' What?');
    BREAK;
    GOTO 1
  END; \
  1: PLACES := 3;
  XL := 0; XH := 9;
  MATRIX;
  XL := -9; XH := 0;
  FOR J := 1 TO 9 DO
    ANGLE[0,J] := 12.0;
  MATRIX;
  CLOSE (OUTPUT);
  WRITELN (TTY,' DONE');
  BREAK
END.
   