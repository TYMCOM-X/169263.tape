$OPTIONS MAIN, SPECIAL, NOCHECK

VAR
    STR: STRING;
    CH: CHAR;

BEGIN
  REWRITE (TTY);
  OPEN (TTY);
  LOOP
    WRITE (TTY,'FILE: '); BREAK;
    READLN (TTY);
  EXIT IF EOLN (TTY);
    STR := '';
    WHILE NOT EOLN (TTY) DO BEGIN
      READ (TTY,CH);
      STR := STR || CH;
    END;
    RESET (INPUT,'.PAS '||STR);
    IF EOF (INPUT) THEN
      WRITELN (TTY,'CAN''T OPEN FILE ',STR)
    ELSE BEGIN
      REWRITE (OUTPUT,STR || '.LST');
      WRITELN (TTY); BREAK;
      REPEAT
	READLN;
      UNTIL EOF;
      CLOSE (INPUT);
      CLOSE (OUTPUT);
    END;
  END;
END.
