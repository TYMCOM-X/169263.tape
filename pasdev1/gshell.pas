(*$&+,O-*)
VAR N,I: INTEGER; R: REAL;
  OUT: FILE OF INTEGER;
BEGIN
  REWRITE(OUT,'INPUT    ');
  READ(TTY,N);
  R:= 0.5;
  FOR I:=1 TO N DO
  BEGIN  R:= RANDOM(R); OUT^:= TRUNC(10000.0*R);
    PUT(OUT)
  END
END.
    