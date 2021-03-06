$header nst1.hdr
$INCLUDE DTIME.TYP

TYPE
   INT = MININT..MAXIMUM(INTEGER);
   STR2 = STRING[2];

EXTERNAL FUNCTION DC_TIME(TIME_INT): TIMEREC;
EXTERNAL FUNCTION CHARS2(INT): STR2;


PUBLIC FUNCTION NS_T1(TIME: TIME_INT): NS_TIME1;

VAR
   TIME_BIN: TIMEREC;
   AM: PACKED ARRAY [1..2] OF CHAR;

BEGIN

   TIME_BIN := DC_TIME(TIME);

   WITH TIME_BIN DO

   BEGIN
   if time < 377777b then begin
      am := 'AM';
      if hours = 0 then hours := 12;
   end
   else begin
      am := 'PM';
      if hours > 12 then hours := hours - 12;
   end;

      NS_T1 := CHARS2(HOURS) || ':'
         || CHARS2(MINS) || ':'
         || CHARS2(SECS) || ' '
         || AM

   END


END.
  