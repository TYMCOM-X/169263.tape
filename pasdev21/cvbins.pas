$header cvbins.hdr

$INCLUDE DTIME.TYP

TYPE
   INT = MININT..MAXINT;
   INT_STR = STRING[11];

PUBLIC FUNCTION CV_BIN_STR(INT_PARAM: INT): INT_STR;

TYPE
   DIGIT_TAB = ARRAY [0..9] OF CHAR;

CONST
   DIGIT_TXT: DIGIT_TAB := ('0', '1', '2', '3', '4', '5',
      '6', '7', '8', '9' );

VAR
   RESULT: INT_STR;
   I: 1..11;
   INT_VAL: INT;

BEGIN
   INT_VAL := ABS(INT_PARAM);
   RESULT := '';
   CV_BIN_STR := '';

   REPEAT

      RESULT := RESULT || DIGIT_TXT[INT_VAL MOD 10];
      INT_VAL := INT_VAL DIV 10

   UNTIL INT_VAL = 0;

   FOR I := LENGTH(RESULT) DOWNTO 1 DO
      CV_BIN_STR := CV_BIN_STR || RESULT[I]

END.
  