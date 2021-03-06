MODULE ECDATE
  OPTIONS NOLIBRARY;

$HEADER ECDATE.HDR[31024,320220]

$SYSTEM IDTIME.TYP
$SYSTEM DTIMEI.INC[31024,320156]

PUBLIC PROCEDURE EC_DATE(VAR ERR_CODE: DTIME_ERR;
   DATE_BIN: DATEREC; VAR DATE: DATE_INT);

TYPE
   MONTH_TAB = ARRAY [1..12] OF 0..365;

CONST
   DAYS_IN_MONTH: MONTH_TAB = (31, 29 , 31, 30, 31, 30,
      31, 31, 30, 31, 30, 31 );
   DAYS_BEFORE: MONTH_TAB = (0 , 31, 59, 90, 120,
      151,181, 212, 243, 273, 304, 334 );

VAR
   DAYS_SINCE: 0..MAXIMUM(INTEGER);
   QUAD_CENT, CENTS, QUAD_YEARS: 0..MAXIMUM(INTEGER);
   LEAP_YEAR: BOOLEAN;

BEGIN

   WITH DATE_BIN DO

   BEGIN
      
      (* ERROR CHECK DATE PASSED IN *)

      IF (YEAR < 1858) ORIF
         (MONTH < 1) ORIF
         (MONTH > 12) ORIF
         (DAY < 1) ORIF
         (DAY > DAYS_IN_MONTH[MONTH])
      THEN ERR_CODE := DT_ERR

      ELSE

      BEGIN
         ERR_CODE := DT_NOERR;

         IF YEAR <> 1858 THEN

         BEGIN
            DAYS_SINCE := 44; (*DAYS FROM YEAR 1858 *)
            DAYS_SINCE := DAYS_SINCE + (YEAR-1859) * 365;

            (* ADD IN 1 FOR EACH LEAP YEAR.  A LEAP YEAR IS
            ASSUMED TO BE ANY YEAR DIVISIBLE BY 4 BUT NOT DIVISIBLE
            BY 100, UNLESS DIVISIBLE BY 400 ALSO.  THUS 1900 WAS 
            NOT A LEAP YEAR, THE YEAR 2000 WILL BE *)

            QUAD_CENT := (YEAR-1) DIV 400 - (1858 DIV 400);
            CENTS := (YEAR-1) DIV 100 - (1858 DIV 100);
            QUAD_YEARS := (YEAR-1) DIV 4 - (1858 DIV 4);

            DAYS_SINCE := DAYS_SINCE + QUAD_CENT
               - CENTS + QUAD_YEARS;

            (* ADD IN DAYS FOR THE COMPLETED MONTHS OF THIS YEAR *)

            DAYS_SINCE := DAYS_SINCE + DAYS_BEFORE[MONTH];

            IF MONTH > 2 THEN
            BEGIN
               LEAP_YEAR := ((YEAR MOD 4) = 0) AND
                  (((YEAR MOD 100) <> 0) OR
                  ((YEAR MOD 400) = 0));
               IF LEAP_YEAR THEN DAYS_SINCE := DAYS_SINCE + 1;
            END;

            (* NOW ADD IN THE DAYS OF THIS MONTH *)

            DAYS_SINCE := DAYS_SINCE + DAY;

         END

         ELSE

         (* SPECIAL CASE - DATE IN 1858 *)

         BEGIN
            IF MONTH = 12 THEN DAYS_SINCE := DAY + 13
            ELSE IF MONTH = 11 THEN 
               DAYS_SINCE := DAY - 17
            ELSE ERR_CODE := DT_ERR
         END;

         IF ERR_CODE <> DT_ERR THEN
	    DATE := EC_DAYS ( DAYS_SINCE );

      END (* FIRST IF STATEMENT *)

   END (* WITH *)

END.
    