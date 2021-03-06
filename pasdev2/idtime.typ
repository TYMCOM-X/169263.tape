(*---------------------------------------------------------------------------*)
(*
 *  INCLUDE FILE FOR PASCAL DAY/TIME ROUTINES PACKAGE.
 *)

CONST
   MAX_YEAR = 2217;
   DAY_ZERO = 'NOV 17, 1858';

TYPE
    D_T_RECORD = PACKED RECORD D, T: 0 .. 777777B END;
   DTIME_INT = D_T_RECORD;	(* INTERNAL DAY/TIME *)
   DTIME_EXT = PACKED ARRAY [1..18] OF CHAR; (* STANDARD EXTERNAL DAY/TIME *)
   NS_EXT = STRING[24];		(* FOR NON-STANDARD EXTERNAL DAY/TIMES *)
   DATE_INT = D_T_RECORD;	(* INTERNAL DATE *)
   TIME_INT = D_T_RECORD;	(* INTERNAL TIME *)
   DEC_DATE = 0..77777B;	(* 15 BIT DEC DATE *)
   TYM_DATE = 0..77777B;	(* 15 BIT TYMSHARE DATE *)
   DEC_TIME = 0..86400000;	(* MILLISECONDS SINCE MIDNIGHT TIME *)
   DTIME_ERR = (DT_NOERR,DT_ERR);	(* ERROR CODES *)
   DAYS = INTEGER;
   SECONDS = INTEGER;
   WEEK_DAY = (SUNDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,
               FRIDAY,SATURDAY);
   NS_DATE1 = PACKED ARRAY [1..8] OF CHAR;	(* 'MM/DD/YY' *) 
   NS_DATE2 = PACKED ARRAY [1..12] OF CHAR;	(* 'MMM DD, YYYY' *)
   NS_TIME1 = PACKED ARRAY [1..11] OF CHAR;	(* 'HH:MM:SS PM' *)
   DTIMEREC = RECORD		(* BINARY DAY/TIME RECORD *)
      YEAR: 1858..MAX_YEAR;
      MONTH: 1..12;
      DAY: 1..31;
      HOURS: 0..23;
      MINS: 0..59;
      SECS: 0..59
   END;
   DATEREC = RECORD		(* BINARY DATE RECORD *)
      YEAR: 1858..MAX_YEAR;
      MONTH: 1..12;
      DAY: 1..31
   END;
   TIMEREC = RECORD		(* BINARY TIME RECORD *)
      HOURS: 0..23;
      MINS: 0..59;
      SECS: 0..59
   END;

CONST
  BASE_DTIME_INTERNAL: DTIME_INT = ( 0, 0 );
(*---------------------------------------------------------------------------*)
 