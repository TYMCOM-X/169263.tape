(*
 *  DTTEST - TEST PROGRAM FOR PASCAL DAY/TIME ROUTINES.
 *)

PROGRAM DTTEST OPTIONS nocheck;

$INCLUDE IDTIME.TYP
$INCLUDE DTIME.INC
$INCLUDE INFPAC.INC

VAR
   TESTNUM: MINIMUM(INTEGER)..MAXIMUM(INTEGER);
   PARAM1,PARAM2,PARAM3: MINIMUM(INTEGER)..MAXIMUM(INTEGER);
   RESULT1,RESULT2: MINIMUM(INTEGER)..MAXIMUM(INTEGER);
   DATE_BIN: DATEREC;
   TIME_BIN: TIMEREC;
   DTIME_BIN: DTIMEREC;
   ERR_CODE: DTIME_ERR;
   DT_EXT: NS_EXT;
   CH: CHAR;
   dtime1,dtime2: dtime_int;
   time1:  time_int;
   date1:  date_int;

$PAGE routines to read/write internal day/times, days and times


$IF ( VAX )

procedure read_dtime ( var dtime1: dtime_int );
  begin
    read ( tty, param1:10:h, param2:10:h );
    dtime1.d1 := param1;
    dtime1.d2 := param2;
  end;

procedure read_time ( var time1: time_int );
  begin
    read ( tty, param1:10:h, param2:10:h );
    time1.d1 := param1;
    time1.d2 := param2;
  end;

procedure read_date ( var date1: date_int );
  begin
    read ( tty, param1:10:h, param2:10:h );
    date1.d1 := param1;
    date1.d2 := param2;
  end;
procedure read_date ( var date1: date_int );
  begin
    read ( tty, param1:8:o );
    date1.d := param1;
    date1.t := 0;
  end;


procedure write_dtime ( dtime1: dtime_int );
  begin
    writeln ( tty, dtime1.d1:10:h, dtime1.d2:10:h );
  end;

procedure write_date ( date1: date_int );
  begin
    writeln ( tty, date1.d1:10:h, date1.d2:10:h );
  end;

procedure write_time ( time1: time_int );
  begin
    writeln ( tty, time1.d1:10:h, time1.d2:10:h );
  end;

$ENDIF

$IF ( TOPS10 )

PROCEDURE READ_DTIME ( VAR DTIME1: DTIME_INT );
  BEGIN
    READ ( TTY, PARAM1:8:o, PARAM2:8:O );
    DTIME1.D := PARAM1;
    DTIME1.T := PARAM2;
  END;

procedure read_time ( var time1: time_int );
  begin
    read ( tty, param1:8:o );
    time1.d := 0;
    time1.t := param1;
  end;

procedure read_date ( var date1: date_int );
  begin
    read ( tty, param1:8:o );
    date1.d := param1;
    date1.t := 0;
  end;

procedure write_dtime ( dtime1: dtime_int );
  begin
    writeln ( tty, dtime1.d:8:o, dtime1.t:8:o );
  end;

procedure write_date ( date1: date_int );
  begin
    writeln ( tty, date1.d:8:o );
  end;

procedure write_time ( time1: time_int );
  begin
    writeln ( tty, time1.t:8:o );
  end;

$ENDIF
$PAGE
PROCEDURE TDAYTIME;

   BEGIN
      DTIME1 := DAYTIME;
      WRITE(TTY,'DAYTIME:');
      write_dtime ( dtime1 );
   END;


PROCEDURE TDC_EXT;

   BEGIN
      read_dtime ( dtime1 );
      WRITELN(TTY,'DC_EXT:  ',DC_EXT(DTIME1))
   END;


PROCEDURE TEC_EXT;

   BEGIN
      DT_EXT := '';
      WHILE NOT EOLN(TTY) DO
      BEGIN
         READ(TTY,CH);
         DT_EXT := DT_EXT || CH
      END;

      EC_EXT(ERR_CODE, DT_EXT, DTIME1);

      WRITE(TTY,'EC_EXT: ', ORD(ERR_CODE) );
      write_dtime ( dtime1 );
      WRITELN(TTY, '        ', DC_EXT(DTIME1))
   END;


PROCEDURE TEXTR_DATE;

   BEGIN
      read_dtime ( dtime1 );
      DATE1 := EXTR_DATE(DTIME1);
      WRITE(TTY,'EXTR_DATE:');
      write_date ( date1 );
   END;


PROCEDURE TEXTR_TIME;

   BEGIN
      read_dtime ( dtime1 );
      TIME1 := EXTR_TIME(DTIME1);
      WRITE(TTY,'EXTR_TIME:');
      write_time ( time1 );
   END;


PROCEDURE TDT_COMBINE;

   BEGIN
      read_date ( date1 );
      read_time ( time1 );
      DTIME1 := DT_COMBINE(DATE1,TIME1);
      WRITE(TTY,'DT_COMBINE:');
      write_dtime ( dtime1 );
   END;


PROCEDURE TDAYS_DIFF;

   BEGIN
      read_dtime ( dtime1 );
      read_dtime ( dtime2 );
      RESULT1 := DAYS_DIFF(DTIME1,DTIME2);
      WRITELN(TTY,'DAYS_DIFF:',RESULT1:13,RESULT1:13:H)
   END;


PROCEDURE TSECS_DIFF;

   BEGIN
      read_dtime ( dtime1 );
      read_dtime ( dtime2 );
      RESULT1 := SECS_DIFF(DTIME1,DTIME2);
      WRITELN(TTY,'SECS_DIFF:',RESULT1:13,RESULT1:13:H)
   END;


PROCEDURE TADD_DAYS;

   BEGIN
      read_dtime ( dtime1 );
      READ(TTY,PARAM2);
      DTIME1 := ADD_DAYS(DTIME1,PARAM2);
      WRITE(TTY,'ADD_DAYS:');
      write_dtime ( dtime1 );
   END;


PROCEDURE TADD_SECS;

   BEGIN
      read_dtime ( dtime1 );
      READ(TTY,PARAM2);
      DTIME1 := ADD_SECS(DTIME1,PARAM2);
      WRITE(TTY,'ADD_SECS:');
      write_dtime ( dtime1 );
   END;



PROCEDURE TDAY_OF_WEEK;

   BEGIN
      read_date ( date1 );
      RESULT1 := ORD(DAY_OF_WEEK(DATE1));
      WRITELN(TTY,'DAY_OF_WEEK:',RESULT1:3)
   END;


PROCEDURE TNS_D1;

   BEGIN
      read_date ( date1 );
      WRITELN(TTY,'NS_D1:  ',NS_D1(DATE1))
   END;


PROCEDURE TNS_D2;

   BEGIN
      read_date ( date1 );
      WRITELN(TTY,'NS_D2:  ',NS_D2(DATE1))
   END;


PROCEDURE TNS_T1;

   BEGIN
      read_time ( time1 );
      WRITELN(TTY,'NS_T1:  ', NS_T1(TIME1))
   END;


PROCEDURE TGMDTIME;

   BEGIN
      DTIME1 := GMDTIME;
      WRITE(TTY,'GMDTIME:');
      write_dtime ( dtime1 );
   END;


PROCEDURE TDC_DTIME;

   BEGIN
      read_dtime ( dtime1 );
      DTIME_BIN := DC_DTIME(DTIME1);
      WITH DTIME_BIN DO
         WRITELN(TTY, 'DC_DTIME:', YEAR:6, MONTH:4, DAY:4,
            HOURS:8, MINS:4, SECS:4 )
   END;


PROCEDURE TEC_DTIME;

   BEGIN
      WITH DTIME_BIN DO
         READ(TTY,YEAR, MONTH, DAY, HOURS, MINS, SECS);
      EC_DTIME(ERR_CODE, DTIME_BIN, DTIME1);
      WRITE(TTY,'EC_DATE:', ORD(ERR_CODE):3);
      write_dtime ( dtime1 );
   END;


PROCEDURE TEC_DATE;

   BEGIN
      WITH DATE_BIN DO
      BEGIN
         READ(TTY,YEAR,MONTH,DAY);
         EC_DATE(ERR_CODE,DATE_BIN,DATE1);
         WRITE(TTY,'EC_DATE:',ORD(ERR_CODE):3);
         write_date ( date1 );
      END
   END;


PROCEDURE TDC_DATE;

   BEGIN
      read_date ( date1 );
      DATE_BIN := DC_DATE(DATE1);
      WITH DATE_BIN DO
         WRITELN(TTY,'DC_DATE:',YEAR:6,MONTH:4,DAY:4)
   END;


PROCEDURE TEC_TIME;

   BEGIN
      WITH TIME_BIN DO
      BEGIN
         READ(TTY,HOURS,MINS,SECS);
         EC_TIME(ERR_CODE,TIME_BIN,TIME1);
         WRITE(TTY,'EC_TIME:',ORD(ERR_CODE):3);
         write_time ( time1 );
      END
   END;


PROCEDURE TDC_TIME;

   BEGIN
      read_time ( time1 );
      TIME_BIN := DC_TIME(TIME1);
       WITH TIME_BIN DO
         WRITELN(TTY,'DC_TIME:',HOURS:4,MINS:4,SECS:4)
   END;



PROCEDURE TEC_DCDATE;

   BEGIN
      READ(TTY,PARAM1);
      DATE1 := EC_DCDATE(PARAM1);
      WRITE(TTY, 'EC_DCDATE:');
      write_date ( date1 );
   END;


PROCEDURE TEC_DCTIME;

   BEGIN
      RESULT1 := TIME;
      WRITELN(TTY, 'CURRENT DEC TIME:', RESULT1:13,RESULT1:13:H);
      BREAK(TTY);
      READ(TTY,PARAM1);
      TIME1 := EC_DCTIME(PARAM1);
      WRITE(TTY, 'EC_DCTIME:');
      write_time ( time1 );
   END;
$PAGE

(*
 *  BEGIN MAIN ROUTINE.
 *)
BEGIN

   OPEN(TTY);
   REWRITE(TTY);

   LOOP
      WRITE(TTY,'ENTER TEST NUMBER:');
      BREAK(TTY);
      READ(TTY,TESTNUM);

      EXIT IF TESTNUM <= 0;

      CASE TESTNUM OF

         1: 	TDAYTIME;

         2:	TDC_EXT;

         3:	TEC_EXT;

         4:	TEXTR_DATE;

         5:	TEXTR_TIME;

         6:	TDT_COMBINE;

         7:	TDAYS_DIFF;

         8:	TSECS_DIFF;

         9:	TADD_DAYS;

         10:	TADD_SECS;

         11:	TDAY_OF_WEEK;

         12:	TNS_D1;

         13:	TNS_D2;

         14:	TNS_T1;

         15:	TGMDTIME;

         16:	TDC_DTIME;

         17:	TEC_DTIME;

         18:	TEC_DATE;

         19:	TDC_DATE;

         20:	TEC_TIME;

         21:	TDC_TIME;

         22:	TEC_DCDATE;

         23:	TEC_DCTIME


      END  (* CASE *)

   END (* LOOP *)

END.
