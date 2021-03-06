(**** Declarations for file written by Pascal Call Statistics Facility ****)

CONST	MAXSTATCODE = 7;		(*Reserve 4 bits for future events*)
	CALLING = 0;			(*Statcode for routine call*)
	RETURNING = 1;			(*Statcode for routine return*)

TYPE	STATCODE = CALLING..MAXSTATCODE;    (*Stat record identification*)
	S_VALUE = -1..377777777777B;	(*Maximum range of timer values*)
	STATRECORD = PACKED RECORD	(*Record written for each event*)
	  NAME: PACKED ARRAY[1..10] OF CHAR;	(*Routine name or 'Unknown'*)
	  LOCATION: 0..777777B;		(*if calling.....entry address
					  if returning...return address*)
	  WHAT: STATCODE;		(*Identification of event*)
	  TIMER: S_VALUE                (*E-box timer value*)
	END;

  (* Entry points into run-time collection package available to programs
     being monitored. *)

EXTERNAL FUNCTION S_TIME: S_VALUE;	(*Returns current value of timer*)

EXTERNAL PROCEDURE S_WRITE (STATRECORD); (*Supplies timer and writes arbitrary record*)

(**************************************************************************)
  