(* Routines to handle link, sys, sub, doc, and 6 bit files *)
(* 11/02/81 - modified by djm to add code to confirm default modifier
              value in WFILECONVERT *)

TYPE TYPECODE=(FNERROR,FNOK,FNDEVICE);		(* for 940 syntax routine *)


EXTERNAL PROCEDURE FN940TO10( FILE_ID;		(* 940 style name *)
			      VAR FILE_ID;	(* pdp 10 style name *)
			      VAR TYPECODE);	(* fnerror,fnok,fndevice *)

$PAGE
$PAGE
PUBLIC PROCEDURE WFILECONVERT(FILE_NAME: FILE_ID;   (* name to convert *)
			FILE_MODIFIER: WMODIFIER;   (* type of conversion L S D B 6 7 *)
			VAR PDP_NAME: FILE_ID;	(* resultant name *)
			VAR WCODE: WCODES;	(* error codes  *)
			VAR DRCONVERT: BOOLEAN);    (* further convert on name req. *)

TYPE INTEGER= 0..99999;
     WL_OR_D= (LNUMBER,DNUMBER);		(*link number or documentation*)

VAR CODE: TYPECODE;
    WERROR: BOOLEAN;
    WNUMBER: INTEGER;
    TMODIFIER: WMODIFIER;

$PAGE  determine the user
FUNCTION  WUSER(WNUMBER: INTEGER;		(* link number to determine user *)
		WNUMBER_TYPE: WL_OR_D		(*lnumber or dnumber given*)
		): WMODIFIER;

VAR ZERO: 0..9999;
    DIGIT: 0..7;
    ACCT,N: 0..99999;

BEGIN
  (* the following computes the acct number for link names and
     link documentation names based on files per account and
     the number of accounts.

     links 1-9    user 42164
     links 10-19  user 42165
	 |               |
     links 90-99  user 42175  in other words add to the base acct the tens digit

     links 100-109   user 42217
     links 200-209   user 42252  add the hundreds digit*27 to the base acct

     the above pattern repeats for links above 999

     documentation files are arranged 30 per file for 26 user names
     and then repeats through the user names for the next 780 files *)

  IF WNUMBER_TYPE=LNUMBER THEN ACCT:=17524+((WNUMBER DIV 100)MOD 10)*27
					+((WNUMBER DIV 10) MOD 10)

  ELSE ACCT:=( ( ( WNUMBER - 1 ) MOD 780 ) DIV 30 ) + 9046;
  WUSER:='';					(* start string as null string *)
  ZERO:=ORD( '0' );				(* need internal rep of zero *)
  WHILE ACCT<>0 DO
    BEGIN
      N:= ACCT DIV 8;				(* determine octal user number *)
      DIGIT:= ACCT - N*8;			(* this gives digits from right to left *)
      WUSER:= CHR(ZERO + DIGIT) || WUSER;
      ACCT:= N
    END;
END;						(* wuser *)


$PAGE  validate the number and generate the string || .940
PROCEDURE WVALIDATE_NUMBER(FILE_NAME: FILE_ID;	(* contains only the number *)
			VAR WNUMBER: INTEGER;	(* number to be returned *)
			VAR WNEW_NAME: FILE_ID;	(* number with .940 added *)
			VAR WNUM_ERROR: BOOLEAN);   (* error if not num or bad *)

VAR I,N: INTEGER;
    J: -9999..9999;

BEGIN
  WNUM_ERROR:= FALSE;
  N:=LENGTH(FILE_NAME);
  IF N>4 THEN WNUM_ERROR:=TRUE
  ELSE BEGIN
    WNUMBER:= 0;
    FOR I:= 1 TO N DO
    BEGIN
      WNEW_NAME:=WNEW_NAME||FILE_NAME[I];
      J:= ORD(FILE_NAME[I])-ORD('0');
      IF(J<0) OR (J>9) THEN WNUM_ERROR:=TRUE
      ELSE WNUMBER:=WNUMBER*10+J

    END;
    WNEW_NAME:=WNEW_NAME||'.940';
  END
END;						(* wvalidate *)

$PAGE wfileconvert code section
BEGIN						(* this is the start of wfileconvert *)
  DRCONVERT:= FALSE;
  WCODE:= WOK;
  PDP_NAME:='';
  IF FILE_MODIFIER='' THEN
    BEGIN
      IF (LENGTH(FILE_NAME)>4) ANDIF
         (SUBSTR(FILE_NAME,LENGTH(FILE_NAME)-3,4)='.940') ANDIF
         (NOT QUERY ('7 bit file, OK')) THEN
        BEGIN
          WCODE:=WBADNAME;
          RETURN;
        END;
      WCODE:=WTENFILE;
      PDP_NAME:=FILE_NAME
    END
  ELSE BEGIN
  IF FILE_MODIFIER[1]='6' THEN
    BEGIN
      FN940TO10(FILE_NAME,PDP_NAME,CODE);
      IF CODE=FNOK THEN WCODE:=WOK
      ELSE WCODE:=WBADNAME
    END
  ELSE IF FILE_MODIFIER[1]='7' THEN 
    BEGIN
      WCODE:=WTENFILE;
      PDP_NAME:=FILE_NAME
    END
  ELSE BEGIN
    WVALIDATE_NUMBER(FILE_NAME,WNUMBER,PDP_NAME,WERROR);
    IF WERROR THEN WCODE:=WBADNAME
    ELSE
    BEGIN
      TMODIFIER:=UPPERCASE (FILE_MODIFIER);
      CASE TMODIFIER[1] OF
	'L': PDP_NAME:= PDP_NAME||'[50122,'||
				WUSER(WNUMBER,LNUMBER)||']<007>';
	'S': PDP_NAME:=PDP_NAME||'[50122,34321]<007>';
	'B': PDP_NAME:=PDP_NAME||'[50122,34124]<007>';
	'D': BEGIN
		   PDP_NAME:='';
		   CASE LENGTH(TMODIFIER) OF
		     1:  PDP_NAME:=FILE_NAME;
		     2:  BEGIN
			   IF ((TMODIFIER[2]<>'F') AND (TMODIFIER[2]<>'G') AND
			    (TMODIFIER[2]<>'R')) THEN WCODE:=WBADNAME
			   ELSE PDP_NAME:=FILE_NAME||TMODIFIER[2]
			 END;
		     3:  BEGIN
			  IF ((TMODIFIER[2]='F') OR (TMODIFIER[2]='G'))
			   AND (TMODIFIER[3]='R') THEN 
			   PDP_NAME:=FILE_NAME||SUBSTR(TMODIFIER,2)
			   ELSE WCODE:=WBADNAME
			  END;
		     OTHERS:  WCODE:=WBADNAME
		   END;
		   IF WCODE<>WBADNAME THEN
		     BEGIN
		       PDP_NAME:=PDP_NAME||'.940[50127,'||
				WUSER(WNUMBER,DNUMBER)||']<007>';
		       IF SEARCH(TMODIFIER,['R'])=0 THEN DRCONVERT:= TRUE;
		     END;
		 END;
	OTHERS: WCODE:=WBADNAME
      END
    END;
    END;
  END
END.						(* wfileconvert *)
    