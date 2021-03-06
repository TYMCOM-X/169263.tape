
      (*RETURNED BY SOURCE PROGRAM SCANNER INSYMBOL:*)
      (**********************************************)

      sy: symbol;				(*LAST SYMBOL*)
      op: operator;				(*CLASSIFICATION OF LAST SYMBOL*)
      val: valu;				(*VALUE OF LAST CONSTANT*)
      lgth: integer;				(*LENGTH OF LAST STRING CONSTANT*)
      id: alfa;					(*LAST IDENTIFIER (POSSIBLY TRUNCATED)*)
      ch: char;					(*LAST CHARACTER*)
      eol, eofn: boolean;			(* END OF LINE, END OF FILE FLAGS *)

      (*COUNTERS:*)
      (***********)

      rtime,
      codeend,					(*FIRST LOCATION NOT USED FOR INSTRUCTIONS*)
      lc,ic: addrrange;				(*DATA LOCATION AND INSTRUCTION COUNTER*)
      statlc: addrrange;
      lcpar: addrrange;				(* LOCAL STORAGE ORIGIN, PARMS OMITTED *)
      testpacked: boolean;			(* INDICATES WHETHER PACKED VALS IN LOCAL *)

      (*SWITCHES:*)
      (***********)

      incompilation,				(*FALSE IF READING FILENAMES, TRUE IF COMPILING SOURCE *)
      kl10sw,					(*IF CAN USE SPECIAL KL10 INSTRUCTIONS*)
      dmove,					(*CAN USE DMOVE FOR KI10 AND KL10*)
	DBLREAL,			(* IF TRUE, PERFORM REAL ARITH. IN DOUBLE PRECISION  *)
      shorterrors,				(*IF TRUE EMIT ERROR NUMBER ONLY*)
      listinfo,					(*PRINT VERSION DATE, TIME, ETC. TO TTY*)
      relout,					(*PRODUCE REL FILE*)
      listsource,				(*PRODUCE SOURCE LISTING*)
      glistsource,				(*GLOBAL, NO LISTING AT ALL IF SPEC'D FROM TTY*)
      overlaysw,				(*SPECIFIES OVERLAY*)
      progresssw,				(*PROC NAME TO TTY AS COMPILED*)
      dp,					(*DECLARATION PART*)
      swflag,					(*TO IGNORE SWITCHES WHICH MUST NOT BE RESET*)
      cortmp,		(* true if reading tmpcor file *)
      prterr,					(*TO ALLOW FORWARD REFERENCES IN POINTER TYPE
						 DECLARATION BY SUPPRESSING ERROR MESSAGE*)
      specialsw,				(*PERMITS NONSTANDARD FEATURES*)
      main,					(*IF FALSE COMPILER PRODUCES EXTERNAL PROCEDURE OR FUNCTION*)
      ttyinuse,					(*IF TRUE TTY-CHANNELS SHALL BE OPENED*)
      outinuse,					(*IF TRUE, OUTPUT SHALL BE OPENED *)
      ininuse,					(*SAME, FOR INPUT *)
      debug,					(*ENABLE DEBUGGING*)
      xref,					(* PROC/FUNC XREF IF TRUE *)
      xref_open,		(* xref file open if true *)
      tracer,					(*LIMITED DEBUGGING AIDS*)
      listcode,					(*LIST MACRO CODE*)
      debugcomments,				(*INCLUDE DEBUGCOMMENTS*)
(* $X5      INITGLOBALS, *)		      (*INITIALIZE GLOBAL VARIABLES*)
      loadnoptr,				(*TRUE IF NO POINTERVARIABLE SHALL BE LOADED*)
      runtmcheck: boolean;			(*SWITCH FOR RUNTIME-TESTS*)
      compsw: packed array[minconditional..maxconditional] of boolean;
      corsiz:  addrrange;			(* SPACE FOR STACK AND BUFFERS *)
      virtual: boolean;		(* use virtual heap ? *)
      incore: boolean;		(* virtual heap with incore areas *)
      mask_offset: addrrange;	(* backlink refs to runtime const *)
      area_mask: addrrange;
      ovfl_mask: addrrange;
      v_nil: addrrange;
      shl_area, shr_area: addrrange;
      allocation: min_allocation..max_allocation;
      alc_used: boolean;
      pagelength,				(* SET BY $LENGTH DIRECTIVE *)
      linesthispage,
      linewidth,				(* SET BY $WIDTH *)
      titlelength: integer;			(* LENGTH OF $TITLE STRING *)
      title: packed array[1..maxtitlelen] of char;
      optline, optlen: 0..chcntmax;		(* SET BY OPTIONLIST FOR ECHOING OPTIONS IN LISTING*)
      lensubtitle : integer;			(* LENGTH OF SUBTITLE *)
      subtitle : packed array[1..maxtitlelen] of char;
      stmt_indicator: char;			(* '*' IF STMT BLOCK EMITTED FOR
						   THIS LINE, ELSE BLANK *)

      (*POINTERS:*)
      (***********)

      localpfptr: ctp;				(*PTRS TO LOCAL PROC/FUNC-CHAIN*)
      fwptr: ctp;				(*HEAD OF CHAIN OF FORW DECL TYPE IDS*)
      slastbtp: btp;				(*HEAD OF BYTEPOINTERTABLE*)
      firstkonst: ksp;
(* $X5      SFGLOBPTR,CGLOBPTR : GTP ; *)       (*POINTER TO FIRST AND CURRENT GLOBAL INITIALISATION RECORD*)
      globtestp : testp ;			(*POINTER TO LAST PAIR OF POINTERTYPES*)

      (*BOOKKEEPING OF DECLARATION LEVELS:*)
      (************************************)

      level: levrange;				(*CURRENT STATIC LEVEL*)
      disx,					(*LEVEL OF LAST ID SEARCHED BY SEARCHID*)
      top: disprange;				(*TOP OF DISPLAY*)

      LEVEL0: CTP;			(* HEAD OF TREE OF EXTERNAL FORTRAN ROUTINES  *)

      display:					(*WHERE:   MEANS:*)
      array[disprange] of
      packed record
	       (*=BLCK:	 ID IS VARIABLE ID*)
	       flabel: labp;			(* FIRST LABEL DEFINED AT THIS LEVEL *)
	       fname: ctp;			(*=CREC:   ID IS FIELD ID IN RECORD WITH*)
	       case occur: where of		(*	      CONSTANT ADDRESS*)
		    crec: (clev: levrange;	(*=VREC:   ID IS FIELD ID IN RECORD WITH*)
			   cindr: acrange;	(*	      VARIABLE ADDRESS*)
			   cindb: ibrange;
			   crelbyte: relbyte;
			   cdspl,
			   clc	: addrrange);
		    blck: (blkid: ctp)		(* ID RECORD FOR BLOCK NAME *)
	     end;

      (*EXPRESSION COMPILATION:*)
      (*************************)

      gattr: attr;				(*DESCRIBES THE EXPR CURRENTLY COMPILED*)

      (*VARIABLES FROM BODY*)
      (*********************)

      relblock: packed record
			 case boolean of
			      true: (component: array[1..20] of integer);
			      false: (item: addrrange; count: addrrange;
				      relocator: relword;
				      code: array[0..17] of integer)
		       end;

      code: packed record
		     relocation:  packed array[coderange] of relbyte;
		     information: packed array[coderange] of char;
		     case integer of
			  1: (instruction: packed array[coderange] of pdp10instr);
			  2: (word:	   packed array[coderange] of integer);
			  3: (halfword:    packed array[coderange] of halfs)
		   end;

      regc,					(*TOP OF REGISTERSTACK*)
      regcmax: acrange;				(*MAXIMUM OF REGISTERS FOR EXPRESSION STACK*)
      cix,
      patchsize,				(*TOO INSERT LCMAX IN ENTRYCODE*)
      pfstart: integer;				(*START OF NORMAL ENTRYCODE OF EACH FUNC. OR PROC.*)
      progchain,				(*FWD REFERENCES TO "PROGRAM" BLOCK*)
      pfblock: integer;				(*PROCEDURE BLOCK ADDRESS FOR DEBUG*)
      curproc: ctp;				(* ID RECORD FOR PROC/FUNC BEGIN COMPILED *)
      lcmax: addrrange; 
      cbpssaddr: addrrange;
      smbfile,					(* TEMPFILE FOR DEBUG SYMBOL TABLE *)
      relfile: file of integer;			(*RELOCATABLE BINARY OUTPUT*)
      symfile: text;				(* XREF FILE *)
      deb_words, wordswritten, relwords: integer;   (* COUNT OUTPUT TO RELFILE, SYMBOL FILE *)
      pflist: ctp;
      withix,					(*TOP OF WITH-REG STACK*)
      highestcode,				(*MAXIMUM OF HIGH SEGMENTS ADDRESS*)
      mainstart,				(*FIRST CODE OF BODY OF MAIN*)
      startaddr: integer;			(*STARTADDRESSE*)

      entry, filename, symbolnam: alfa;
      day: packed array[1..9] of char;
      entrydone: boolean;

      libix: integer;
      liborder: packed array[1..4] of symbol;

  ttyi: text;
  pasmsg: text;
  relnam, listnam: filnamestr;
  filemax,filenum: integer;
  srcnam: array [1..maxfils] of filnamestr;
  breakch:  char;
  realtty: boolean;

      (*------------------------------------------------------------------------------*)

  