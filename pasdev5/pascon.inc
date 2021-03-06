
      header = 'PASCAL-10, version 6.03 of ';
      rversion := 6.03;

      displimit = 20; maxlevel = 8;
      strglgth = 120; bitmax = 36;
      sizeoffileblock=21 ;
      maxsize = 26;				(* MAX SIZE OF CTP, STP, CSP RECORDS *)
      maxctpsize = 9;
      diskblock = 128;				(* WORDS PER DISK BLOCK *)
      offset=40b;				(*FUER SETVERARBEITUNG DER ASCIICHARACTER*)
      chcntmax = 132;				(*MAXIMUM OF CHARACTERS IN ONE LINE*)
      maxwidth = 132;				(*MAXIMUM LINE WIDTH*)
      minwidth = 64;
      minlength = 10;
      minconditional = 0;
      maxconditional = 99;
      maxtitlelen = 80;
      left = 2;right = 1;both = 3;no = 0; min8 = 400000000000b;

	maximumint = 377777777777b;
	minimumint = 400000000000b;

      (*KONSTANTEN VON BODY: *)
      (***********************)

      cixmax = 3000;	       hwcstmax = 377777b;
      stackandfiles = 3000b;    maxerr = 4;		  basemax = 71;
      maxfils = 10;				(*MAXIMUM NUMBER OF SOURCE FILES*)
      maxinclusion = 2;				(* MAXIMUM NESTING OF INCLUDE FILES*)
      pfmaxkey = 69;				(* MAXIMUM KEY NO. FOR STD. PROC/FUN'S *)

      (*ADDRESSES:
       ***********)

      hac=0;					(*HILFSREGISTER*)
      tac=1;					(*HILFSREGISTER AUCH FUER BYTEPOINTER*)
      regin=1;					(*INITIALISE REGC*)
      reginp1=2;				(*REGIN+1*)
      reginp2=3;				(*REGIN+2*)
      reginp3=4;				(*REGIN+3*)
      reginp4=5;				(*REGIN+4*)
      reginp5=6;				(*REGIN+5*)
      parregcmax=6;				(*HIGHEST REGISTER USED FOR PARAMETERS*)
      paroverfreg=7;				(*CONTAINS ADDRESS OF REST IF CAN'T FIT ALL IN REG'S*)
      within=12;				(*FIRST REGISTER FOR WITHSTACK*)
      newreg=13;				(*LAST PLACE OF NEW-STACK*)
      basis=14;					(*BASIS ADDRESS STACK*)
      topp=15;					(*FIRST FREE PLACE IN DATASTACK*)
      retloc=4;					(*OFFSET FROM TOPP FOR FUNCTION VALUE*)
      retlocp1=5;
      retlocp2=6;
      progrst = 1b;				(*START STATIC AREA AT 141B*)
      highstart=400000b;
      maxaddr=777777b;

      min_allocation := 0;
      max_allocation := 99;

   