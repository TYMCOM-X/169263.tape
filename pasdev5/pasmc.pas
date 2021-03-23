$title pasmc - PASCAL compiler object module routines
$OPTIONS special, nocheck

(* Conditional compilation switches for this module:

	X0:  If enabled, LIST options is honored
	Y2:  Unless enabled, full DEBUG support is included 
	X5:  If enabled, support initprocedures
							*)

$PAGE includes
$INCLUDE PASDCL.INC

$INCLUDE PASFB.INC

$INCLUDE PASLEX.INC

$INCLUDE PASCMP.INC
$INCLUDE PASHAK.INC

$PAGE declarations
(*$x
type
  mne_array = array[1..45] of packed array[1..60] of char;

const mnemonics : mne_array := (
 '***001***002***003***004***005***006***007***010***011***012',
 '***013***014***015***016***017***020***021***022***023***024',
 '***025***026***027***030***031***032***033***034***035***036',
 '***037CALL  INIT  ***042***043***044***045***046CALLI OPEN  ',
 'TTCALL***052***053***054RENAMEIN    OUT   SETSTSSTATO STATUS',
 'STATZ INBUF OUTBUFINPUT OUTPUTCLOSE RELEASMTAPE UGETF USETI ',
 'USETO LOOKUPENTER UJEN  ***101***102***103***104ADJSP ***106',
 '***107DFAD  DFSB  DFMP  DFDV  DADD  DSUB  DMUL  DDIV  DMOVE ',
 'DMOVN FIX   ***123DMOVEMDMOVNMFIXR  FLTR  UFA   DFN   FSC   ',
 'IBP   ILDB  LDB   IDPB  DPB   FAD   FADL  FADM  FADB  FADR  ',
 'FADRI FADRM FADRB FSB   FSBL  FSBM  FSBB  FSBR  FSBRI FSBRM ',
 'FSBRB FMP   FMPL  FMPM  FMPB  FMPR  FMPRI FMPRM FMPRB FDV   ',
 'FDVL  FDVM  FDVB  FDVR  FDVRI FDVRM FDVRB MOVE  MOVEI MOVEM ',
 'MOVES MOVS  MOVSI MOVSM MOVSS MOVN  MOVNI MOVNM MOVNS MOVM  ',
 'MOVMI MOVMM MOVMS IMUL  IMULI IMULM IMULB MUL   MULI  MULM  ',
 'MULB  IDIV  IDIVI IDIVM IDIVB DIV   DIVI  DIVM  DIVB  ASH   ',
 'ROT   LSH   JFFO  ASHC  ROTC  LSHC  ***247EXCH  BLT   AOBJP ',
 'AOBJN JRST  JFCL  XCT   ***257PUSHJ PUSH  POP   POPJ  JSR   ',
 'JSP   JSA   JRA   ADD   ADDI  ADDM  ADDB  SUB   SUBI  SUBM  ',
 'SUBB  CAI   CAIL  CAIE  CAILE CAIA  CAIGE CAIN  CAIG  CAM   ',
 'CAML  CAME  CAMLE CAMA  CAMGE CAMN  CAMG  JUMP  JUMPL JUMPE ',
 'JUMPLEJUMPA JUMPGEJUMPN JUMPG SKIP  SKIPL SKIPE SKIPLESKIPA ',
 'SKIPGESKIPN SKIPG AOJ   AOJL  AOJE  AOJLE AOJA  AOJGE AOJN  ',
 'AOJG  AOS   AOSL  AOSE  AOSLE AOSA  AOSGE AOSN  AOSG  SOJ   ',
 'SOJL  SOJE  SOJLE SOJA  SOJGE SOJN  SOJG  SOS   SOSL  SOSE  ',
 'SOSLE SOSA  SOSGE SOSN  SOSG  SETZ  SETZI SETZM SETZB AND   ',
 'ANDI  ANDM  ANDB  ANDCA ANDCAIANDCAMANDCABSETM  SETMI SETMM ',
 'SETMB ANDCM ANDCMIANDCMMANDCMBSETA  SETAI SETAM SETAB XOR   ',
 'XORI  XORM  XORB  IOR   IORI  IORM  IORB  ANDCB ANDCBIANDCBM',
 'ANDCBBEQV   EQVI  EQVM  EQVB  SETCA SETCAISETCAMSETCABORCA  ',
 'ORCAI ORCAM ORCAB SETCM SETCMISETCMMSETCMBORCM  ORCMI ORCMM ',
 'ORCMB ORCB  ORCBI ORCBM ORCBB SETO  SETOI SETOM SETOB HLL   ',
 'HLLI  HLLM  HLLS  HRL   HRLI  HRLM  HRLS  HLLZ  HLLZI HLLZM ',
 'HLLZS HRLZ  HRLZI HRLZM HRLZS HLLO  HLLOI HLLOM HLLOS HRLO  ',
 'HRLOI HRLOM HRLOS HLLE  HLLEI HLLEM HLLES HRLE  HRLEI HRLEM ',
 'HRLES HRR   HRRI  HRRM  HRRS  HLR   HLRI  HLRM  HLRS  HRRZ  ',
 'HRRZI HRRZM HRRZS HLRZ  HLRZI HLRZM HLRZS HRRO  HRROI HRROM ',
 'HRROS HLRO  HLROI HLROM HLROS HRRE  HRREI HRREM HRRES HLRE  ',
 'HLREI HLREM HLRES TRN   TLN   TRNE  TLNE  TRNA  TLNA  TRNN  ',
 'TLNN  TDN   TSN   TDNE  TSNE  TDNA  TSNA  TDNN  TSNN  TRZ   ',
 'TLZ   TRZE  TLZE  TRZA  TLZA  TRZN  TLZN  TDZ   TSZ   TDZE  ',
 'TSZE  TDZA  TSZA  TDZN  TSZN  TRC   TLC   TRCE  TLZE  TRCA  ',
 'TLCA  TRCN  TLCN  TDC   TSC   TDCE  TSCE  TDCA  TSCA  TDCN  ',
 'TSCN  TRO   TLO   TROE  TLOE  TROA  TLOA  TRON  TLON  TDO   ',
 'TSO   TDOE  TSOE  TDOA  TSOA  TDON  TSON  ***700            ');
	 *)

$PAGE align
public procedure align (minsize: integer);

(* assures that a block of "minsize" words is available within a
   disk block of debug symbol table records *)

begin
  if (deb_words mod diskblock) + minsize > diskblock then
    deb_words := ((deb_words + diskblock - 1) div diskblock) * diskblock;
end;

type
    writeform = (writeentry, writename, writehiseg, writeglobals, writecode,
      writeinternals, writelibrary, writefileblocks,writesymbols,writestart,
	writeend, writeconstants, writeprogblock);

static var
    progblockaddr: addrrange;			(* highseg location of "program block" *)
    rx50name: addrrange;			(* code to identify compilations to debugger *)

$PAGE writemc
public procedure writemc(writeflag:writeform);

type
    wandelform=(konstante,pdp10code,realcst,strcst,sixbitcst,halfwd,pdp10bp,
      radix) ;
    recordform=(none,constntrec,structurerec,identifrec,debugrec);
    bigalfa = packed array[1..15] of char ;

const
    request := 14b;

var
    i,j,l : integer;
    checker: ctp;
    (*$X LLISTCODE, SAVELISTCODE: BOOLEAN; *)
    lic : addrrange;
    lfirstkonst: ksp;
    lrelbyte: relbyte;
    lfileptr: ftp;
    switchflag: flagrange;
    filblockadr : addrrange ;
    codearray: boolean;
    licmod4: addrrange;
    lsize: 1..maxsize;
    firstpass: boolean;
    relarray, relempty: array[1..maxsize] of relbyte;
    wandlung : packed record
      case wandelform of
	konstante:(
	  wkonst :integer);
	pdp10code:(
	  winstr :pdp10instr);
	realcst :(
	  wreal: real);
	strcst :(
	  wstring:charword);
	sixbitcst:(
	  wsixbit:packed array[1..6] of 0..77b);
	halfwd :(
	  wlefthalf:addrrange ;
	  wrighthalf : addrrange);
	pdp10bp :(
	  wbyte: bpointer);
	radix :(
	  flag: flagrange;
	  symbol: radixrange)
    end;

type
    d_constant = record				(* for reals, sets, and strings only *)
      case d_cclass: cstclass of
	int : (
	  d_intval : integer;
	  d_intval1: integer);			(* to access second word of a set *)
	reel: (
	  d_rval: real);
	pset: (
	  d_pval: set of 0..71);
	strd,					(* debugger can treat strd and strg as synonymous *)
	strg: (
	  d_slgth: 0..strglgth;			(* legnth of string following *)
	  d_sval: stringarray)			(* actual string *)
    end;
    d_structure = packed record
      d_size: addrrange;			(* max # words required by type *)
      d_typeid: ctp;				(* if declared type, pointer to type id *)
      d_bitsize: bitrange;			(* minimum bits required *)
      case d_form: structform of
	scalar: (
	  case d_scalkind: declkind of
	    declared: (
	      d_fconst: ctp);			(* to last scalar constant *)
	    standard: (
	      d_vartype: typekind));		(* only for INTEGER, REAL and CHAR *)
	subrange: (
	  d_rangetype: stp;			(* range of what? *)
	  d_min,				(* min.ival is lowerbound of subrange *)
	  d_max: valu);				(* max.ival is upperbound of subrange *)
	pointer: (
	  d_eltype: stp;			(* points to what type? *)
	  d_voffset: boolean);			(* offset if true *)
	power: (
	  d_elset: stp);			(* type of set elements *)
	arrays: (
	  d_arraypf: boolean;			(* true if packed *)
	  d_aeltype: stp;			(* type of array elements *)
	  d_inxtype: stp);			(* type of array index *)
	strings: (
	  d_skind: stringkind;			(* varying or not *)
	  d_maxlength: integer);
	records: (
	  d_recordpf: boolean;			(* true if packed *)
	  d_fieldfg: boolean;			(* true if fields precede variants *)
	  d_fstfld: ctp;			(* to identifier record for first
						   field--rest chained through "next" *)
	  d_recvar: stp);			(* to last variant *)
	files: (
	  d_filepf: boolean;			(* true if packed *)
	  d_filetype: stp);			(* file of what *)
	tagfwithid, tagfwithoutid: (
	  d_fstvar: stp;			(* to last variant *)
	  case boolean of
	    true: (
	      d_tagfieldp: ctp);		(* identifier record for tagfield *)
	    false: (
	      d_tagfldtype: stp));		(* type of tagfield if undiscriminated union *)
	variant: (
	  d_nxtvar,				(* link to previous variant *)
	  d_subvar: stp;			(* to internal tagfield and variants *)
	  d_firstfield: ctp;			(* first field within variant *)
	  d_varval: valu);			(* variant value *)
	formalprocfunc: (
	  d_proctyptr: ctp)			(* type info formal procs/funcs
						   stored in CTP record *)
    end;
    d_identifier = packed record
      d_name: alfa;
      d_llink,					(* usual left subtree *)
      d_rlink: ctp;				(* right subtree *)
      d_idtype: stp;				(* link to type info *)
      d_next: ctp;				(* links parameters, fields, and scalars *)
      case d_klass: idclass of
      (* no variant for "types" - info store in idtype^ *)
	konst: (
	  d_values: valu);			(* value or pointer to constant record *)
	vars: (
	  d_vkind: idkind;			(* actual or formal *)
	  d_vclass: storageclass;		(* determines usage of vaddr *)
	  d_vconst: boolean;			(* true if structured constant *)
	  d_vlevel: levrange;			(* declaration level *)
	  d_vaddr: addrrange);			(* address *)
	field: (
	  d_packf: packkind;			(* packing clue *)
	  d_fldaddr: addrrange;			(* offset within record if not packk *)
	  d_fldbp: bpointer);			(* if packk, byte pointer for ldb/dpb access *)
	proc, func: (
	  d_language: symbol;
	  d_pflevel: levrange;			(* level of declaration *)
	  d_pfaddr: addrrange;			(* address *)
	  d_parmsize: addrrange;		(* parameter overflow data *)
	  case d_pfkind: idkind of
	    actual: (
	      d_pfclass: storageclass;
	      d_pfchain: ctp;
	      d_pflower: ctp;
	      d_externalname: alfa);
	    formal: (
	      d_parameters: ctp))
    end;

var
    deb_record: record
      case integer of
	1: (
	  deb_array: array[1..maxsize] of integer);
	2: (
	  d_cst: d_constant);
	3: (
	  d_id : d_identifier);
	4: (
	  d_str: d_structure)
    end;

    (*$X procedure checklisting;
	 begin
	    if (pagelength # 0) andif (linesthispage >= pagelength) then begin
	      page; linesthispage := 1;
	    end
	    else linesthispage := linesthispage + 1;
	 end;

	PROCEDURE NEUEZEILE;
	 BEGIN
	   IF LISTCODE AND (LIC > 0)
	   THEN
		 BEGIN
		  WRITELN ;
		   checklisting;
		   IF RELBLOCK.ITEM = 1
		   THEN
		     BEGIN
		      WRITE(LIC:6:O,' ');
		     END
		   ELSE WRITE(' ':7)
		 END
	 END (*NEUEZEILE*) ;  *)

$PAGE putrelcode, writeblockst - in writemc
  procedure putrelcode;

  var
      i: integer;

  begin
    with relblock do
      if ((count > 1) orif (item <> 1)) andif (count > 0) then begin
	for i:= count+1 to 18 do
	  relocator[i-1] := no;
	relwords := relwords + count + 2;
	for i:= 1 to count+2 do begin
	  relfile^:= component[i];
	  put(relfile)
	end;
	count:= 0
      end
  end;

  (*$X PROCEDURE SHOWRELOCATION(FSIDE: RELBYTE; FRELBYTE: RELBYTE);
   BEGIN
     IF (FRELBYTE = FSIDE) ORIF (FRELBYTE = BOTH)
     THEN WRITE('''')
     ELSE WRITE(' ')
   END;  *)

  procedure writeblockst( fitem: addrrange);

  var
      wandlung: packed record
	case boolean of
	  true: (
	    wkonst: integer);
	  false: (
	    wlefthalf: addrrange;
	    wrighthalf: addrrange)
      end;

  begin
    with relblock , wandlung do begin
      if count <> 0 then
	putrelcode;
      item:= fitem;
      if item = 1 then begin
	wlefthalf:= 0;
	wrighthalf:= lic;
	code[0]:= wkonst;
	if wrighthalf < progrst then
	  relocator[0] := no
	else
	  relocator[0] := right;
	count:= 1
      end
      else if item = 10b then count := 0;
    end
  end;

$PAGE writeword - in writemc
  procedure writeword(frelbyte: relbyte; fword: integer);

  var
      wandlung: packed record
	case boolean of
	  true: (
	    wkonst: integer);
	  false: (
	    wlefthalf: addrrange;
	    wrighthalf: addrrange)
      end;

  begin
    with wandlung do begin
      wkonst := fword;
      with relblock do begin
	if count = 0 then
	  writeblockst(item);
	code[count]:= fword;
	if frelbyte in [left,both] then
	  if (wlefthalf < progrst) or (wlefthalf = 377777b) then
	    frelbyte := frelbyte - left;
	if frelbyte in [right,both] then
	(* STATEMENT PREVIOUSLY ASSUMED RH < PROGRST SHOULD
	   BE ABSOLUTE. HOWEVER, SCRIPTED ACCESSES MAY GENERATE
	   THESE, THUS, THE CHECK WAS REMOVED...
	   *** A. KORTESOJA 11/15/76***)
	  if (wrighthalf = 377777b) then
	    frelbyte := frelbyte - right;
	relocator[count]:= frelbyte;
	lrelbyte := frelbyte;
	count := count+1;
	if count = 18 then
	  putrelcode
      end;
      (*$X IF LLISTCODE
      THEN
	BEGIN
	 NEUEZEILE;
	  IF LIC > 0
	  THEN WRITE(' ':13);
	  IF (WRITEFLAG > WRITEFILEBLOCKS) ANDIF (WRITEFLAG <> WRITECONSTANTS)
	  THEN WRITE(' ':7)
	  ELSE
	    BEGIN
	     WRITE(WLEFTHALF:6:O) ; SHOWRELOCATION(LEFT,FRELBYTE)
	    END;
	 WRITE(WRIGHTHALF:6:O); SHOWRELOCATION(RIGHT,FRELBYTE);  WRITE(' ':3)
	END;  *)
      if not codearray then
	lic := lic + 1
    end
  end;

$PAGE radix50, writepair, writeidentifier, reference - in writemc
  function radix50( fname: alfa): radixrange;

  var
      i: integer;
      octalcode, radixvalue: radixrange;

  begin
    radixvalue:= 0;
    i:=1;
    while (i <= 6) andif (fname[i] <> ' ') do begin
      if fname[i] in digits then
	octalcode:= ord(fname[i])-ord('0')+1
      else if fname[i] in alphabetic then
	octalcode:= ord(fname[i])-ord('A')+11
      else
	case fname[i] of
	  '.':
	    octalcode:= 37;
	  '$':
	    octalcode:= 38;
	  others:
	    octalcode:= 39
	end;
      radixvalue:= radixvalue*50b;
      radixvalue:= radixvalue+octalcode;
      i:=i+1
    end;
    radix50:= radixvalue
  end;

  procedure writepair( frelbyte: relbyte; faddr1, faddr2: addrrange);

  begin
    with wandlung do begin
      wlefthalf:= faddr1;
      wrighthalf:= faddr2;
      writeword(frelbyte,wkonst)
    end
  end;

  procedure writeidentifier( fflag: flagrange; fsymbol: alfa);

  begin
  (*$X LLISTCODE := FALSE;  *)
    with wandlung do begin
    (*$X IF LISTCODE ANDIF (WRITEFLAG > WRITEHISEG)
    THEN
      BEGIN
	IF  (LIC > 0)
	THEN
	  BEGIN
	   WRITELN;
	   checklisting;
	   WRITE(' ':7)
	  END;
	IF LIC > 0
	THEN WRITE(' ':13); WRITE(FSYMBOL:6,' ':11)
      END;  *)
      if fflag <> 6b then begin
	flag:= fflag;
	symbol:= radix50(fsymbol)
      end;
      writeword(no,wkonst)			(*$X ; LLISTCODE := LISTCODE *)
    end
  end;

  procedure reference (flag: flagrange; symbol: alfa; reloc: relbyte; addr: addrrange);
  begin
    writeidentifier (flag, symbol);
    writepair (reloc, 0, addr);
  end;

  (*$X PROCEDURE WRITEFIRSTLINE ;
   BEGIN
     IF LISTCODE
     THEN
	   BEGIN
	    WRITELN;
	    checklisting;
	   END
   END ;  *)

  (*$X PROCEDURE WRITEHEADER(FTEXT: BIGALFA);
		       BEGIN
			 IF LISTCODE
			 THEN
			       BEGIN
				WRITELN;
				checklisting;
				WRITE(FTEXT:15,':',' ':4)
			       END
		       END;  *)

$PAGE mcfileblocks - in writemc
  procedure mcfileblocks;

  var
      stopptr: ftp;

  begin
  (*MCFILEBLOCKS*)
    lfileptr:= fileptr;
    if main then
      stopptr := nil
    else
      stopptr := sfileptr;
    while lfileptr <> stopptr do
      with lfileptr^, fileident^, wandlung do begin
	filblockadr := vaddr ;
	lic := filblockadr ;			(*$X WRITEFIRSTLINE ;  *)
	writeblockst(1);
	writeword(right,filblockadr + filcmp) ;
	writeword(no,0) ;
	writeword(no,0) ;			(*RESERVE LOCATIONS FOR FILEOF AND FILEOL*)
	wkonst := 0;
	winstr.instr := 50b (*OPEN*);
	winstr.ac := 0 ;
	winstr.address := filblockadr + filsta ;
	writeword(right,wkonst) (*FILOPN*);
	winstr.instr := 76b (*LOOKUP*);
	winstr.address := filblockadr + filnam ;
	writeword(right,wkonst) ;
	winstr.instr := 77b (*ENTER*);
	writeword(right,wkonst) ;
	winstr.address := 0 ;
	winstr.instr := 56b (* IN*);
	writeword(no,wkonst) ;
	winstr.instr := 57b (*OUT*);
	writeword(no,wkonst) ;
	winstr.instr := 71b (*RELEASE*);
	writeword(no,wkonst) ;
	if comptypes(idtype,textptr) then
	  writeword(no, 0 (*DATAMODE=ASCII*))
	else
	  writeword(no, 14b (*  "   =BINARY*)) ;
	if (name = 'TTY       ') or (name = 'TTYOUTPUT ') then
	  wlefthalf := 646471b			(*SIXBIT /TTY/ *)
	else
	  wlefthalf := 446353b;			(*SIXBIT /DSK/ *)
	wrighthalf := 0;
	writeword(no,wkonst);
	writeword(no,0) ;			(*BUFFERHEADER ADDRESS INSERTED DURING RESET OR REWRITE*)
	for i := 1 to 6 do
	  wsixbit[i] := ord( name[i] ) - 40b ;
	writeword(no,wkonst) ;
	wkonst := 0 ;
	for i := 1 to 3 do
	  wsixbit[i] := ord( name[i+6] ) - 40b ;
	writeword(no,wkonst) ;
	for i := 1 to 6 do
	  writeword(no, 0 )			(*ZERO IN FILPROT, FILPPN, FILBFH, FILBTP, FILBTC,FILLNR*)
	  ;
	wlefthalf := - idtype^.size ;
	wrighthalf := filblockadr + filcmp ;
	writeword(right,wkonst) (*FILCNT*);
	for i := 1 to idtype^.size do
	  writeword(no, 0 ) (*CLEAR COMPONENT LOCATIONS *);
	lfileptr := nextftp ;
      end
  end (*MCFILEBLOCKS*);

$PAGE mcconstants - in writemc
  procedure mcconstants;

  var
      i: integer;

  begin
    lic := codeend - cix - 1;
    codearray := false;
    writeblockst (1);
    with code do
      for i := 0 to cix do begin
	writeword (relocation[i],word[i]);
      end;
    cix := -1;
  end;

(*$X5 
  PROCEDURE MCGLOBALS;

  BEGIN
  (*MCGLOBALS*)
  (*$X IF LISTCODE ANDIF (FGLOBPTR # NIL)
			 THEN WRITEBUFFER;  *)
    WHILE FGLOBPTR <> NIL DO
      WITH FGLOBPTR^ DO BEGIN
	LIC := FIRSTGLOB ; (*$X WRITEFIRSTLINE ;  *)
	J := FCIX ;
	WRITEBLOCKST(1);
	FOR I := FIRSTGLOB TO LASTGLOB DO BEGIN
	  WANDLUNG.WINSTR := CODE.INSTRUCTION[J] ;
	  J := J + 1 ;
	  WRITEWORD(NO,WANDLUNG.WKONST) ;
	END ;
	FGLOBPTR := NEXTGLOBPTR
      END;
    CIX := -1;
  END (*MCGLOBALS*);	*)

  (*$Y2
    procedure put_record (size: integer; loc: integer);

    var
	word: packed record
	  case boolean of
	    true: (
	      int: integer);
	    false:(
	      left: addrrange;
	      right: addrrange)
	end;
	i: integer;

    begin
      if loc > wordswritten then
	for i := wordswritten to loc - 1 do begin
	  smbfile^ := 0;
	  put (smbfile);
	end;
      with deb_record do
	for i := 1 to size do begin
	  word.int := deb_array[i];
	  deb_array[i] := 0;
	  smbfile^ := word.int;
	  put (smbfile);
	  (*$x if listcode then begin
		 checklisting;
		 writeln (loc+i-1:6:o,' -> ',word.left:6:o,' ',word.right:6:o);
	       end; *)
	end;
      wordswritten := loc+size;
    end;

$PAGE csp_size, stp_size, ctp_size - in writemc
function csp_size (cst: csp): integer;

begin
  with cst^ do
    case cclass of
      int, pset:
	csp_size := 3;
      reel:
	csp_size := 2;
      strd, strg:
	csp_size := 2 + (slgth + 4) div 5
    end;
end;

function stp_size (struct: stp): integer;

begin
  with struct^ do
    case form of
      scalar:
	stp_size :=      subrange, variant:
	stp_size := 4;
      arrays, strings, tagfwithid, tagfwithoutid, records:
	stp_size := 3;
      others:
	stp_size := 2
    end
end;

function ctp_size (id: ctp): integer;

begin
  with id^ do
    case klass of
      types:
	ctp_size := 5;
      proc, func:
	if pfkind = actual then
	  ctp_size := 9
	else
	  ctp_size := 8;
      others:
	ctp_size := 6
    end;
end;

$PAGE dref_ctp, dref_stp - in writemc
function dref_ctp (id: ctp): ctp;

begin
  if id = nil then
    dref_ctp := nil
  else
    dref_ctp := id^.selfctp
end;

function dref_stp (str: stp): stp;

begin
  if str = nil then
    dref_stp := nil
  else
    dref_stp := str^.selfstp
end;

$PAGE copycsp - in writemc
procedure copycsp (cst: csp);

var
    cstsize: integer;
    cst_record: record
      case integer of
	1: (
	  c1: integer; (* overlays selfcsp *)
	  c2: integer; (* overlays nocode *)
	  d_cst: d_constant);
	2: (
	  cst: constnt)
    end;

begin
  if cst <> nil then
    with cst^ do begin
      cstsize := csp_size (cst);
      if firstpass then begin
	if selfcsp = nil then begin
	  align (cstsize);
	  selfcsp := ptr (deb_words);
	  deb_words := cstsize + deb_words;
	  nocode := true;
	end;
      end
      else if nocode then begin
	cst_record.cst := cst^;
	deb_record.d_cst := cst_record.d_cst;
	(*$x if listcode then begin
	  checklisting;
	  writeln;
	  checklisting;
	  writeln ('CSP');
	end;    *)
	put_record (cstsize,ord(selfcsp));
	nocode := false;
      end
    end
end;

procedure copystp (struct: stp);

forward;

$PAGE copyctp - in writemc
procedure copyctp (id: ctp);

var
    ctpsize: integer;

begin
  if id <> nil then
    with id^ do
      if (selfctp = nil) and firstpass orif nocode and not firstpass then begin
	ctpsize := ctp_size (id);
	if firstpass then begin
	  align (ctpsize);
	  selfctp := ptr (deb_words);
	  deb_words := ctpsize + deb_words;
	  nocode := true;
	end
	else
	  with deb_record.d_id do begin
	    d_name := name;
	    if ord (id) > ord (head_symbol) then begin
	      d_llink := dref_ctp (llink);
	      d_rlink := dref_ctp (rlink);
	    end
	    else begin
	      d_llink := nil;
	      d_rlink := nil;
	    end;
	    d_next := dref_ctp (next);
	    if idtype <> nil then begin
	      if klass = konst then
		if idtype^.form > pointer then
		  d_values.valp := values.valp^.selfcsp
		else if idtype = realptr then
		  d_values.ival := values.valp^.intval
		else
		  d_values.ival := values.ival;
	    end;
	    d_idtype := dref_stp (idtype);
	    d_klass := klass;
	    case klass of
	      vars: begin
		d_vkind := vkind;
		d_vlevel := vlev;
		d_vconst := vconst;
		d_vaddr := vaddr;
		d_vclass := vclass;
	      end;
	      field: begin
		d_packf := packf;
		d_fldaddr := fldaddr;
		d_fldbp := fldbp;
	      end;
	      proc, func: begin
		d_language := language;
		d_pflevel := pflev;
		d_pfaddr := pfaddr;
		d_parmsize := parmsize;
		d_pfkind := pfkind;
		if pfkind = actual then begin
		  d_pfclass := pfclass;
		  d_pfchain := dref_ctp (pflink);
		  d_pflower :=dref_ctp (pflower);
		  d_externalname := externalname;
		  if pfclass <> externalsc then
		  (* link procedure block to symbol table with internal request *)
		    if pflev > 1 then
		      writepair (left,pfaddr+pflev-maxlevel-2,ord(selfctp))
		    else
		      writepair (left,pfaddr-2,ord(selfctp));
		end
		else
		  d_parameters := dref_ctp (parameters);
	      end
	    end; (* case *)
	    (*$x if listcode then begin
						checklisting;
						writeln;
						checklisting;
						writeln ('CTP: ',name);
					      end; *)
	    put_record (ctpsize,ord(selfctp));
	    nocode := false;
	  end; (* with deb_record.d_id *)
	if ord (id) > ord (head_symbol) then begin
	  copyctp (llink);
	  copyctp (rlink);
	end;
	copystp (idtype);
	copyctp (next);
	if (klass = konst) andif (idtype <> nil) andif (idtype^.form > pointer)
	  then
	    copycsp (values.valp);
	if klass >= proc then
	  if pfkind = formal then
	    copyctp (parameters);
      end;
end;

$PAGE copystp - in writemc
procedure copystp (* struct: stp *);

var
    stpsize: integer;

begin
  if struct <> nil then
    with struct^ do
      if (selfstp = nil) and firstpass orif nocode and not firstpass then begin
	stpsize := stp_size (struct);
	if firstpass then begin
	  nocode := true;
	  align (stpsize);
	  selfstp := ptr (deb_words);
	  deb_words := stpsize + deb_words;
	end
	else if nocode then
	  with deb_record.d_str do begin
	    d_size := size;
	    d_bitsize := bitsize;
	    d_form := form;
	    d_typeid := dref_ctp (typeid);
	    case form of
	      scalar: begin
		d_scalkind := scalkind;
		if scalkind = declared then
		  d_fconst := dref_ctp (fconst)
		else
		  d_vartype := vartype;
	      end;
	      subrange: begin
		d_rangetype := dref_stp (rangetype);
		d_min.ival := min.ival;
		d_max.ival := max.ival;
	      end;
	      pointer: begin
		d_eltype := dref_stp (eltype);
		d_voffset := v_offset;
	      end;
	      power:
		d_elset := dref_stp (elset);
	      arrays: begin
		d_arraypf := arraypf;
		d_aeltype := dref_stp (aeltype);
		d_inxtype := dref_stp (inxtype);
	      end;
	      strings: begin
		d_maxlength := maxlength;
		d_skind := skind;
	      end;
	      records: begin
		d_fstfld := dref_ctp (fstfld);
		d_recvar := dref_stp (recvar);
		d_recordpf := recordpf;
		d_fieldfg := fieldfg;
	      end;
	      files: begin
		d_filetype := dref_stp (filtype);
		d_filepf := filepf;
	      end;
	      tagfwithid, tagfwithoutid: begin
		d_fstvar := dref_stp (fstvar);
		if form = tagfwithid then
		  d_tagfieldp := dref_ctp (tagfieldp)
		else
		  d_tagfldtype := dref_stp (tagfieldtype);
	      end;
	      variant: begin
		d_nxtvar := dref_stp (nxtvar);
		d_subvar := dref_stp (subvar);
		d_firstfield := dref_ctp (firstfield);
		d_varval.ival := varval.ival;
	      end;
	      formalprocfunc:
		d_proctyptr := dref_ctp (proctyptr)
	    end;
	    (*$x if listcode then begin
	      checklisting;
	      writeln;
	      checklisting;
	      writeln ('STP');
	    end; *)
	    put_record (stpsize,ord(selfstp));
	    nocode := false;
	  end;
	copyctp (typeid);
	case form of
	  scalar:
	    if scalkind = declared then
	      copyctp (fconst);
	  subrange:
	    copystp (rangetype);
	  pointer, power:
	    copystp (eltype); (* elset *)
	  arrays: begin
	    copystp (aeltype);
	    copystp (inxtype);
	  end;
	  records: begin
	    copyctp (fstfld);
	    copystp (recvar);
	  end;
	  files:
	    copystp (filtype);
	  tagfwithid, tagfwithoutid: begin
	    copystp (fstvar);
	    if form = tagfwithid then
	      copyctp (tagfieldp)
	    else
	      copystp (tagfieldtype);
	  end;
	  variant: begin
	    copystp (nxtvar);
	    copystp (subvar);
	    copyctp (firstfield);
	  end;
	  formalprocfunc:
	    copyctp (proctyptr)
	end;
      end
end;
   *)

$PAGE mccode - in writemc
  procedure mccode;

  begin
  (*MCCODE*)
    codearray := false;				(*$X LLISTCODE:= FALSE;
			     IF LISTCODE
			     THEN WRITEBUFFER;  *)
    if lastbtp <> nil then
      lic := lastbtp^.fieldcp^.fldaddr;
      (*$X WRITEFIRSTLINE ; *)
    writeblockst(1);
    while lastbtp <> nil do begin
      with lastbtp^,byte do begin
      (*$X IF LISTCODE
      THEN
	BEGIN
	 NEUEZEILE;
	  WRITE(' ':7);
	 WRITE(' POINT  ',SBITS:2,',') ;
	  IF IBIT = 0
	   THEN WRITE('  ')
	  ELSE WRITE(' @') ;
	 WRITE(RELADDR:5:O,'(',IREG:2:O,'),',36-PBITS:2) ;
	END;  *)
	with wandlung do begin
	  wbyte := byte;
	  writeword(no,wkonst)
	end;
	lastbtp := last
      end
    end (* WHILE*);
    lic := codeend - cix - 1 ;
    codearray := true;
    writeblockst(1);				(*$X WRITEFIRSTLINE;  *)
    for i := 0 to cix do
      with code, instruction[i], halfword[i] do begin
	lrelbyte := relocation[i];
	writeword(lrelbyte,word[i]);
	(*$X IF LISTCODE
	THEN
	  BEGIN
	   NEUEZEILE;
	    WRITE(' ':7);
	    IF INFORMATION[I]='W' THEN
		  BEGIN
		   WRITE(' ':6,LEFTHALF :6:O); SHOWRELOCATION(LEFT,LRELBYTE);
		   WRITE(RIGHTHALF:6:O); SHOWRELOCATION(RIGHT,LRELBYTE);
		   WRITE(' ':5)
		  END
		     ELSE BEGIN
		      WRITE(' ',MNEMONICS[(INSTR+9) DIV 10,((INSTR+9) MOD 10)*6+1:6]:6, ' ',AC:2:O,', ');
		       IF INDBIT = 0
		       THEN WRITE(' ')
		       ELSE WRITE('@');
		      WRITE(ADDRESS:6:O); SHOWRELOCATION(RIGHT,LRELBYTE);
		       IF INXREG > 0
		       THEN WRITE('(',INXREG:2:O,')',INFORMATION[I]:1)
		       ELSE WRITE(' ':4,INFORMATION[I]:1)
		     END
	  END;  *)
	lic := lic + 1
      end (*FOR *);
    codearray := false;				(*$X LLISTCODE := LISTCODE;  *)
    if firstkonst <> nil then begin
      lfirstkonst := firstkonst;		(*$X WRITEFIRSTLINE; *)
      writeblockst(1);
      while lfirstkonst <> nil do begin
	with lfirstkonst^.constptr^ do
	  case cclass of
	    int (*$X4 , REEL *):
	      writeword(no,intval) ;
(*$Y4	    REEL: BEGIN
	      WRITEWORD (NO,INTVAL);
	      IF DBLREAL THEN
		WRITEWORD (NO,INTVAL1);
	    END;	*)
	    pset: begin
	    (* THE SET IS PICKED UP
	     AND WRITTEN OUT AS TWO OCTAL NUMBERS *)
	      writeword(no,intval) ;
	      writeword(no,intval1) ;
	    end ;
	    strd, strg:
	      with wandlung do begin
		if lfirstkonst^.addr[0] <> 0 then begin	(* LENGTH REFERENCED *)
		  wkonst := slgth;
		  writeword (no, wkonst)
		end;
		j :=0;
		wkonst := 0;
		for i := 1 to slgth do begin
		  j := j+1;
		  wstring[j] := sval[i];
		  if j=5 then begin
		    j := 0;
		    writeword(no,wkonst);
		    wkonst := 0
		  end
		end;
		if j<>0 then
		  writeword(no,wkonst)
	      end
	  end;
	lfirstkonst := lfirstkonst^.nextkonst
      end					(*WHILE*)
    end;
    (*$y2    if debug and (level = 1) then begin
	    writeblockst (10b);     (* will emit internal requests to fill in
				       symbol table addresses in procedure blocks *)
	    dump_tree (copyctp, display[1].fname, ttyfptr, firstpass);
	    (* essentially:
		for firstpass := true downto false do begin
		  copyctp (display[1].fname);
		  copyctp (ttyfptr);	(* dumps standard id's *)
		end;	*)
	end; *)
    cix := -1;
  end (*MCCODE*);

$PAGE mcvarious - in writemc
  procedure mcvarious;

  var
      inlevel: boolean;
      lab: labp;
      i: 0..3;
      farray: filblock;

  begin
  (*MCVARIOUS*)
    case writeflag of
      writeinternals: begin
	lic := 0;
	(*$X WRITEHEADER('LOCAL REQUESTS '); *)
	inlevel := true;
	writeblockst(8);
	checker := localpfptr;
	while inlevel andif (checker <> nil) do
	  with checker^ do
	    if pflev = level then begin
	      if pfaddr <> 0 then
		for i := 0 to maxlevel do
		  if linkchain[i] <> 0 then
		    writepair(both,linkchain[i],pfaddr-i);
	      checker:= pfchain
	    end
	    else
	      inlevel := false;
	if level > 1 then
	  localpfptr := checker;
	while firstkonst <> nil do
	  with firstkonst^, constptr^ do begin
	    for i := 0 to 2 do
	      if addr[i] <> 0 then
		writepair (both,addr[i],kaddr+i);
	    firstkonst:= nextkonst
	  end;
	lab := display [top].flabel;
	while lab <> nil do
	  with lab^ do begin
	    if not defined then begin
	      error (215);
	      (* CALL ERRORWITHNUM... *)
	    end
	    else if labchain <> 0 then
	      writepair (both, labchain, labaddr);
	    lab := nextlab
	  end;
	display [top].flabel := nil;
	(* since labels may be emitted for debug info
	   blocks before TOP is decremented *)
      end;
      writeprogblock: begin
      (*$Y2
      LIC := IC;
      CODEARRAY := FALSE;
      (*$X LLISTCODE := FALSE; *)
      progblockaddr := ic;
      WRITEBLOCKST (1B);
      writepair (left,fileic,ord(dref_ctp(pflist))); (* backlink through file blocks, ptr to last level 1 proc *)
      writepair (both,400000b,0b); (* relocation bases *)
      writepair (no,ord(ttyfptr^.selfctp),ord(intptr^.selfstp));
	(* chain level 0 file ids *)
      writeword (no,ord(realptr^.selfstp));
      writeword (no,ord(charptr^.selfstp));
      writeword (no,ord(boolptr^.selfstp));
      farray := fileb$ (smbfile);
      for i := 0 to 3 do
	writeword (no,farray[i]);
      ic := lic - 1;
      highestcode := ic;
      *)
      end;
      writeend: begin
	lic := 0;
	(*$x llistcode := false; *)
	writeblockst (10b);
	if progchain <> 0 then begin
	  writepair (no,-1,-1);
	  if debug then
	    writepair (both,progchain,progblockaddr)
	  else
	    writepair (left,progchain,377777b);
	end;
	if debug then
	  writepair (both,400000b,progblockaddr)
	else
	  writepair (left,400000b,377777b);
	writepair (no,-1,-1);			(* RESOLVE LEFT *)
	writepair (left,400000b,rx50name);	(* IDENTIFY AS ENTRY FOR DEBUGGER *)
	(*$x llistcode := listcode; *)
	writeblockst (5);
	(*$X WRITEHEADER ('HIGHSEG_BREAK  '); *)
	writepair(right,0,highestcode);
	lic := 0;				(*$X WRITEHEADER('LOWSEG-BREAK   '); *)
	writepair(right,0,statlc);
	putrelcode;
	if debug then
	  close (smbfile);
      end;
      writestart:
	if main then begin
	  lic := 0;				(*$X WRITEHEADER('STARTADDRESS   '); *)
	  writeblockst(7);
	  writepair(right,0,startaddr)
	end;
      writeentry: begin
	writeblockst(4);
	writeidentifier(0,entry)
      end;
      writename: begin
	writeblockst(6);
	writeidentifier(0,entry);
	rx50name := wandlung.wrighthalf;	(* right half of radix50 symbol *)
      end;
      writehiseg: begin
      (*$X LLISTCODE := FALSE;  *)
	writeblockst(3);
	writepair(no,400000b,400000b);
      end
    end						(*CASE*)
  end (*MCVARIOUS*);

$PAGE defsymbol - in writemc
  procedure defsymbol (class: storageclass; id: ctp);

  begin
    if id <> nil then begin
      if id^.llink <> nil then
	defsymbol (class, id^.llink);		(* GET ONES WITH LOWER NAMES FIRST *)
      with id^ do begin
	case klass of
	  vars: begin
	    if vclass = class then begin
	      if class = publicsc then begin
		reference (1b,name,right,vaddr);
	      end
	      else if (class = externalsc) and (vaddr <> 0) then begin
		reference (request,name,right,vaddr);
		vaddr := 0			(* RESET BACKLINK CHAIN *)
	      end
	    end
	  end;
	  proc, func: begin
	    if pfdeckind = declared then
	      if pfkind = actual then
		if (pfclass = class) and (class = externalsc)
		(* PUBLIC HANDLED AS ENTRYPOINTS *)
		then
		  if linkchain[0] <> 0 then begin
		    if pflev = 0 then
		      writeidentifier (14b, externalname)
		    else
		      writeidentifier (14b, name);
		    writepair (right, 0, linkchain[0]);
		    linkchain[0] := 0;		(* RESET BACKLINK CHAIN *)
		  end
	  end
	end
      end;
      if id^.rlink <> nil then
	defsymbol (class, id^.rlink);		(* GET ONES WITH HIGHER NAMES *)
    end
  end;

$PAGE mcsymbols - in writemc
  procedure mcsymbols;

  var
      misc_name: alfa;
      lastpf: ctp;
      entryfound: boolean;
      supportix: supports;

  begin
  (*MCSYMBOLS*)
    lic := 0;					(*$X WRITEHEADER('ENTRYPOINT(S)  '); *)
    writeblockst(2);
    (*$X SAVELISTCODE := LISTCODE;
    LISTCODE := FALSE;  *)
    for switchflag := 1b to 2b do begin
      entryfound := false;
      if main then begin
	reference (switchflag,entry,right,startaddr);
	reference (switchflag,'START.    ',right,startaddr);
	entryfound := true
      end;

      (* GET PUBLIC PROCS/FUNCS, EVEN IN MAIN COMPILATION *)
      checker := localpfptr;
      lastpf := nil;
      while checker <> nil do
	with checker^ do begin
	  if (pfaddr <> 0) and (pfclass = publicsc) then begin
	    if not entryfound then
	      entryfound := entry = name;
	    reference (switchflag,name,right,pfaddr);
	    lastpf := checker
	  end;
	  checker:= pfchain
	end;

	(* IF NO ENTRYPOINT WITH ENTRY NAME FOUND, THEN DEFINE
	   THE LAST VALID PROCEDURE WITH THAT NAME *)
      if (lastpf <> nil) and (not entryfound) then begin
	reference (switchflag,entry,right,lastpf^.pfaddr);
      end;

      (*$X  LISTCODE := SAVELISTCODE; *)
      lic := 0
    end;
    lic := 0;					(*$X WRITEHEADER ('GLOBAL SYMBOLS '); *)
    defsymbol (publicsc, display[1].fname);
    lic := 0;					(*$X WRITEHEADER ('GLOBAL REQUESTS'); *)
    misc_name := 'INIT .    ';
    misc_name[5] := chr (4*ord(incore)+2*ord(virtual and not incore) + ord (debug) + ord ('0'));
    reference (request,misc_name,no,0);
    if main then reference (request,'FIRST.    ',no,0);
    if mask_offset <> 0 then
	reference (request,'OMASK.    ',right,mask_offset);
    if area_mask <> 0 then
	reference (request,'AMASK.    ',right,area_mask);
    
    if ovfl_mask <> 0 then
	reference (request,'OVFL.     ',right,ovfl_mask);
    
    if v_nil <> 0 then
	reference (request,'VNIL.     ',right,v_nil);
    
    if shl_area <> 0 then
	reference (request,'A.SHL     ',right,shl_area);
    
    if shr_area <> 0 then
	reference (request,'A.SHR     ',right,shr_area);

    if alc_used and (allocation > 0) then begin
      misc_name := 'ALC .     ';
      if allocation < 10 then begin
	misc_name[4] := chr (allocation+ord('0'));
      end
      else begin
	misc_name[4] := chr ((allocation div 10)+ord('0'));
	misc_name[5] := chr ((allocation mod 10)+ord('0'));
	misc_name[6] := '.';
      end;
      reference (request,misc_name,no,0);
    end;
    defsymbol (externalsc, display[1].fname);
    defsymbol (externalsc, display[0].fname);
    if cbpssaddr <> 0 then begin
      reference (request,'CBPSS.    ',right,cbpssaddr);
      cbpssaddr := 0
    end;
    for supportix:= succ(firstsupport) to pred(lastsupport) do
      if rnts.link[supportix] <> 0 then begin
	reference (request,rnts.name[supportix],right,rnts.link[supportix]);
	rnts.link[supportix] := 0		(* RESET BACKLINK CHAIN *)
      end;
  end (*MCSYMBOLS*);

$PAGE mclibrary - in writemc
  procedure mclibrary;

  var
      languageix: symbol;

    procedure dumplibrec(library: librec);

    var
	highseg_ppn: integer;

    begin
      with library,wandlung do begin		(*USES J OF WRITEMC*)
	for j:=1 to 6 do
	  wsixbit[j]:=ord(name[j])-40b;
	writeidentifier (6b,name);
	writepair (no,projnr,prognr);
	for j:=1 to 6 do
	  wsixbit[j]:=ord(device[j])-40b;
	writeidentifier(6b,device);
	lic:= lic+1
      end
    end (*DUMPLIBREC*);

  begin
  (*MCLIBRARY*)
    lic := 0;					(*$X WRITEHEADER('LINK LIBRARIES '); *)
    writeblockst(15);
    (*ALWAYS DUMP PASCAL LIBRARY SEARCH RECORD*)
    dumplibrec(library[pascalsy]);
    (*NOW DUMP LIBREC'S ACCORDING TO ORDER OF
      DECLARED EXTERNAL PROCEDURES WHICH WERE CALLED*)
    for i:= 1 to libix do
      if library[liborder[i]].called andif (liborder[i]<>pascalsy) then
	dumplibrec(library[liborder[i]]);
	(*NOW DUMP LIBREC'S OF UNDECLARED EXTERNAL PROCEDURES
	  WHICH WERE CALLED, I.E., NON-PASCAL STD ROUTINES*)
    for languageix:= fortransy to cobolsy do	(*OMIT PASCAL*)
      with library[languageix] do
	if (not inorder (*NOT IN LIBORDER ARRAY*))and called then
	  dumplibrec(library[languageix])
  end (*MCLIBRARY*);

$PAGE writemc - body
begin
(*WRITEMC*)
  if not errorflag then begin
    for i:=1 to maxsize do
      relempty[i] := 0;
      (*$X LLISTCODE := LISTCODE;  *)
    case writeflag of
      writefileblocks:
	mcfileblocks;				(*LINK-ITEM 01B*)
(*$X5      WRITEGLOBALS :
	MCGLOBALS; *) (*LINK-ITEM 01B*)
      writeconstants:
	mcconstants;				(*LINK-ITEM 01B*)
      writecode :
	mccode;					(*LINK-ITEM 01B*)
      writesymbols :
	mcsymbols;				(*LINK-ITEM 02B*)
      writeinternals,				(*LINK-ITEM 10B*)
      writeentry,				(*LINK-ITEM 04B*)
      writeend,					(*LINK-ITEM 05B*)
      writestart,				(*LINK-ITEM 07B*)
      writeprogblock, writehiseg,		(*LINK-ITEM 03B*)
      writename :
	mcvarious;				(*LINK-ITEM 06B*)
      writelibrary :
	mclibrary				(*LINK-ITEM 17B*)
    end						(*$X ;
			      if listcode and (writeflag > writehiseg) then begin
			      writeln;
			      checklisting;
			      end; *)
  end						(*IF ERRORFLAG*)
  else if writeflag = writecode then
    lastbtp := nil
end (*WRITEMC*).
] 
QÛ