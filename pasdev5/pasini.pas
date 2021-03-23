$PAGE Static initialization

const
 filenam_length = 80;

static const

 int_str: structure := (nil,1,nil,bitmax,false,0,scalar,standard,intkind);

 real_str: structure := (nil,1,nil,bitmax,false,0,scalar,standard,realkind);

 char_str: structure := (nil,1,nil,7,false,0,scalar,standard,charkind);

 bool_str: structure := (nil,1,nil,1,false,0,scalar,declared,0,nil);

 nil_str: structure := (nil,1,nil,18,false,0,pointer,nil,false);

 text_str: structure := (nil,1,nil,bitmax,false,0,files,^char_str,false);

 onetonine: structure := (nil,1,nil,4,false,0,subrange,^int_str,(true,1),(true,9));

 date_str: structure := (nil,2,nil,bitmax,false,0,arrays,0,^char_str,^onetonine,true);

 onetoten: structure := (nil,2,nil,bitmax,false,0,subrange,^int_str,(true,1),(true,alfaleng));

 alfa_str: structure := (nil,2,nil,bitmax,false,0,arrays,0,^char_str,^onetoten,true);

 string_str: structure := (nil,26,nil,bitmax,false,0,strings,128,varying);

 str30_str: structure := (nil,7,nil,bitmax,false,0,strings,30,varying);

 str30_id: identifier := (' ',nil,nil,^str30_str,nil,nil,false,vars,actual,valparmsc,false,1,0);

 heap_boolean: identifier := (' ',nil,nil,^bool_str,nil,nil,false,vars,actual,valparmsc,false,1,0);

 heap_id2: identifier := (' ',nil,nil,^str30_str,^heap_boolean,nil,false,vars,actual,valparmsc,false,1,0);

 heap_id1: identifier := (' ',nil,nil,^str30_str,nil,nil,false,vars,actual,valparmsc,false,1,0);


 area_id: identifier := (' ',nil,nil,^int_str,nil,nil,false,vars,actual,valparmsc,false,0,0);

 area_string: identifier := (' ',nil,nil,^string_str,nil,nil,false,vars,actual,
	valparmsc,false,0,0);

 area_id_param: identifier := (' ',nil,nil,^int_str,^area_string,nil,false,vars,
	actual,valparmsc,false,0,0);

 int_parm: identifier := (' ',nil,nil,^int_str,nil,nil,false,vars,actual,
	valparmsc,false,0,0);

 filenam_str: structure := (nil,17,nil,bitmax,false,0,strings,filenam_length,varying);

public const

 intptr: stp := ^int_str;
 realptr: stp := ^real_str;
 charptr: stp := ^char_str;
 boolptr: stp := ^bool_str;
 nilptr: stp := ^nil_str;
 textptr: stp := ^text_str;
 dateptr: stp := ^date_str;
 alfaptr: stp := ^alfa_str;
 arbptrtyp: stp := ^nil_str;
 strptr: stp := ^string_str;

public var

 lastbtp: btp := nil;
(*$x5 fglobptr: gtp := nil; *)
 fileptr: ftp := nil;

 sfileptr: ftp := nil;
$PAGE Level 0 symbol table

(* keys for standard procedures and functions:

	key	function	procedure
	---	--------	---------

	 1	*RUNTIME*	GET
	 2	*TIME*		GETLN
	 3	ABS		PUT
	 4	SQR		PUTLN
	 5	TRUNC		RESET
	 6	ODD		REWRITE
	 7	ORD		READ
	 8	CHR		READLN
	 9	PRED		BREAK
	10	SUCC		WRITE
	11	EOF		WRITELN
	12	EOLN		PACK
	13	CHANNEL		UNPACK
	14	MINIMUM		NEW
	15	MAXIMUM		MARK
	16	SIZE		RELEASE
	17	UPPERCASE	GETLINENR
	18	LOWERCASE	*PUT8BITSTOTTY*
	19	ADDRESS		PAGE
	20	ROUND		DISPOSE
	21	LENGTH		PROTECTION
	22	SUBSTR		CLOSE
	23	INDEX		GETCHANNEL
	24	SEARCH		FREECHANNEL
	25	VERIFY		OPEN
	26	UPPERBOUND	STOP
	27	LOWERBOUND	RETURN
	28	PTR		SETROOT
	29	MIN
	30	MAX
	31	FLOAT
	32	ROOT
	33	POINTER
	34	OFFSET
	35	AREAID
	36	LN
	37	LOG
	38	EXP
	39	SIN
	40	SIND
	41	SINH
	42	COS
	43	COSD
	44	COSH
	45	TAN
	46	TANH
	47	ARCSIN
	48	ARCCOS
	49	ARCTAN
	50	SQRT
	51	ARCTAN
	52	RANDOM
	53	FILENAME

   Note:  RUNTIME, TIME, and PUT8BITSTOTTY are now defined as external
          functions and their keys are unused.

*)
$PAGE tree of level 0 identifiers

  (***********************************************************************)
  (***********************************************************************)
  (**********							**********)
  (**********		Level 0 symbol table			**********)
  (**********							**********)
  (**********	Head node assumed to be NIL_ID			**********)
  (**********							**********)
  (***********************************************************************)
  (***********************************************************************)

static const

  abs_id: identifier := ('ABS',nil,nil,nil,nil,nil,false,func,nil,standard,3);

  address_id: identifier := ('ADDRESS',^abs_id,nil,nil,nil,nil,false,func,nil,standard,19);

  a_save_id: identifier := ('AREA_SAVE',nil,nil,nil,^area_id_param,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.SAVE',(0,0,0,0,0,0,0,0,0));

  a_create_id: identifier := ('AREA_CREATE',nil,nil,^int_str,nil,nil,false,
	func,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.CREATE',(0,0,0,0,0,0,0,0,0));

  a_extent_id: identifier := ('AREA_EXTENT',nil,nil,nil,^area_id,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.EXTENT',(0,0,0,0,0,0,0,0,0));

  a_delete_id: identifier := ('AREA_DELETE',^a_create_id,^a_extent_id,nil,^area_id,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.DELETE',(0,0,0,0,0,0,0,0,0));

  a_load_id: identifier := ('AREA_LOAD',nil,nil,nil,^area_id_param,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.LOAD',(0,0,0,0,0,0,0,0,0));

  a_out_id: identifier := ('AREA_OUT',^a_load_id,^a_save_id,nil,^area_id,nil,false,proc,nil,
	declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,'AREA.OUT',
	(0,0,0,0,0,0,0,0,0));

  a_in_id: identifier := ('AREA_IN',^a_delete_id,^a_out_id,nil,^area_id,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'AREA.IN',(0,0,0,0,0,0,0,0,0));

  arccos_id: identifier := ('ARCCOS',nil,nil,^real_str,nil,nil,false,func,nil,standard,48);

  arctan_id: identifier := ('ARCTAN',nil,nil,^real_str,nil,nil,false,func,nil,standard,49);

  arcsin_id: identifier := ('ARCSIN',^arccos_id,^arctan_id,^real_str,nil,nil,false,func,nil,standard,47);

  areaid_id: identifier := ('AREAID',^arcsin_id,^a_in_id,nil,nil,nil,false,func,nil,standard,35);

  alfaleng_id: identifier := ('ALFALENG',nil,^areaid_id,^int_str,nil,nil,false,konst,
		(true,10));

  alfa_id: identifier := ('ALFA',^address_id,^alfaleng_id,^alfa_str,nil,nil,false,types);

  channel_id: identifier := ('CHANNEL',nil,nil,nil,nil,nil,false,func,nil,standard,13);

  break_id: identifier := ('BREAK',nil,^channel_id,nil,nil,nil,false,proc,nil,standard,9);

  bool_id: identifier := ('BOOLEAN',^alfa_id,^break_id,^bool_str,nil,nil,false,types);

  cosd_id: identifier := ('COSD',nil,nil,^real_str,nil,nil,false,func,nil,standard,43);

  cosh_id: identifier := ('COSH',^cosd_id,nil,^real_str,nil,nil,false,func,nil,standard,44);

  cos_id: identifier := ('COS',nil,^cosh_id,^real_str,nil,nil,false,func,nil,standard,42);

  cvr_id: identifier := ('CV_REAL',^cos_id,nil,^real_str,^str30_id,nil,false,
		func,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,
		nil,nil,'STR2R.',(0,0,0,0,0,0,0,0,0));

  comp_id: identifier := ('COMPDATE',nil,^cvr_id,^date_str,nil,nil,false,konst,(true,0));
	(* must NEW (comp_id.values.valp), etc. *)

  dispose_id: identifier := ('DISPOSE',nil,nil,nil,nil,nil,false,proc,nil,standard,20);

  date_id: identifier := ('DATE',^comp_id,^dispose_id,^date_str,nil,nil,false,
		func,nil,declared,fortransy,0,0,0,actual,false,externalsc,
		nil,nil,nil,'DATE',(0,0,0,0,0,0,0,0,0));

  close_id: identifier := ('CLOSE',nil,^date_id,nil,nil,nil,false,proc,nil,standard,22);

  chr_id: identifier := ('CHR',nil,^close_id,nil,nil,nil,false,func,nil,standard,8);

  exp_id: identifier := ('EXP',nil,nil,^real_str,nil,nil,false,func,nil,standard,38);

  eoln_id: identifier := ('EOLN',nil,^exp_id,nil,nil,nil,false,func,nil,standard,12);

  eof_id: identifier := ('EOF',^chr_id,^eoln_id,nil,nil,nil,false,func,nil,standard,11);

  char_id: identifier := ('CHAR',^bool_id,^eof_id,^char_str,nil,nil,false,types);

  filelen_id: identifier := ('FILE_NAME',nil,nil,^filenam_str,nil,nil,false,types);

  filenam_id: identifier := ('FILENAME',nil,^filelen_id,nil,nil,nil,false,func,nil,standard,53);

  freech_id: identifier := ('FREECHANNEL',nil,nil,nil,nil,nil,false,
		proc,nil,standard,24);

  float_id: identifier := ('FLOAT',^filenam_id,^freech_id,nil,nil,nil,false,
		func,nil,standard,31);

  getlnr_id: identifier := ('GETLINENR',nil,nil,nil,nil,nil,false,
		proc,nil,standard,17);

  getch_id: identifier := ('GETCHANNEL',nil,^getlnr_id,nil,nil,nil,false,
		proc,nil,standard,23);

  get_id: identifier := ('GET',^float_id,^getch_id,nil,nil,nil,false,
		proc,nil,standard,1);

  heap_save_id: identifier := ('HEAP_SAVE',nil,nil,nil,^heap_id1,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'HEAP.SAVE',(0,0,0,0,0,0,0,0,0));

  heap_create_id: identifier := ('HEAP_CREATE',nil,nil,nil,^heap_id2,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'HEAP.CREATE',(0,0,0,0,0,0,0,0,0));

  heap_load_id: identifier := ('HEAP_LOAD',^heap_create_id,^heap_save_id,nil,^heap_id2,nil,false,
	proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,nil,nil,nil,
	'HEAP.LOAD',(0,0,0,0,0,0,0,0,0));

  index_id: identifier := ('INDEX',^heap_load_id,nil,nil,nil,nil,false,
		func,nil,standard,23);

  getln_id: identifier := ('GETLN',^get_id,^index_id,nil,nil,nil,false,
		proc,nil,standard,2);

  int_id: identifier := ('INTEGER',nil,nil,^int_str,nil,nil,false,
		types);

  input_id: identifier := ('INPUT',^getln_id,^int_id,^text_str,nil,nil,false,
		vars,actual,externalsc,false,0,0);

  ln_id: identifier := ('LN',nil,nil,^real_str,nil,nil,false,func,nil,standard,36);

  log_id: identifier := ('LOG',^ln_id,nil,^real_str,nil,nil,false,func,nil,standard,37);

  lwb_id: identifier := ('LOWERBOUND',^log_id,nil,nil,nil,nil,false,
		func,nil,standard,27);

  max_id: identifier := ('MAX',nil,nil,nil,nil,nil,false,func,nil,standard,30);

  mark_id: identifier := ('MARK',nil,^max_id,nil,nil,nil,false,
		proc,nil,standard,15);

  min_id: identifier := ('MIN',nil,nil,nil,nil,nil,false,func,nil,standard,29);

  mini_id: identifier := ('MINIMUM',^min_id,nil,nil,nil,nil,false,
		func,nil,standard,14);

  maxi_id: identifier := ('MAXIMUM',^mark_id,^mini_id,nil,nil,nil,false,
		func,nil,standard,15);

  lowerc_id: identifier := ('LOWERCASE',^lwb_id,^maxi_id,nil,nil,nil,false,
		func,nil,standard,18);

  new_id: identifier := ('NEW',^lowerc_id,nil,nil,nil,nil,false,
		proc,nil,standard,14);

  length_id: identifier := ('LENGTH',^input_id,^new_id,nil,nil,nil,false,
		func,nil,standard,21);

  false_id: identifier := ('FALSE',^char_id,^length_id,^bool_str,nil,nil,false,
		konst,(true,0));

  offset_id: identifier := ('OFFSET',nil,nil,nil,nil,nil,false,func,nil,standard,34);

  odd_id: identifier := ('ODD',nil,^offset_id,nil,nil,nil,false,
		func,nil,standard,6);

  open_id: identifier := ('OPEN',^odd_id,nil,nil,nil,nil,false,
		proc,nil,standard,25);

  ord_id: identifier := ('ORD',^open_id,nil,nil,nil,nil,false,
		func,nil,standard,7);

  pack_id: identifier := ('PACK',nil,nil,nil,nil,nil,false,
		proc,nil,standard,12);

  pointer_id: identifier := ('POINTER',nil,nil,nil,nil,nil,false,func,nil,standard,33);

  page_id: identifier := ('PAGE',^pack_id,^pointer_id,nil,nil,nil,false,
		proc,nil,standard,19);

  protect_id: identifier := ('PROTECTION',nil,nil,nil,nil,nil,false,
		proc, nil, standard, 21);

  ptr_id: identifier := ('PTR',^protect_id,nil,nil,nil,nil,false,
		func,nil,standard,28);

  ptr_type: identifier := ('PTR',^protect_id,nil,^nil_str,nil,nil,false,types);

  pred_id: identifier := ('PRED',^page_id,^ptr_id,nil,nil,nil,false,
		func,nil,standard,9);

  put8_id: identifier := ('PUT8BITSTOTTY',nil,nil,nil,^int_parm,nil,false,
		proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,'PUT.8B',(0,0,0,0,0,0,0,0,0));

  random_id: identifier := ('RANDOM',nil,nil,^real_str,nil,nil,false,func,nil,standard,52);

  putln_id: identifier := ('PUTLN',^put8_id,^random_id,nil,nil,nil,false,
		proc,nil,standard,4);

  put_id: identifier := ('PUT',^pred_id,^putln_id,nil,nil,nil,false,
		proc,nil,standard,3);

  readln_id: identifier := ('READLN',nil,nil,nil,nil,nil,false,
		proc,nil,standard,8);

  read_id: identifier := ('READ',^put_id,^readln_id,nil,nil,nil,false,
		proc,nil,standard,7);

  output_id: identifier := ('OUTPUT',^ord_id,^read_id,^text_str,^input_id,nil,false,
		vars,actual,externalsc,false,0,0);

  release_id: identifier := ('RELEASE',nil,nil,nil,nil,nil,false,
		proc,nil,standard,16);

  return_id: identifier := ('RETURN',nil,nil,nil,nil,nil,false,
		proc,nil,standard,27);

  reset_id: identifier := ('RESET',^release_id,^return_id,nil,nil,nil,false,
		proc,nil,standard,5);

  runtime_id: identifier := ('RUNTIME',nil,nil,^int_str,nil,nil,false,
		func,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,'RN.TIM',(0,0,0,0,0,0,0,0,0));

  root_id: identifier := ('ROOT',nil,nil,nil,nil,nil,false,func,nil,standard,32);

  round_id: identifier := ('ROUND',^root_id,^runtime_id,^real_str,nil,nil,false,
		func,nil,standard,20);

  sind_id: identifier := ('SIND',nil,nil,^real_str,nil,nil,false,func,nil,standard,40);

  sinh_id: identifier := ('SINH',^sind_id,nil,^real_str,nil,nil,false,func,nil,standard,41);

  sin_id: identifier := ('SIN',nil,^sinh_id,^real_str,nil,nil,false,func,nil,standard,39);

  setroot_id: identifier := ('SETROOT',nil,^sin_id,nil,nil,nil,false,proc,nil,standard,28);

  search_id: identifier := ('SEARCH',^round_id,^setroot_id,nil,nil,nil,false,
		func,nil,standard,24);

  sqrt_id: identifier := ('SQRT',nil,nil,^real_str,nil,nil,false,func,nil,standard,50);

  sqr_id: identifier := ('SQR',nil,^sqrt_id,nil,nil,nil,false,
		func,nil,standard,4);

  stop_id: identifier := ('STOP',^sqr_id,nil,nil,nil,nil,false,
		proc,nil,standard,26);

  size_id: identifier := ('SIZE',^search_id,^stop_id,nil,nil,nil,false,
		func,nil,standard,16);

  rewrite_id: identifier := ('REWRITE',^reset_id,^size_id,nil,nil,nil,false,
		proc,nil,standard,6);

  real_id: identifier := ('REAL',^output_id,^rewrite_id,^real_str,nil,nil,false,
		types);


  tanh_id: identifier := ('TANH',nil,nil,^real_str,nil,nil,false,func,nil,standard,46);

  tan_id: identifier := ('TAN',nil,^tanh_id,^real_str,nil,nil,false,func,nil,standard,45);

  succ_id: identifier := ('SUCC',nil,^tan_id,nil,nil,nil,false,
		func,nil,standard,10);

  trace_id: identifier := ('TRACE',nil,nil,nil,nil,nil,false,
		proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,'TRACE',(0,0,0,0,0,0,0,0,0));

  time_id: identifier := ('TIME',nil,^trace_id,^int_str,nil,nil,false,
		func,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,'DA.TIM',(0,0,0,0,0,0,0,0,0));

  text_id: identifier := ('TEXT',^succ_id,^time_id,^text_str,nil,nil,false,
		types);

  substr_id: identifier := ('SUBSTR',nil,^text_id,nil,nil,nil,false,
		func,nil,standard,22);

  trunc_id: identifier := ('TRUNC',nil,nil,nil,nil,nil,false,
		func,nil,standard,5);

  true_id: identifier := ('TRUE',^substr_id,^trunc_id,^bool_str,^false_id,nil,false,
		konst,(true,1));

  unpack_id: identifier := ('UNPACK',nil,nil,nil,nil,nil,false,
		proc,nil,standard,13);

  upb_id: identifier := ('UPPERBOUND',^unpack_id,nil,nil,nil,nil,false,
		func,nil,standard,26);

  ttyout_id: identifier := ('TTYOUTPUT',nil,^upb_id,^text_str,^output_id,nil,false,
		vars,actual,externalsc,false,0,0);

  verify_id: identifier := ('VERIFY',nil,nil,nil,nil,nil,false,
		func,nil,standard,25);

  upperc_id: identifier := ('UPPERCASE',^ttyout_id,^verify_id,nil,nil,nil,false,
		func,nil,standard,17);

  writeln_id: identifier := ('WRITELN',nil,nil,nil,nil,nil,false,
		proc,nil,standard,11);

  write_id: identifier := ('WRITE',^upperc_id,^writeln_id,nil,nil,nil,false,
		proc,nil,standard,10);

  tty_id: identifier := ('TTY',^true_id,^write_id,^text_str,^ttyout_id,nil,false,
		vars,actual,externalsc,false,0,0);

  string_id: identifier := ('STRING',^real_id,^tty_id,^string_str,nil,nil,false,
		types);

  nil_id: identifier := ('NIL',^false_id,^string_id,^nil_str,nil,nil,false,
		konst,(true,ord(nil)));
$PAGE Undefined ID types
  utype_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		types);

  ucst_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		konst,(true,0));

  uvar_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		vars,actual,externalsc,false,0,0);

  ufld_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		field,notpack,0,(0,0,0,0,0,0));

  uproc_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		proc,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,' ',(0,0,0,0,0,0,0,0,0));

  ufunc_id: identifier := (' ',nil,nil,nil,nil,nil,false,
		func,nil,declared,pascalsy,0,0,0,actual,false,externalsc,
		nil,nil,nil,' ',(0,0,0,0,0,0,0,0,0));
$PAGE File pointers, undefined ID's, and symbol sets
public const

  head_symbol: ctp := ^nil_id;

  ttyfptr: ctp := ^tty_id;			(* also head of chain of level 0 file ids *)

  toutfptr: ctp := ^ttyout_id;

  inputfptr: ctp := ^input_id;

  outfptr: ctp := ^output_id;

  utypptr: ctp := ^utype_id;

  ucstptr: ctp := ^ucst_id;

  uvarptr: ctp := ^uvar_id;

  ufldptr: ctp := ^ufld_id;

  uprcptr: ctp := ^uproc_id;

  ufctptr: ctp := ^ufunc_id;

  digits: setofchar := ['0'..'9'];

  alphabetic: setofchar := ['A'..'Z'];

  hexadigits: setofchar := ['0'..'9','A'..'F'];

  lettersdigitsorleftarrow: setofchar := ['0'..'9','A'..'Z','_','$'];

  languagesys: setofsys := [fortransy, algolsy, cobolsy, pascalsy];

  constbegsys: setofsys := [addop, intconst, realconst, stringconst, ident];

  simptypebegsys: setofsys := [addop, intconst, realconst, stringconst, ident, lparent];

  typebegsys: setofsys := [addop, intconst, realconst, stringconst, ident, lparent,
			  arrow, atsign, packedsy, arraysy, recordsy, setsy, filesy];

  typedels: setofsys := [arraysy, recordsy, setsy, filesy];

  blockbegsys: setofsys := [labelsy, constsy, typesy, varsy, publicsy, externalsy,
			   staticsy, initprocsy, proceduresy, functionsy, beginsy];

  selsys: setofsys := [arrow, period, lbrack, lparent];

  facbegsys: setofsys := [intconst, realconst, gconst, ident, lparent, lbrack, notsy];

  statbegsys: setofsys := [beginsy, gotosy, ifsy, whilesy, repeatsy, loopsy,
			  forsy, withsy, casesy];
$PAGE Runtime support routines
public var

  rnts: record
	 name: array[supports] of alfa;
	 link: packed array[supports] of addrrange
	end   := 

      (((* firstsupport *)	'  ',
	(* stackoverflow *)	'CORER.',
	(* allocate *)		'NEW.',
	(* exitprogram *)	'END.',
	(* getline *)		'GETLN.',
	(* getfile *)		'GET.',
	(* putline *)		'PUTLN.',
	(* putfile *)		'PUT.',
	(* resetfile *)		'RESET.',
	(* rewritefile *)	'REWRT.',
	(* getcharacter *)	'GETCH.',
	(* putpage *)		'PUTPG.',
	(* errorinassignment *)	'SRERR.',
	(* writepackedstring *)	'WRPST.',
	(* writestring *)	'WRUST.',
	(* writeboolean *)	'WRBOL.',
	(* readcharacter *)	'READC.',
	(* readinteger *)	'READI.',
	(* readreal *)		'READR.',
	(* breakoutput *)	'BREAK.',
	(* opentty *)		'TTYOP.',
	(* initializedebug *)	'INDEB.',
	(* enterdebug *)	'EXDEB.',
	(* indexerror *)	'INXER.',
	(* writeoctal *)	'WROCT.',
	(* writeinteger *)	'WRINT.',
	(* writereal *)		'WRTRL.',
	(* writehexadecimal *)	'WRHEX.',
	(* writecharacter *)	'WRITC.',
	(* convertintegertoreal *) 'INTRL.',
	(* convertrealtointeger *) 'TRUNC.',
	(* markop *)		'MARK.',
	(* releaseop *)		'RLEAS.',
	(* disposeop *)		'DSPOS.',
	(* unwind *)		'UNWND.',
	(* closeop *)		'CLOSE.',
	(* getchan *)		'GETCN.',
	(* freechan *)		'FRECN.',
	(* openfile *)		'OPEN.',
	(* pointererror *)	'PTRER.',
	(* roundrealtointeger *) 'ROUND.',
	(* cmssr *)		'CMSSR.',
	(* cmsf *)		'CMSF.',
	(* cmff *)		'CMFF.',
	(* writevaryingstring *) 'WRVST.',
	(* writesubstring *)	'WRSST.',
	(* ctss *)		'CTSS.',
	(* ctssr *)		'CTSSR.',
	(* cmpss *)		'CMPSS.',
	(* ixss *)		'IXSS.',
	(* ix3ss *)		'IX3SS.',
	(* srss *)		'SRSS.',
	(* sr3ss *)		'SR3SS.',
	(* vfss *)		'VFSS.',
	(* vf3ss *)		'VF3SS.',
	(* expii *)		'EXPII.',
	(* expri *)		'EXPRI.',
	(* exprr *)		'EXPRR.',
	(* procreturn *)	'PRTRN.',
	(* initprogram *)	'INITP.',
	(* procentry *)		'PNTRY.',
	(* ka10entry *)		'KA10N.',
	(* ki10entry *)		'KI10N.',
	(* ka10exit *)		'KA10X.',
	(* ki10exit *)		'KI10X.',
	(* stmtblock *)		'STMT.',
	(* formalcall *)	'PCALL.',
(*$y4
	(* readdreal *)		'READD.',
	(* rounddreal *)	'DRND.',
	(* convertdrealtointeger *) 'TRUND.',
	(* dconvertintegertoreal *) 'INDRL.',
	(* dxpri *)		'DXPRI.',
	(* dxprr *)		'DXPRR.',
*)
	(* unchainfile *)	'UNCHN.    ',
	(* initfileblock *)	'INIFB.    ',
	(* v_trans *)		'VTRNS.    ',
	(* v_new *)		'VNEW.     ',
	(* a_new *)		'ANEW.     ',
	(* v_dispose *)		'VDPS.     ',
	(* v_mark *)		'VMARK.    ',
	(* v_nil_word *)	'VNILW.    ',
	(* v_release *)		'VRLES.    ',
	(* f_trans *)		'FTRNS.    ',
	(* offsetoverflow *)	'VOVFL.    ',
	(* areacheck *)		'ACHCK.    ',
	(* getroot *)		'ROOT.     ',
	(* setroot *)		'SROOT.    ',
	(* v_nil_offset *)	'VNILO.    ',
	(* ln_function *)	'R.LN      ',
	(* ln_double *)		'D.LN      ',
	(* log_function *)	'R.LOG     ',
	(* log_double *)	'D.LOG     ',
	(* exp_function *)	'R.EXP     ',
	(* exp_double *)	'D.EXP     ',
	(* sin_function *)	'R.SIN     ',
	(* sin_double *)	'D.SIN     ',
	(* sind_function *)	'R.SIND    ',
	(* sind_double *)	'D.SIND    ',
	(* sinh_function *)	'R.SINH    ',
	(* sinh_double *)	'D.SINH    ',
	(* cos_function *)	'R.COS     ',
	(* cos_double *)	'D.COS     ',
	(* cosd_function *)	'R.COSD    ',
	(* cosd_double *)	'D.COSD    ',
	(* cosh_function *)	'R.COSH    ',
	(* cosh_double *)	'D.COSH    ',
	(* tan_function *)	'R.TAN     ',
	(* tan_double *)	'D.TAN     ',
	(* tanh_function *)	'R.TANH    ',
	(* tanh_double *)	'D.TANH    ',
	(* asin_function *)	'R.ASIN    ',
	(* asin_double *)	'D.ASIN    ',
	(* acos_function *)	'R.ACOS    ',
	(* acos_double *)	'D.ACOS    ',
	(* atan_function *)	'R.ATN     ',
	(* atan_double *)	'D.ATN     ',
	(* sqrt_function *)	'R.SQRT    ',
	(* sqrt_double *)	'D.SQRT    ',
	(* atan2_function *)	'R.ATN2    ',
	(* atan2_double *)	'D.ATN2    ',
	(* random_function *)	'RAND1.    ',
	(* random_noseed *)	'RAND0.    ',
	(* vtrans2 *)		'VTR2.     ',
	(* vtrans3 *)		'VTR3.     ',
	(* vtrans4 *)		'VTR4.     ',
	(* vtrans5 *)		'VTR5.     ',
	(* vtrans6 *)		'VTR6.     ',
	(* vtrans7 *)		'VTR7.     ',
	(* vtrans10 *)		'VTR10.    ',
	(* vtrans11 *)		'VTR11.    ',
	(* vtrans12 *)		'VTR12.    ',
	(* vtrans13 *)		'VTR13.    ',
	(* vtrans14 *)		'VTR14.    ',
	(* getfname *)		'FNAME.    ',
	(* unused6 *)		'  ',
	(* unused5 *)		'  ',
	(* unused4 *)		'  ',
	(* unused3 *)		'  ',
	(* unused2 *)		'  ',
	(* unused1 *)		'  ',
	(* lastsupport *)	'  '),

       (						(* now backlinks for all supports *)
	0,0,0,0,0,0,0,0,0,0,			(* firstsupport :==> rewritefile *)
	0,0,0,0,0,0,0,0,0,0,			(* getcharacter :==> breakoutput *)
	0,0,0,0,0,0,0,0,0,0,			(* opentty :==> convertintegertoreal *)
	0,0,0,0,0,0,0,0,0,0,			(* convertrealtointeger :==> pointererror *)
	0,0,0,0,0,0,0,0,0,0,			(* roundrealtointeger :==> ixss *)
	0,0,0,0,0,0,0,0,0,0,			(* ix3ss :==> initprogram *)
	0,0,0,0,0,0,0,0,0,0,		(* procentry :==> convertdrealtointeger *)
	0,0,0,0,0,0,0,0,0,0,		(* dconvertdrealtointeger :==> v_mark *)
	0,0,0,0,0,0,0,0,0,0,		(* v_nil_word :==> ln_double *)
	0,0,0,0,0,0,0,0,0,0,		(* log_function :==> sinh_double *)
	0,0,0,0,0,0,0,0,0,0,		(* cos_function :==> tanh_double *)
	0,0,0,0,0,0,0,0,0,0,		(* asin_function :==> atan2_double *)
	0,0,0,0,0,0,0,0,0,0,		(* random_function :==> vtrans11 *)
	0,0,0,0,0,0,0,0,0,0,		(* vtrans12 :==> unused1 *)
	0));				(* lastsupport *)

public var

  library: array[pascalsy..cobolsy] of librec :=

       ((false,false,'PASLIB',0,0,'DSK'),
	(false,false,'FORLIB',0,0,'SYS'),
	(false,false,'ALGLIB',0,0,'SYS'),
	(false,false,'LIBOL ',0,0,'SYS'));

  pas: linenrtype := '.PAS ';
  lst: linenrtype := '.LST ';
  rel: linenrtype := '.REL ';
  errmsgfile: alfa := 'PASMSG.TXT';

$PAGE set_ptr_type

public procedure set_ptr_type (klass: idclass);
(* In declarations sections, PTR is considered a type, whereas
   in expression evaluation it is considered a function (see BODY). *)

begin
  if klass = types
    then pred_id.rlink := address (ptr_type)
    else pred_id.rlink := address (ptr_id);
end;
$PAGE Level 0 symbol table completion
procedure init_0;
var highseg_ppn: packed record
  case boolean of
    true:  (ppn: integer);
    false: (proj: addrrange; prog: addrrange)
  end;
begin
  (* complete level 0 symbol table *)
  set_ptr_type (types);	(* initially a type *)
  bool_str.fconst := address (true_id);
  input_id.next := address (true_id);
  int_str.typeid := address (int_id);
  real_str.typeid := address (real_id);
  char_str.typeid := address (char_id);
  bool_str.typeid := address (bool_id);
  nil_str.typeid := address (nil_id);
  text_str.typeid := address (text_id);
  date_str.typeid := address (date_id);
  alfa_str.typeid := address (alfa_id);
  string_str.typeid := address (string_id);
  filenam_str.typeid := address (filelen_id);
  with highseg_ppn do begin
    prgm_ppn (ppn);
    with library[pascalsy] do begin
      projnr := proj;
      prognr := prog;
    end;
  end;
  with comp_id do begin
    new (values.valp, strg: 9);
    with values.valp^ do begin
      selfcsp := nil; nocode := false;
      slgth := 9;
      sval[1:9] := date;
    end;
  end;
end;
    o
