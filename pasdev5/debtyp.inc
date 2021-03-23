$options nosource

const
  maxlevel = 8;
  strglgth = 120;

  (* register addresses and misc. offsets *)

  ac0 = 0;
  ac1 = 1;
  linkreg = 1;					(* jsp 1,pntry. and jsp 1,stmt. *)
  reg = 2;					(* first register for parameters and expression evaluation *)
  regmax = 6;					(* highest register used for parameters *)
  overflowreg = 7;				(* contains address of rest of parameters if
				   can't fit in registers 2->6 *)
  basis = 16b;					(* base of local stack frame *)
  topp = 17b;					(* top of local stack frame *)
  oldbasis = 1;					(* within a procedure/function, caller's basis
				     stored in local stack frame at 1(basis) *)
  oldtopp = 2;					(* within a procedure/function, caller's topp
				      stored in local stack frame at 2(basis) *)
  linkword = 3;					(* address of word following jsp 1,pntry.
				     stored in stack frame at 3(basis) *)
  func_value = 4;				(* within function, result stored at 4(basis) *)
  func_result = 5;				(* on return from function, result is at 5(topp) *)

   maxdisplevel = 8;
   maxwithlevel = 11;
   maxstacklevel = 500;				(* some arbitrarily large number *)

type
  half_word = 0..777777b;
  addrrange = 0..777777b;

  ctp = ^identifier;

  stp = ^structure;

  csp = ^constant;

  intctp = half_word;				(* rel file contains integers that will be converted to ctp *)

  intstp = half_word;				(* integers to stps *)

  intcsp = half_word;				(* integers to csps *)


  bitrange = 0..36;

  ibrange = 0..1;

  acrange = 0..17b;
$PAGE type definitions for stmt,page,and file blocks

  stmtblkptr = ^stmt_block;
  stmt_block = packed record			(* word following jsp 1,stmt. *)
		 previous_stmt: stmtblkptr;	(* ^page_block for first stmt *)
		 line_number: half_word
	      end;

  pageblkptr = ^page_block;
  page_block = packed record			(* four words *)
		 previous_page: pageblkptr;	(* ^file_block for first page in file *)
		 last_stmt: stmtblkptr;		(* to last stmt in page *)
		 page_filler: half_word;	(* free halfword *)
		 page_number: half_word;
		 subtitle: alfa
	      end;

  fileblkptr = ^file_block;
  file_block = packed record			(* four words *)
		 previous_file: fileblkptr;	(* zero for first file *)
		 last_page: pageblkptr;		(* block for last page in file *)
		 mod_word_ptr: ^module_word;	(*indirect pointer to program block*)
		 file_number: half_word;
		 file_name: alfa
	      end;
$PAGE program and procedure blocks

   sixbit = 0..77b;
   sixbit6 = packed array[1..6] of sixbit;
   sixbit3 = packed array[1..3] of sixbit;


   fileblock = packed record
      case boolean of				(* overlay field names:  *)
      true : (device    : sixbit6;		(* odevice *)
	      name      : sixbit6;		(* oname *)
	      ext       : sixbit3;		(* ext_fill *)
	      proj      : half_word;		(* proj_prog *)
	      prog      : half_word);
      false: (odevice   : integer;
	      oname     : integer;
	      ext_fill  : integer;
	      proj_prog : integer)
   end (* fileblock *);

 progblkptr = ^program_block;
  program_block = packed record			(* one per rel file *)
		    last_file: fileblkptr;	(* backlink through "previous_file" fields *)
		    level1_proc_ptr: intctp;	(*pointer to level1 routines
						  chained through pfchain*)
		    hiseg_base: half_word;	(* code relocation base *)
		    lowseg_base: half_word;	(* static relocation base *)
		    level0_head: intctp;	(*pointer to level0 external symbols
						  (at the moment, only standard files)
						  chained through next*)
		    intpoint: intstp;		(* intptr *)
		    realleft: half_word;	(* filler *)
		    realpoint: intstp;		(* realptr *)
		    charleft: half_word;	(* filler *)
		    charpoint: intstp;		(* charptr *)
		    boolleft: half_word;
		    boolpoint: intstp;
		    symfile_name: fileblock
		  end;

  procblkptr = ^procedure_block;
  procedure_block = packed record		(* four words *)
		       proc_name: alfa;
		       pflev: half_word;	(* level *)
		       proc_ptr: intctp;	(* pointer to identifier record for the procedure or zero *)
		       prog_blk: progblkptr;	(* link to program data area or zero *)
		       filler2: half_word	(* unused *)
		     end;

  module_word = packed record			(*first word emitted in hiseg of debug compilation*)
		  right_r50: half_word;		(*right half of radix-50 name*)
		  prog_blk: progblkptr
		end;
$PAGE entry block, stack frame, constant structure


   stkframeptr = ^stack_frame;
  stack_frame= packed record			(* reserved storage in stack for procedure/functions *)
		  rtn_flags: half_word;
		  rtn_addr:  half_word;
		  caller_basis: stkframeptr;
		  parent_basis: stkframeptr;
		  stack_size: half_word;	(* negative count of words remaining in stack *)
		  caller_top: stkframeptr;	(* topp on entry to procedure *)
		  link_flags: half_word;	(* flags from jsp 1,pntry. *)
		  link_addr: ^procblklink;	(* address of word following jsp 1,pntry. *)
		  case boolean of
		    true: (f_value: integer)	(* value if function *)
		end;

  procblklink = packed record
		  filler1:half_word;
		  proc_blk:procblkptr
	       end;


  stringarray = packed array[1..strglgth] of char;
  stringkind = (varying, nonvarying);
  const_class = (int, reel, pset, strd, strg);

  constant = record				(* for  sets, and strings only *)
	     case cclass: const_class of
	       int : (intval : integer;
		      intval1: integer);	(* to access second word of a set *)
	       reel: (rval: real);
	       pset: (pval: set of 0..71);
	       strd,				(* debugger can treat strd and strg as synonymous *)
	       strg: (slgth: 0..strglgth;	(* legnth of string following *)
	       sval:  stringarray)		(* actual string *)
	      end;

  valu = record
	    case boolean of			(* whether value or pointer determined 
				     from other information (idtype, etc.) *)
	      true:  (ival: integer);		(* integers and chars *)
	      false: (valp: intcsp)		(* other constants *)
	     end;
$PAGE structure definition
  (* since address fields in structure and identifier records are not
     relocated by link they do not have to be halfword aligned; thus all
     "filler" fields have been removed *)

  (* structure record contains primary type information *)

  structform = (scalar, subrange, pointer, power, arrays, records, files,
  tagfwithid, tagfwithoutid, variant, strings, formalprocfunc);
  typekind = (intkind, realkind, charkind);	(* for standard scalar types
	     integer, real and char *)
  declkind = (standard, declared);
  levrange = 0..maxlevel;			(* level of declaration *)

  structure = packed record
		 size: half_word;		(* max # words required by type *)
		 ctptr: intctp;			(* link to type identifier *)
		 bitsize: bitrange;		(* minimum bits required *)
		 case form: structform of
		   scalar: (case scalkind: declkind of
		       declared: (fconst: intctp);  (* to last scalar constant *)
		       standard: (vartype: typekind));	(* only for integer, real and char *)
		   subrange: (rangetype: intstp;    (* range of what? *)
			      min,		(* min.ival is lowerbound of subrange *)
			      max: valu);	(* max.ival is upperbound of subrange *)
		   pointer:  (eltype: intstp;	(* points to what type? *)
			      v_offset: boolean);(* true if offset *)
		   power:    (elset:  intstp);	(* type of set elements *)
		   arrays: (arraypf: boolean;	(* true if packed *)
			    aeltype: intstp;	(* type of array elements *)
			    inxtype: intstp);	(* type of array index *)
		   strings: (skind: stringkind;	(* varying or not *)
			     maxlength: integer);
		   records: (recordpf: boolean;	(* true if packed *)
			     fieldsf: boolean;	(* true if  a nonvarient fields exists *)
			     fstfld: intctp;	(* to identifier record for first
			   field--rest chained through "next" *)
			     recvar: intstp);	(* to last variant *)
		   files:   (filepf: boolean;	(* true if packed *)
			     filtype: intstp);	(* file of what? *)
		   tagfwithid,
		   tagfwithoutid:
				  (fstvar: intstp;  (* to last variant *)
				   case boolean of
				     true:  (tagfieldp: intctp);    (* identifier record for tagfield *)
				     false: (tagfieldtype: intstp));	(* type of tagfield if undiscriminated union *)
		   variant: (nxtvar,		(* link to previous variant *)
			     subvar: intstp;	(* to internal tagfield and variants *)
			     firstfield: intctp;    (* first field within variant *)
			     varval: valu);	(* variant value *)
		   formalprocfunc: (proctyptr: intctp)	(* type info formal procs/funcs
			  stored in ctp record *)
		  end;
$PAGE 
  idclass = (types, konst, vars, field, proc, func);
  idkind = (actual, formal);
  packkind = (notpack, packk, packr, hwordr, hwordl);	(* record packing arrangements *)
  storageclass = 
       (codesc,					(* local procedures *)
	localsc,				(* local variables *)
	valparmsc,				(* "value" parameters *)
	varparmsc,				(* "var" parameters *)
	publicsc,
	externalsc,
	staticsc,
	defaultsc,				(* used by compiler only *)
	spvalparmsc);				(* value parameter passed by address *)
  symbol = (ident, intconst, realconst, stringconst, notsy, mulop, addop,
     relop, lparent, rparent, lbrack, rbrack, comma, semicolon, period,
     arrow, colon, becomes, labelsy, constsy, typesy, varsy, valuesy,
     functionsy, proceduresy, setsy, packedsy, arraysy, recordsy,
     filesy, forwardsy, beginsy, ifsy, casesy, repeatsy, whilesy,
     forsy, withsy, loopsy, gotosy, exitsy, endsy, elsesy, untilsy,
     ofsy, dosy, tosy, downtosy, externsy, pascalsy, fortransy,
     algolsy, cobolsy, programsy, thensy, othersy, initprocsy,
     elipsis, otherssy, externalsy, publicsy, staticsy, onsy, precsy,
     bysy, eofsy);

  bpointer = packed record
	       sbits, pbits: bitrange;
	       ibit, dummybit: ibrange;
	       ireg: acrange;
	       reladdr: half_word
	     end;

$PAGE addressing information
   addr_type = record
	       wordoffset:addrrange;
	       bitoffset:bitrange;
	       packedflag:boolean		(* indicates the bitoffset is significant *)
	    end;

    indirectword = packed record
		      filler: half_word;
		      addr : addrrange
		   end;
$PAGE identifier definition
  identifier =
      packed record
	name: alfa;
	llink,					(* usual left subtree *)
	rlink: intctp;				(* right subtree *)
	idtype: intstp;				(* link to type info *)
	next: intctp;				(* links parameters, fields, and scalars *)
	case klass: idclass of
	  (* no variant for "types" - info store in idtype^ *)
	  konst:  (values: valu);		(* value or pointer to constant record *)
	  vars:  (vkind: idkind;		(* actual or formal *)
		  vclass: storageclass;		(* determines usage of vaddr *)
		  vconst: boolean;		(* true if structured constant *)
		  vlevel: levrange;		(* declaration level *)
		  vaddr: half_word);		(* address *)
	  field: (packf: packkind;		(* packing clue *)
		  fldaddr: half_word;		(* offset within record if not packk *)
		  fldbp: bpointer);		(* if packk, byte pointer for ldb/dpb access *)
	  proc,
	  func: (language: symbol;
		 pflevel: levrange;		(* level of declaration *)
		 pfaddr: half_word;		(* address *)
		 parmsize: half_word;		(* parameter overflow data *)
		 case pfkind: idkind of
		   actual: (pfclass: storageclass;
			    pfchain: intctp;	(* pointer to other procs at this level *)
			    pflower: intctp;	(* pointer to internal procedures *)
			    externalname: alfa);
		   formal: (parameters: intctp))
      end;

   display =
      record
	prog_blk :   progblkptr;		(* points to program block of containing mod *)
				(* or nil if not compiled in debug mode *)
	proc_ptr :   intctp;			(* nil if main or unknown *)
	idtree  :   intctp;			(* root of id tree for lower symbols, if known *)
	stackbase: stkframeptr;			(* base of stk frame for this level, or nil if unknown *)
	staticbase: half_word			(* base of static storage or 0 if unknown *)
      end;

  with_reference =
      record
	rectype: intstp;			(* record type *)
	recbase: addr_type;			(* address of start of record *)
	fldidtree: intctp			(* base of field id tree (1st id) *)
      end;


   withlevel = 0..maxwithlevel;
   displaylevel = 0..maxdisplevel;
   stklevel = 0..maxstacklevel;

   name_list = record
		   name_count:displaylevel;
		   name:array[1..maxdisplevel] of alfa 
		end;

   scope_type = record
		  display_levels:displaylevel;	(* static lev of scope opened *)
		  displays:array[1..maxdisplevel] of display
		end;

  with_type = record
		active_withs: withlevel;
		withs: array[1..maxwithlevel] of with_reference
	      end;
$PAGE descriptor definition

type
  desc_kinds =
      (	scalar_dt, int_dt, real_dt, char_dt, fstring_dt, vstring_dt, file_dt,
	pointer_dt, array_dt, record_dt, set_dt, subr_dt			);

  descriptor =
      record
	valueparm: boolean;			(* indicates not assignable *)
	cstvalue: boolean;			(* name is of a constant *)
	addr: addr_type;			(* address of value, may point into the below *)
	value: constant;			(* recepticle for constant values *)
	typtr: intstp;				(* type for those interested *)
	bitsize: bitrange;	(* bitwidth of datum, from stp *)
	case kind: desc_kinds of
	  scalar_dt, int_dt, char_dt:
	    ( minval, maxval: integer;
	      basetype: intstp	      );	(* for scalar_dt, else nil *)
	  array_dt:
	    ( ael, inx: intstp );		(* element and index types *)
	  pointer_dt:
	    ( el: intstp;			(* target type *)
	      vir_offset: boolean );		(* true if offset *)
	  record_dt:
	    ( fst: intctp );			(* first field *)
	  file_dt:
	    ( fil: intstp );			(* file component type *)
	  set_dt:
	    ( sel: intstp );		(* set element type *)
	  fstring_dt, vstring_dt:
	    ( charsize: integer  )		(* (max) number of chars *)
       end;
$PAGE link symbol table
const
   modtype = 0;					(* link sym tab entry type *)

type r50word = 0..37777777777b;			(* 32 bits of info *)
type
   symbol_type = 0..17b;
   linkentry = packed record
	  case stype:symbol_type of
	    0:  (modname:r50word;
		       stsize :half_word;	(* neg size of local st for this mod *)
		       firstword: ^module_word);
	    1..17b:   (symname:r50word;
			filler :half_word;
			symaddr:half_word)
	   end;

   linkentryptr = ^linkentry;

$PAGE error return codes

type

      (* returns from stack_scope, frame_scope, mod_scope, ext_scope routines *)

  scopereturn = ( notdefined,			(*module is not defined*)
		  notpascal,			(*module is not pascal module*)
		  noscope,			(*module compiled in notrace mode*)
		  notdebug,			(*module not in debug mode*)
		  debug,			(*module in debug mode -- normal return*)
		  badnest,			(*list names passed not properly nested routines*)
		  wronginvocation);		(*desired invocation of innermost name not found*)

      (* returns from <reference> parsing procedures *)

  parse_error = ( success,			(*normal return*)
		  ixtype_wrong,			(*array script of wrong type*)
		  too_many_subs,		(*too many subscripts given*)
		  not_array_string,		(*script follows non array/string name*)
		  not_record,			(*field name follows non record name*)
		  not_ptr_file,			(*dereference follows non pointer/file name*)
		  no_such_field,		(*field name not defined in record*)
		  low_scalar,			(*scalar value too small*)
		  high_scalar,			(*scalar value too large*)
		  nil_value,			(*dreferenced pointer is nil*)
		  not_id,			(*identifier expected*)
		  not_scalar,			(*scalar value expected*)
		  not_modifiable,		(*<reference> has constant value*)
		  not_defined,			(*identifier is not defined*)
		  right_bkt_expected,		(* ] expected after subscript(s) *)
		  not_addressible,		(*stack frame of local variable not defined*)
		  ptr_uninitialized,		(*dereferenced pointer is zero*)
		  wrong_type,			(*type conflict of operands*)
		  not_variable,		(* const, type, etc. used in place of var *)
		  bad_set);	(* elements of set out of order or range *)

const						(*to disambiguate symbols of interest to debugger*)
    plus = labelsy;				(*for symbol '+'*)
    minus = constsy;				(*for symbol '_'*)
    atsign = typesy;				(*for symbol ''*)
    slash = varsy;				(*for symbol '/'*)

type						(*descriptive information on statements*)

  source_id = packed record
		mname,fname,pname: alfa;	(*names of module,file,page*)
		fnumber,pnumber,lnumber: half_word  (*numbers of ...*)
	      end;
$options source
    