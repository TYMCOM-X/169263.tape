
      (*DESCRIBING:*)
      (*************)

      (*BASIC SYMBOLS*)
      (***************)

      symbol = (ident,intconst,realconst,stringconst,notsy,mulop,addop,relop,
		lparent,rparent,lbrack,rbrack,comma,semicolon,period,arrow,
		colon,becomes,labelsy,constsy,typesy,varsy,valuesy,functionsy,
		proceduresy,setsy,packedsy,arraysy,recordsy,filesy,forwardsy,
		beginsy,ifsy,casesy,repeatsy,whilesy,forsy,withsy,loopsy,
		gotosy,exitsy,endsy,elsesy,untilsy,ofsy,dosy,tosy,downtosy,
		externsy,pascalsy,fortransy,algolsy,cobolsy,programsy,
		thensy,othersy,initprocsy,elipsis,otherssy,
		externalsy,publicsy,staticsy,onsy,precsy,bysy,eofsy,modulesy,atsign);

      operator = (mul,rdiv,andop,idiv,imod,plus,minus,orop,ltop,leop,geop,gtop,
		  neop,eqop,inop,noop,concat,expn,orifop,andifop);

      setofsys = set of symbol;
      setofchar = set of char;

      supports = (firstsupport,stackoverflow,allocate,exitprogram,getline,getfile,putline,putfile,
		  resetfile,rewritefile,getcharacter,putpage,errorinassignment,
		  writepackedstring,writestring,writeboolean,readcharacter,readinteger,readreal,
		  breakoutput,opentty,initializedebug,enterdebug,indexerror,
		  writeoctal,writeinteger,writereal,
		  writehexadecimal,writecharacter,convertintegertoreal,convertrealtointeger,
		  markop,releaseop,disposeop,unwind,closeop,getchan,freechan,
		  openfile,pointererror,roundrealtointeger,cmssr,cmsf,cmff,
		  writevaryingstring,writesubstring,ctss,ctssr,cmpss,
		  ixss,ix3ss,srss,sr3ss,vfss,vf3ss,expii,expri,exprr,
		  procreturn,initprogram,procentry,ka10entry,ki10entry,
		  ka10exit,ki10exit,stmtblock,formalcall,
		   READDREAL,ROUNDDREAL,CONVERTDREALTOINTEGER,
		  DCONVERTINTEGERTOREAL,DXPRI,DXPRR,
		  unchainfile,initfileblock,	(* pointer file support *)
		  v_trans,	(* virtual pointer translation *)
		  v_new,a_new,v_dispose,v_mark,v_nil_word,v_release,	(* virtual heap routines *)
		  f_trans,
		  offsetoverflow,areacheck,getroot,setroot,
		  v_nil_offset,
		  (* NOTE!! the following must be in this order
			(see CALL)	*)
		  ln_function,ln_double,
		  log_function,log_double,
		  exp_function,exp_double,
		  sin_function,sin_double,
		  sind_function,sind_double,
		  sinh_function,sinh_double,
		  cos_function,cos_double,
		  cosd_function,cosd_double,
		  cosh_function,cosh_double,
		  tan_function,tan_double,
		  tanh_function,tanh_double,
		  asin_function,asin_double,
		  acos_function,acos_double,
		  atan_function,atan_double,
		  sqrt_function,sqrt_double,
		  atan2_function,atan2_double,
		  random_function,random_noseed, (* second not implemented *)
		  vtrans2,vtrans3,vtrans4,vtrans5,vtrans6,vtrans7,
		  vtrans10,vtrans11,vtrans12,vtrans13,vtrans14,
		  getfname,
		  (* unused supports which may be renamed as required *)
		  unused6,unused5,
		  unused4,unused3,unused2,unused1,
		  lastsupport);

      (* INPUT SOURCE SELECTION *)

	inputstate =  (prompting, reading, including);

      (*CONSTANTS*)
      (***********)

      stringarray = packed array[1..strglgth] of char;
      string60 = string[60];
      filnamestr = file_name;
      stringtype = string;
      stringkind = (varying, nonvarying);
      cstclass = (int,reel,pset,strd,strg);
      csp = ^ constnt;
      constnt = record
		  selfcsp: csp; nocode: boolean;
		  case cclass: cstclass of
		       int : (intval: integer; intval1:integer (*TO ACCESS SECOND WORD OF PVAL*) );
		       reel: (rval: real  ; RVAL2: INTEGER  );
		       pset: (pval: set of 0..71);
		       strd,
		       strg: (slgth: 0..strglgth;
			      sval: stringarray)
		end;

      valu = record
	       case boolean of
		    true:   (ival: integer);
		    false:  (valp: csp)
	     end;

      (*DATA STRUCTURES*)
      (*****************)

      levrange = 0..maxlevel; addrrange = 0..maxaddr; instrange = 0..677b ;
      radixrange = 0..37777777777b; flagrange = 0..17b;
      bitrange = 0..bitmax; acrange = 0..15; ibrange = 0..1; coderange = 0..cixmax ;
      bits6 = 0..77b;  bits7 = 0..177b;
      structform = (scalar,subrange,pointer,power,arrays,records,files,
		    tagfwithid,tagfwithoutid,variant,strings,formalprocfunc);
      typekind = (intkind, realkind, charkind);
      declkind = (standard,declared);
      stp = ^ structure; ctp = ^ identifier; btp = ^bytepoint;
      ftp = ^filblck;
      gtp = ^globptr ;

      structure = packed record
			   selfstp: stp; size: addrrange;
			   typeid: ctp;
			   bitsize: addrrange(*BITRANGE*); nocode: boolean;
			   db0: 0..17777b(*ALIGNMENT*);
			   case form: structform of
				scalar:   (case scalkind: declkind of
						declared: (db1: 0..377777b; fconst: ctp);
						standard: (vartype: typekind));
				subrange: (rangetype: stp; min,max: valu);
				pointer:  (eltype: stp; v_offset: boolean);
				power:	  (elset: stp);
				arrays:   (arraybpaddr: addrrange;
					   aeltype,inxtype: stp; arraypf: boolean);
				strings:  (maxlength: integer;
					   skind: stringkind);
				records:  (fstfld: ctp; recvar: stp; recordpf: boolean;
					   fieldfg: boolean);	(* TRUE IF FIELDS PRECEDE VARIANTS *)
				files:	  (filtype: stp; filepf: boolean);
				tagfwithid,
				tagfwithoutid: (fstvar: stp;
						case boolean of
						true : (tagfieldp: ctp);
						false  : (tagfieldtype: stp));
				variant:  (nxtvar,subvar: stp; firstfield: ctp;
					   varval: valu);
				formalprocfunc:  (proctyptr: ctp)
			 end;

      bpointer = packed record
			  sbits,pbits: bitrange;
			  ibit,dummybit: ibrange;
			  ireg: acrange;
			  reladdr: addrrange
			end;

      bytepoint = packed record
			   byte: bpointer;
			   last   :btp;
			   fieldcp: ctp
			 end;
      globptr = record
		  nextglobptr: gtp ;
		  firstglob,
		  lastglob   : addrrange ;
		  fcix	     : coderange
		end ;

      filblck = packed record
			 nextftp : ftp ;
			 fileident : ctp
		       end ;

      (*NAMES*)
      (*******)

      idclass = (types,konst,vars,field,proc,func);
      setofids = set of idclass;
      idkind = (actual,formal);
      packkind = (notpack, packk, packr, hwordr, hwordl);
      storageclass = (codesc, localsc,valparmsc,varparmsc,publicsc,
		externalsc, staticsc, defaultsc,spvalparmsc);
      charword = packed array [1..5] of char;
      (*ALFA = PACKED ARRAY [1..ALFALENG] OF CHAR;*)

      pfkey = 1..pfmaxkey;			(* KEY NUMBER FOR STD PROC/FUN'S *)

      identifier = packed record
	    name: alfa;
	    llink, rlink: ctp;
	    idtype: stp; next: ctp;
	    selfctp: ctp; nocode: boolean;
	    case klass: idclass of
		 konst: (values: valu);
		 vars:	(vkind: idkind; vclass: storageclass; vconst: boolean;
			 vlev: addrrange (*LEVRANGE*); vaddr: addrrange);
		 field: (packf: packkind; fldaddr: addrrange; fldbp: bpointer);
		 (*IF PACKF=PACKK THEN FLDADDR CONTAINS THE
		  ABSOLUTE ADDRESS OF THE CORRESPONDING BYTEPOINTER
		  -----> ENTERBODY*)
		 proc,
		  func:  (pfchain: ctp;
			  case pfdeckind: declkind of
			    standard: (key: pfkey);
			    declared: (language: symbol;
				       pflev: addrrange (*LEVRANGE*);
					pfaddr: addrrange;
					parmsize: addrrange;
					case pfkind: idkind of
					  actual: (forwdecl: boolean;
						   pfclass: storageclass;
						   testfwdptr: ctp;
						   pflink: ctp;
						   pflower: ctp;
						   externalname: alfa;
						   linkchain: packed array[levrange] of addrrange);
					  formal: (parameters: ctp)))
	end;

      disprange = 0..displimit;
      where = (blck,crec);

      (*RELOCATION*)
      (************)

      relbyte = 0..3b (*(NO,RIGHT,LEFT,BOTH)*);

      relword = packed array[0..17] of relbyte;

      (*EXPRESSIONS*)
      (*************)

      attrkind = (cst,varbl,expr,substrng);

      attr = record
	       typtr: stp;
	       case kind: attrkind of
		    cst:   (cval: valu);
		    varbl: (packfg: packkind; indexr: acrange; indbit: ibrange;
			    vlevel: levrange; bpaddr,dplmt: addrrange; vrelbyte: relbyte; subkind: stp;
			     vsclass: storageclass; vid: ctp);
		    substrng,
		    expr:  (reg:acrange)
	     end;

      testp = ^ testpointer;
      testpointer = packed record
			     elt1,elt2: stp;
			     lasttestp: testp
			   end;

      (*  LABEL DEFINITION  *)
       (**********************)

      labp = ^ labl;
      labl = record
		nextlab: labp;			(* NEXT LABEL DECLARED AT THIS LEVEL *)
		lkind:   declkind;		(* DECL => IN LAB DCL, STD => NOT *)
		labval:  integer;		(* LABEL 'NAME' *)
		labchain:addrrange;		(* CHAIN OR REFS TO LABEL *)
		defined: boolean;		(* IMPLIES ADDR IS GOOD *)
		labaddr: addrrange		(* LOC IDENTIFED BY LABEL *)
	    end;

      (*TYPES FROM BODY *)
      (******************)

      ksp = ^ konstrec;
      konstrec = packed record
			  addr: array[0..2] of addrrange;   (* BACKLINK TO OFFSETS 0,1,2 FROM CONST BASE *)
			  kaddr: addrrange;
			  constptr: csp;
			  nextkonst: ksp
			end;

      pdp10instr = packed record
			    instr   : instrange ;
			    ac	    : acrange;
			    indbit  : ibrange;
			    inxreg  : acrange;
			    address : addrrange
			  end ;

      halfs = packed record
		       lefthalf: addrrange;
		       righthalf: addrrange
		     end;

      librec = record				(*LIBRARY SEARCH RECORD*)
		 inorder,called: boolean;
		 name: alfa;
		 projnr, prognr: addrrange;
		 device: alfa
	       end;

      (* TYPES FOR COMMAND SCANNER  *)

      linenrtype = packed array [1..5] of char;

      fileinfo = packed record

	fwdfile,				(* IC of file block for "local request" to its last page block *)

	fwdpage,				(* IC of last page block emitted for local request to
						  its last statement block *)

	laststmt,				(* IC of last statement block *)

	linecnt,				(* listing line # of file *)

	pagecnt,				(* listing page # *)

	localline,				(* lines this page *)

	fileid,					(* FILECNT of this file *)

	lastpage,				(* PAGECNT of last page block emitted *)

	lastline				(* LOCALLINE # of last stmt block emitted *)

	  : addrrange;
	was_in_system: boolean;
	was_listing: boolean

	end;

      (*------------------------------------------------------------------------------*)

 