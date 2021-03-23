$LENGTH 43
$OPTIONS special,notrace,nocheck

module addre$;

$INCLUDE debtyp.inc

const
    uuo_maxac = 1;				(* needed by douuo to get/store byte *)
$INCLUDE douuo.inc

external function deref$ctp(ptr:integer):ctp;

external function deref$stp(ptr:integer):stp;

external function deref$csp (ptr: integer): csp;

external function sym$lookup(name:alfa;var modname:alfa):linkentryptr;

external function pas$lookup(idtree:intctp; symname:alfa; var symid:ctp)
  :intctp;

external procedure in$symbol;

external function vtran$ (integer): integer;

external function dtran$ (integer): integer;

external const v$ncore: boolean;

external function v$poffset (integer; integer): integer;

external function v$offset (integer): integer;

external const v$nil: integer;

external const v$rtual: boolean;

external var
    cur$sy:symbol;
    cur$id:alfa;
    cur$val:constant;
    scope$stack:scope_type;
    with$stack: with_type;
$PAGE low level routines to extract or store a scalar

type
    action = (load,deposit);

procedure ld_dep_byte(fn:action;word:addrrange;bit:bitrange;bitsize:bitrange
  var value:integer);

  (* this routine creates a pdp10 byte pointer and executes douuo
      to do the specified function,load or deposit byte *)


const
    dpb = 137b;					(* opcode for deposit byte *)
    ldb = 135b;					(* opcode for load byte *)


var
    bp:bpointer;

var
    acs:uuo_acblk;

begin						(* ld_dep_byte *)
  with bp do begin
    sbits := bitsize;
    pbits := 36 - (bit + sbits);
    reladdr := word;	(* !!NOTE!! assume virtual addresses have been translated *)
    ibit := 0;
    ireg := 0;
    dummybit := 0;
  end;						(* with *)
  if fn = load then begin
    if douuo(ldb,acs,1,0,ord(address(bp))) = noskip then
      ;
    value := acs[1];
  end
  else begin
    acs[1] := value;
    if douuo(dpb,acs,1,0,ord(address(bp))) = noskip then
      ;
  end;						(* if *)
end;						(* ld_dep_byte *)


public procedure store$scalar (desc: descriptor; value: integer);

var intptr:^integer;
    word: integer;

begin						(* store$scalar *)
  with desc.addr do begin
    if v$rtual then
      word := vtran$ (wordoffset)
    else word := wordoffset;
    if not packedflag then begin
      intptr := ptr(word);
      intptr^ := value;
    end
    else
      ld_dep_byte(deposit,word,bitoffset,desc.bitsize,value);
    end;
end;						(* store$scalar *)


function extract_scalar (desc: descriptor): integer;

 var intptr: ^ integer;
     word: integer;

begin						(* extract_scalar *)
  with desc, addr do begin
    if v$rtual then
      word := vtran$ (wordoffset)	(* translate to real address *)
    else word := wordoffset;
    if v$rtual andif (kind = pointer_dt) andif vir_offset then begin
      (* if loading an offset must call runtime routines to 
	 propagate areaid, nil, etc. *)
      if packedflag then
	extract_scalar := v$poffset (word, bitoffset)
      else extract_scalar := v$offset (word);
    end
    else if not packedflag then begin
      intptr := ptr(word);
      extract_scalar := intptr^;
    end
    else
      ld_dep_byte(load,word,bitoffset,desc.bitsize,extract_scalar);
  end;
end;						(* extract_scalar *)

$PAGE get_field_addr

function get_field_addr(fieldid:ctp;recbase:addr_type):addr_type;

(* calculate the addr_type of a field, possibly with a bit offset,
    given the addr_type of the start of the record *)


begin
  with get_field_addr do begin
    if fieldid^.packf = packk then begin
      packedflag := true;
      with fieldid^.fldbp do begin
	bitoffset := 36 - (sbits + pbits);
	wordoffset := reladdr + recbase.wordoffset;
      end;					(* with fieldid^.fldbp *)
    end
    else if fieldid^.packf in [hwordr, hwordl] then begin
      packedflag := true;
      wordoffset := recbase.wordoffset + fieldid^.fldaddr;
      if fieldid^.packf = hwordl
        then bitoffset := 0
        else bitoffset := 18;
    end
    else begin
      packedflag := false;
      wordoffset := fieldid^.fldaddr + recbase.wordoffset;
    end;					(* if *)
  end;						(* with get_field_addr *)
end;						(* get_field_addr *)

$PAGE get_element_addr

function get_element_addr
     (	arraytype: stp; arraybase: addr_type; indexval: integer; var error: parse_error  ): addr_type;

  (* calculates the addr_type of one element of an array, possibly with
     a bit offset, given the addr_type of the start of the array *)

var
    lbound,ubound:integer;
    ael,inx: intstp;
    aeltype, inxtype: stp;
    scalkonst: ctp;
    aelsize:bitrange;
    aelperword:bitrange;
    desired_element:bitrange;

begin						(* get_element_addr *)
  error := success;
  get_element_addr.packedflag := arraytype^.arraypf;
  ael := arraytype^.aeltype;
  inx := arraytype^.inxtype;
  inxtype := deref$stp(inx);

  (* Check against bounds of subrange types only.  Lower bound of
     other possible script types (char and declared scalar) is
     always zero, so set lbound to zero. *)

  if inxtype^.form<>subrange then begin (*is scalar*)
    lbound:= 0;
    if inxtype^.scalkind=standard then
      ubound:= ord(maximum(char)) (*can only be character*)
    else begin
      scalkonst:= deref$ctp(inxtype^.fconst);
      ubound:= scalkonst^.values.ival
    end
  end
  else begin
    ubound := inxtype^.max.ival;
    lbound := inxtype^.min.ival;
  end;
  if (indexval<lbound) then
    error := low_scalar
  else if (indexval>ubound) then
    error := high_scalar
  else begin
    aeltype := deref$stp (ael);
    desired_element := indexval - lbound;
    with get_element_addr do begin
      if packedflag then begin
        aelsize := aeltype^.bitsize;
	aelperword := 36 div aelsize;
	wordoffset := arraybase.wordoffset + (desired_element div aelperword);
	bitoffset := aelsize * (desired_element mod aelperword);
      end
      else wordoffset := arraybase.wordoffset + (aeltype^.size * desired_element);
    end;					(* with *)
  end;						(* if indexval *)
end;						(* get_element_addr  *)

$PAGE get_substr_addr

function get_substr_addr
     (	desc: descriptor; stringindex: integer; var error: parse_error ): addr_type;

  (* calculates the address of an element of the specified string *)


var
    base:addrrange;
    stringlen:integer;
    intptr:^integer;

begin						(* get_substr_addr *)
  error := success;
  if stringindex < 1 then
    error:=low_scalar;
  if desc.kind = fstring_dt then begin
    base := desc.addr.wordoffset;
    if stringindex > desc.charsize then
      error := high_scalar;
  end
  else begin					(* kind = vstring_dt *)
    base := desc.addr.wordoffset + 1;
    if v$rtual then
      intptr := ptr (vtran$ (desc.addr.wordoffset))
    else intptr := ptr(desc.addr.wordoffset);
    stringlen := intptr^;
    if stringindex > stringlen then
      error := high_scalar;
  end;						(* if *)
  if error in [high_scalar,low_scalar] then
    return;
  with get_substr_addr do begin
    packedflag := true;
    wordoffset := base + ((stringindex-1) div 5);   (* 5 chars per wd *)
    bitoffset := ((stringindex-1) mod 5) * 7;	(* 7 bits per char *)
  end;						(* with *)
end;						(* get_substr_addr *)

$PAGE get_address

function get_address(idrec:ctp;d:display):addr_type;

(* calculates the address of the specified non field,non scalar
  const identifier. ord(nil) is returned if the address is unknown *)
(* ie, the lhs$ref is stack relative, and its containing procedure
   has never been called--no stack frame exists.  also, structured
   constants work a bit differently if they are public or static. the
   hiseg_base address must be retrieved from the program block, and
   vaddr is added to it *)


var
    indirectptr :^indirectword;
    modname:alfa;				(* for module name if var is external *)
    entry:linkentryptr;
    functype:stp;

begin						(* get_address *)
  with get_address, idrec^, d do begin
    packedflag := false;
    if klass = vars then
      case vclass of
	localsc, valparmsc: begin
	  if stackbase <> nil then
	    wordoffset := ord(stackbase) + vaddr
	  else
	    wordoffset := ord(nil);
	end;
	varparmsc, spvalparmsc : begin
	  if stackbase <> nil then begin
	    indirectptr := ptr(ord(stackbase) + vaddr);
	    wordoffset := indirectptr^.addr;
	    if v$ncore then
	      wordoffset := dtran$ (wordoffset);(* make virtual again *)
	  end
	  else
	    wordoffset := ord(nil);
	end;
	staticsc, publicsc : begin
	  if vconst then
	  (* this is a structured constant *)

	    if vaddr > 400000b then (*const in hi-seg*)
	      wordoffset:= prog_blk^.hiseg_base + (vaddr-400000b)
	    else (* is "static" const in low segment *)
	      wordoffset:= prog_blk^.lowseg_base + vaddr
	  else
	    wordoffset := staticbase + vaddr;
	end;
	externalsc : begin
	  modname := '';			(* unknown,search whole st *)
	  entry := sym$lookup(name,modname);
	  wordoffset := entry^.symaddr;
	end
      end					(* case *)
    else					(* klass = func *)
    if stackbase = nil then
      wordoffset := ord(nil)
    else begin
      functype := deref$stp(idtype);
      if functype^.size <= 2 then
	wordoffset := ord(stackbase) + func_value
      else begin
	indirectptr := ptr(ord(stackbase) + func_value);
	wordoffset := indirectptr^.addr;
      end;					(* if size *)
    end;					(* stackbase <> nil *)
  end;						(* with *)
end;						(* get_address *)
$PAGE copy$words
public procedure copy$words(destination:addrrange; source:addrrange; wd_cnt:integer);

(* transfers wd_cnt words from the source address specified to
   the destination address specified  *)


var
    sourceptr,destptr:^integer;
    ctr:integer;

begin						(* copy$words *)
  if v$rtual then begin
    sourceptr := ptr (vtran$ (source));
    destptr := ptr (vtran$ (destination));
  end
  else begin
    sourceptr := ptr(source);
    destptr := ptr(destination);
  end;
  for ctr := 1 to wd_cnt do begin
    destptr^ := sourceptr^;
    sourceptr := ptr(ord(sourceptr) + 1);
    destptr := ptr(ord(destptr) + 1);
  end;						(* for *)
end;						(* copy$words *)
$PAGE id_lookup
function id_lookup(cur$id:alfa; var d:display; var idctp:ctp):intctp;

(* searches each idtree in scope$stack to determine if the cur$id is accessible
   if the cur$id isnt in scope$stack idctp = nil, and intctp = ord(nil).

   level0 symbols are searched when the other lis has been exhausted
   the level0 ctp is stored in the program block, and all level o
   symbols are chained through next.

   on exit, d is set to the display where the symbol is found, or
   to the level one display if level0 or not found.
*)


var
    level:displaylevels;

begin						(* id_lookup *)
  with scope$stack do begin
    level := display_levels;
    loop
      id_lookup := pas$lookup(displays[level].idtree,cur$id,idctp);
    exit if (id_lookup <> ord(nil)) or (level = 1);
      level := level - 1;
    end;					(* loop *)
    if id_lookup <> ord(nil)			(* we found it *)
    then
      d := displays[level]
    else begin

    (* must search level0 ids. get head from program block
       and follow next chain *)

      d := displays[1];
      id_lookup := displays[1].prog_blk^.level0_head;
      idctp := deref$ctp(id_lookup);
      while (idctp <> nil ) andif ( idctp^.name <> cur$id) do begin
	id_lookup := idctp^.next;
	idctp := deref$ctp(id_lookup);
      end;					(* while *)
    end;					(* level0 lookup *)
  end;						(* with scope$stack *)
end;						(* id_lookup *)
$PAGE with_lookup

function with_lookup (cur$id: alfa; var addr: addr_type; var idctp: ctp): intctp;

 (* This searches the stack of active withs for a record containing a field
    with the specified name. *)

 var level: withlevel;

 begin
  with_lookup := ord (nil);
  with with$stack do begin
    for level := active_withs downto 1 do begin
      with_lookup := pas$lookup (withs[level].fldidtree, cur$id, idctp);
    exit if with_lookup <> ord (nil)
      do addr := withs[level].recbase;
    end;
  end;
 end;
$PAGE func_lookup
function func_lookup(funcctp:intctp; var d:display):boolean;

(* determine if function value is defined, ie the function
   ctp is equal to a proc_ptr in the currently defined scope$stack *)


var
    level:displaylevel;

begin						(* func_lookup *)
  func_lookup := true;
  with scope$stack do begin
    for level := display_levels downto 1 do
      if displays[level].proc_ptr = funcctp then begin
	d := displays[level];
	return;
      end;					(* if and for *)
    func_lookup := false;
  end;						(* with scope$stack *)
end;						(* func_lookup *)
$PAGE named$const

public function named$const(konst:constant; konsttype:intstp):alfa;

var 
   konst_stp:stp;
   konst_ctp:ctp;

begin
   named$const := '?Bad Value';
   konst_stp := deref$stp(konsttype);
   konst_ctp := deref$ctp(konst_stp^.fconst);
   while konst_ctp^.values.ival <> konst.intval do begin
     konst_ctp := deref$ctp(konst_ctp^.next);
     if konst_ctp = nil then return;
   end;						(* while *)
   named$const := konst_ctp^.name;
end;						(* named$const *)
$PAGE forward decls: Scalar_Value and Check_Type

procedure scalar_value
	( var scalar: descriptor;
	  var status: parse_error );  forward;

public function check$type
	( item: descriptor;
	  required_type: intstp ): parse_error;   forward;

$PAGE set$kind

public procedure set$kind ( var desc: descriptor; idtypeptr: intstp );

(* sets the descriptor type information according to the stp.
   and saves necessary intstps and intctps needed by selector
   because care must be taken when accessing fields of things
   that have been derefed via deref_xxx  *)

var
    idstp:stp;
    inxstp:stp;
    testdesc: descriptor;

begin						(* set$kind *)
  desc.typtr := idtypeptr;
  idstp := deref$stp(idtypeptr);
  with idstp^ do begin
    desc.bitsize := bitsize;
    case form of
      subrange:
	begin					(* some info from this, some from base type *)
	  desc.minval := min.ival;			(* get limits of range *)
	  desc.maxval := max.ival;
	  set$kind (testdesc, rangetype);
	  desc.kind := testdesc.kind;		(* these derive from base type *)
	  desc.basetype := testdesc.basetype;
	end;

      scalar:
	begin
	  desc.minval := -377777777777B-1;		(* allow all values of type *)
	  desc.maxval := 377777777777B;
	  if scalkind = declared
	    then desc.kind := scalar_dt
	  else if vartype = intkind
	    then desc.kind := int_dt
	  else if vartype = realkind
	    then desc.kind := real_dt
	  else if vartype = charkind
	    then desc.kind := char_dt;
	  if desc.kind = scalar_dt
	    then desc.basetype := desc.typtr
	    else desc.basetype := ord (nil);
	end;

      pointer:
	begin
	  desc.kind := pointer_dt;
	  desc.el := eltype;
	  desc.vir_offset := v_offset;
	end;

      power:
	begin
	  desc.kind := set_dt;
	  desc.sel := elset;
	end;

      arrays:
	begin
	  desc.ael := aeltype;
	  desc.inx := inxtype;
	  set$kind (testdesc, desc.inx);	(* to check for fixed length string *)
	  inxstp := deref$stp (desc.inx);
	  if (scope$stack.displays[1].prog_blk^.charpoint = desc.ael) and arraypf and
	      (testdesc.kind = int_dt) and
	       ((inxstp^.form = subrange) andif (inxstp^.min.ival = 1))
	    then begin
	      desc.kind := fstring_dt;
	      desc.charsize := inxstp^.max.ival;
	    end
	    else desc.kind := array_dt;
	end;

      strings:
	begin
	  desc.kind := vstring_dt;	(* only string[n] enter as this type *)
	  desc.charsize := maxlength;
	end;

      records:
	begin
	  desc.kind := record_dt;
	  desc.fst := fstfld;
	end;

      files:
	begin
	  desc.kind := file_dt;
	  desc.fil := filtype;
	end

    end;					(*case *)
  end;					(* with *)
end;						(* set$kind *)

$PAGE selector

procedure selector (var desc: descriptor; var status: parse_error);

var
    idptr: intctp;				(* "name" of cur$id referenced *)
    idtypeptr: intstp;
    idstp:stp;					(* temp for immediate derefs *)
    idctp,fldctp:ctp;
    fld:intctp;
    indirectptr: ^indirectword;
    constp: csp;
    d: display;					(* within which to address cur$id *)
    index: descriptor;


begin						(* selector *)
 with desc do begin
  status := success;
  if cur$sy <> ident then begin		(* must start with cur$id *)
    status := not_id;
    return;
  end;
  idptr := with_lookup (cur$id, addr, idctp);
  if idptr = ord (nil) then
    idptr := id_lookup (cur$id, d, idctp);
  if idctp = nil then begin			(* cur$id not found *)
    status := not_defined;
    return;
  end;
  if not (idctp^.klass in [konst,vars,field,func]) then begin
    status:= not_variable;
    return
  end;
  in$symbol;
  idtypeptr := idctp^.idtype;
  set$kind (desc, idtypeptr);
  valueparm := false; cstvalue := false;

  idctp:= deref$ctp(idptr); (*set$kind may deref something else*)
  with idctp^ do case klass of

    konst:
      begin
	if kind in [real_dt, int_dt, scalar_dt, char_dt]	(* copy the value cell *)
	  then begin (*scalars have value stored in ctp*)
	    value.intval:= values.ival;
	    addr.wordoffset:= ord( address(value.intval) );
	  end
	  else begin				(* all others have ptr to constant *)
	    constp := deref$csp (values.valp);
	    value := constp^;
	    if value.cclass in [strd, strg]
	      then begin
		addr.wordoffset := ord (address (value.sval));
		desc.kind := fstring_dt;
	      end
	      else addr.wordoffset := ord (address (value.intval));
	  end;
	addr.packedflag := false;
	cstvalue := true;
        return;
      end;

    vars:
      begin
	valueparm := (vclass = spvalparmsc);
	cstvalue := vconst;
	addr := get_address (idctp, d);
      end;

    field:
      addr := get_field_addr (idctp, addr);

    func:
      begin
	if not func_lookup (idptr, d) then begin
	  status := not_variable;
	  return;
	end;
	addr := get_address (idctp, d);
      end;

    others:
      begin
	status := not_variable;
	return;
      end

  end (* case *) ;
  if addr.wordoffset = ord (nil) then begin
    status := not_addressible;
    return;
  e
  while (cur$sy in [lbrack, arrow, period]) and (status = success )  do begin
    set$kind (desc, idtypeptr);
    case cur$sy of


      lbrack: begin
	if not (kind in [array_dt, fstring_dt, vstring_dt]) then
	  status := not_array_string
	else
	  loop
	    in$symbol;				(* get index *)
	    scalar_value (index, status);
	    if status = success then begin
	      if kind = array_dt then begin
		status := check$type (index, inx);
		if status = success then begin
		  idstp := deref$stp(idtypeptr);
		  addr := get_element_addr(idstp,addr,index.value.intval,status);
		  idtypeptr := ael;
		end;
	      end
	      else begin			(* kind = vstring_dt or fstring_dt *)
		if index.kind <> int_dt then
		  status := ixtype_wrong
		else begin
		  addr := get_substr_addr(desc,index.value.intval,status);
		  idtypeptr := scope$stack.displays[1].prog_blk^.charpoint;
		end;				(* cur$sy = intconst *)
	      end;				(* kind = strings *)
	    end;				(* status = success *)
	  exit if (cur$sy <> comma) or (status <> success);
	    set$kind (desc, idtypeptr);
	  exit if not (kind in [array_dt, fstring_dt, vstring_dt]) do
	      status := too_many_subs;
	  end;
	if status = success then begin
	  if cur$sy <> rbrack then
	    status := right_bkt_expected
	  else
	    in$symbol;
	end;					(* status = success *)
      end;					(* lbrack *)

      period: begin
	if kind <> record_dt then
	  status := not_record
	else begin
	  in$symbol;
	  if cur$sy <> ident then
	    status := not_id
	  else begin
	    fld := pas$lookup(fst,cur$id,fldctp);
	    if fldctp = nil then
	      status := no_such_field
	    else begin
	      idtypeptr := fldctp^.idtype;
	      addr := get_field_addr(fldctp,addr);
	      in$symbol;
	    end;				(* fldctp <> nil *)
	  end;				(* cur$sy = ident *)
	end;					(* kind = records *)
      end;					(* period *)

      arrow: begin
	in$symbol;
	if not (kind in [file_dt, pointer_dt]) then
	  status := not_ptr_file
	else begin
	  if kind = file_dt then begin
	    indirectptr := ptr(addr.wordoffset);
	    addr.wordoffset := indirectptr^.addr;
	    addr.packedflag := false;
	    idtypeptr := fil;
	  end
	  else begin				(* kind = pointers *)
	    addr.wordoffset := extract_scalar (desc);
	    if (v$rtual andif (addr.wordoffset = v$nil)) orif (not v$rtual) andif (addr.wordoffset = ord (nil)) then
	      status := nil_value
	    else if addr.wordoffset = 0 then
	      status := ptr_uninitialized
	    else begin
	      addr.packedflag := false;
	      idtypeptr := el;
	    end;				(* if value *)
	  end;				(* kind = pointer *)
	end;					(* kind = pointers or files *)
      end					(* arrow *)

    end;					(* case *)
    set$kind (desc, idtypeptr);
  end;					(* while *)
 end (* with desc *) ;
end;						(* selector *)
$PAGE lhs$ref

public procedure lhs$ref ( var desc: descriptor;
			    var status: parse_error );

 begin
  selector (desc, status);
  if status = success then begin
    with desc do begin				(* check for variableness *)
      if valueparm
	then status := not_modifiable
      else if cstvalue
	then status := not_variable;
    end
  end
 end;
$PAGE rhs$value

public procedure rhs$value ( var desc: descriptor;
			     var status: parse_error );

 (* This returns the descriptor of an arbitrary value.  If the value is a scalar
    or pointer value, then it is placed in desc.value.intval regardless of
    whether it is a constant or a <reference>.  If the value is a varying
    string, a descriptor for an fstring is returned (the length is extracted
    to become the charsize, and the address is adjusted to point to the text of
    the string).  Note that in all cases (constant or <reference>), the addr
    is correct and points to the (faked) value.  Also, except if the value is
    a set or a string (where it is nil), the typtr is correct. *)

 var
   sign: symbol;
   elem: descriptor;
   lelem, idx, offset: integer;
   addr_overlay: packed record
     case boolean of
       true:  (full_addr: integer);
       false: (l_addr: addrrange; r_addr: addrrange)
   end;

begin
 status := success;
 with desc do begin
  sign := othersy;		(* process "+" or "-" expr *)
  if cur$sy in [plus, minus] then begin
    sign := cur$sy;
    in$symbol;
  end;

  valueparm := false;
  cstvalue := false;
  addr.wordoffset := ord (nil);
  addr.packedflag := false;
  typtr := ord (nil);

  if cur$sy = intconst then begin
    value.intval:= cur$val.intval;
    kind := int_dt;
    addr.wordoffset := ord (address (value.intval));
    typtr := scope$stack.displays[1].prog_blk^.intpoint;
    in$symbol;
  end

  else if cur$sy=realconst then begin
    value.rval:= cur$val.rval;
    kind:= real_dt;
    addr.wordoffset:= ord(address(value.rval));
    typtr:= scope$stack.displays[1].prog_blk^.realpoint;
    in$symbol;
  end

  else if cur$sy = stringconst then begin
    if cur$val.slgth = 1 then begin
      kind := char_dt;
      value.intval := ord (cur$val.sval[1]);
      addr.wordoffset := ord (address (value.intval));
      typtr := scope$stack.displays[1].prog_blk^.charpoint;
    end
    else begin
      kind := fstring_dt;
      charsize := cur$val.slgth;
      value := cur$val;
      addr.wordoffset := ord (address (value.sval));
    end;
    in$symbol;
  end

  else if cur$sy = lbrack then begin		(* verily, a set constant ! *)
    in$symbol;
    kind := set_dt;				(* create a cell for it *)
    sel := ord (nil);				(* element type - until we see first element *)
    value.cclass := pset;
    value.pval := [];
    addr.wordoffset := ord (address (value.pval));
    if cur$sy <> rbrack then loop	(* get set elements *)
      scalar_value (elem, status);
      if status <> success then return;
      if sel = ord (nil)
	then begin			(* get element type from 1st element *)
	  sel := elem.typtr;
	  if elem.kind = char_dt
	    then offset := ord (' ')
	    else offset := 0;
	end
	else begin
	  status := check$type (elem, sel);
	  if status <> success then return;
	end;
      lelem := elem.value.intval;
      if cur$sy = elipsis then begin	(* have '..' <value> *)
	in$symbol;
	scalar_value (elem, status);
	if status <> success then return;
	status := check$type (elem, sel);
	if status <> success then return;
      end;
      if lelem > elem.value.intval then begin
	status := bad_set;
	return;
      end;
      for idx := lelem to elem.value.intval do
	value.pval := value.pval + [idx - offset];
    exit if cur$sy <> comma;
      in$symbol;
    end (* loop *) ;
    if cur$sy <> rbrack then begin
      status := right_bkt_expected;
      return;
    end;
    in$symbol;
  end

  else if cur$sy <> ident then begin
    status := not_scalar;
    return;
  end

  else (* if cur$sy = ident then *) begin
    selector (desc, status);
    if status <> success then return;

    (* Note that below we assume that there is no difference between konsts,
       public constants entered as vars, and real vars.  This assumption is
       okay because selector always creates an address for konst (pointing
       at the value in the descriptor). *)

    case kind of

      int_dt, char_dt, scalar_dt, real_dt, pointer_dt:	(* fetch the value into full word *)
	begin
	  value.intval := extract_scalar (desc); (*note that rval and intval are equated*)
	  addr.wordoffset := ord (address (value.intval));
	  addr.packedflag := false;
	end;

      vstring_dt:				(* turn into a fixed length string *)
	begin
	  charsize := extract_scalar (desc);	(* get length word *)
	  if v$rtual then with addr_overlay do begin

	  (* assure areaid removed from address *)

	    full_addr := vtran$ (addr.wordoffset+1);
	    l_addr := 0;
	    addr.wordoffset := full_addr;
	  end
	  else addr.wordoffset := addr.wordoffset + 1;	(* point at string *)
	  kind := fstring_dt;
	  typtr := ord (nil);
	  addr.packedflag := false;
	end;

      fstring_dt:
	typtr := ord (nil)				(* for compatibility with string constants *)

    end (* case *) ;
  end;

  if kind = int_dt then begin
    if sign = minus then value.intval := - value.intval
  end
  else if kind = real_dt then begin
    if sign = minus then value.rval := -value.rval
  end
  else begin
    if sign <> othersy then status := wrong_type
  end;
 end (* with desc *) ;
end;
$PAGE scalar_value

procedure scalar_value
	(* var scalar: descriptor;
	   var status: parse_error *);

 (* This returns a descriptor for a scalar value.  If status = success on return,
    then the kind of the value is either int_dt, char_dt, or scalar_dt and the
    ordinal value of the scalar is in scalar.value.intval.  If status <> success,
    the value is not a simple scalar. *)

 begin
  rhs$value (scalar, status);
  if status <> success then return;

  if not (scalar.kind in [int_dt, char_dt, scalar_dt])
    then status := not_scalar;
 end;
$PAGE compdescs

(* This function does what its counterpart in the compiler (comptypes) does.
   It differs in the fact that it works on descriptors instead of stp records.
   There are two reasons for this difference: (1) Constant string and sets
   do not have stp's and we do not want to generate them, and (2) extracting
   the information in the stp's into descriptors first, avoids problems with
   deref$stp and friends. *)

function compdescs (left, right: descriptor): boolean;

  function comp (leftstp, rightstp: intstp): boolean;
   var l, r: descriptor;			(* for comparing subtypes *)
   begin
    comp := true;				(* okay if subtypes are nil *)
    if leftstp = ord (nil) then return;
    if rightstp = ord (nil) then return;
    set$kind (l, leftstp); set$kind (r, rightstp);
    comp := compdescs (l, r)
   end;

 begin
  (* First check if they are identical stps, if so the types are certainly
     compatible.  Note, it is assumed that the type ptrs (intstp's) are
     relative to the same module. *)

  compdescs := (left.typtr = right.typtr);
  if compdescs then return;

  (* The stp's are not the same (or if one is a set or string, the type may
     be nil), therefore it is necessary to compare the declaration of the 
     types. *)

  compdescs := (left.kind = right.kind);	(* kinds must be same *)
  if not compdescs then return;

  case left.kind of

    scalar_dt:
      compdescs := (left.basetype = right.basetype);

    array_dt, record_dt:
      compdescs := false;		(* types must be identically the same -- this is
					   a stronger condition than the compiler requires,
					   but is reasonable for debugger, as only
					   assignments are performed. *)

    vstring_dt, fstring_dt:
      compdescs := left.charsize = right.charsize;

    pointer_dt:
      compdescs := comp (left.el, right.el);

    file_dt:			(* really irrelevant as we don't access files directly *)
      compdescs := comp (left.fil, right.fil);

    set_dt:
      compdescs := comp (left.sel, right.sel)

  end (* case *) ;
 end;
$PAGE check$type

public function check$type
	(* item: descriptor;
	   required_type: intstp ): parse_error *);

 (* This function determines whether or not the item specified is compatible
    with (i.e. can be assigned to) the specified type.  It is used to in
    assignment, and checking the type of an array index.  The error code 
    returned is "success" if the item is compatible, otherwise the value 
    indicates the nature of the problem. *)

 var
  required_stp: stp;
  testdesc: descriptor;

 begin
  check$type := success;			(* assume okay *)
  set$kind (testdesc, required_type);
  if not (compdescs (item, testdesc) or
	    ([item.kind, testdesc.kind] <= [char_dt, fstring_dt, vstring_dt]))
    then check$type := wrong_type
  else (* types okay *) begin
    if testdesc.kind in [scalar_dt, int_dt, char_dt] then begin	(* do range check *)
      if item.value.intval < testdesc.minval
	then check$type := low_scalar
      else if item.value.intval > testdesc.maxval
	then check$type := high_scalar;
    end;
  end;
 end.
    4@Cœ