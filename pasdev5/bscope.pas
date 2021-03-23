$LENGTH 43
$OPTIONS special,notrace,nocheck

module bscop$;

(* scope$stack -- contains routines for establishing scope$stack in pascal debugger.
   entry points include:
   stack$scope -- build scope$stack of passed stack frame.
   frame$scope -- build scope$stack of stack frame whose index is passed.
   mod$scope -- build scope$stack from list of nested procedure names,
      where the first name is the enclosing module.
   ext$scope -- build scope$stack from list of nested procedure names,
      where the first name is externally defined.
   pas$lookup -- search passed idtree for occurance of passed name.
   dumpscope -- debugging routine to display vector returned by xxx_scope
      routines. *)
$INCLUDE debtyp.inc
$INCLUDE debrun.inc
$INCLUDE debio.inc
$PAGE

external function deref$ctp(ptr:integer):ctp;

external procedure st$open(pb:progblkptr);

(* some global data and types *)


const
    level1ctp = 0;  (* block1,word0 of .deb file is first ctp *)
    entrysize = 2;  (* size of link symboltable entries *)



type
    linkheadtype = packed record
      linkstsize: half_word;	(* neg count of # wds in link st*)
      firstsymbol:linkentryptr
    end;

var
    linkhead : ^linkheadtype;	(* indirectly setup by first$module and used by next$module *)


$PAGE routine to look up a symbol in pascal symboltable

public function pas$lookup(idtree:intctp; symname:alfa; var symid:ctp)
  :intctp;

begin
  pas$lookup := idtree;
  symid := deref$ctp(idtree);
  while (symid <> nil) andif (symid^.name <> symname) do begin
    with symid^ do
      if symname < name then
	pas$lookup := llink
      else
	pas$lookup := rlink;
    symid := deref$ctp(pas$lookup);
  end;		    (* while *)
end;		    (*pas$lookup *)
$PAGE routine to convert ascii name to radix 50 and vice versa

const
    r50period = 45b;
    r50dollar = 46b;
    r50percent = 47b;
    linknamesize = 6;

type
    r50chtype = 0..47b;
    lname = string[linknamesize];

public function r50$asc(r50name:r50word):alfa;


  function convert(int:integer):lname;


    function convert_ch(r50ch:r50chtype): lname;

    begin
      case r50ch of
	0:
	  convert_ch:= '?';
	1..12b:
	  convert_ch:= chr(r50ch+57b);
	13b..44b:
	  convert_ch:= chr(r50ch+66b);
	r50period:
	  convert_ch:= '.';
	r50dollar:
	  convert_ch:= '$';
	r50percent:
	  convert_ch:= '_'  (*actually a percent, but underbar in pascal*)
      end	    (*case*)
    end (*convert_ch*);

  begin		    (*convert*)
    if (int div 50b) <> 0 then
      convert:= convert(int div 50b)
    else
      convert:= '';
    convert:= convert || convert_ch(int mod 50b)
  end (*convert*);

begin		    (*r50$asc*)
  r50$asc:= convert(r50name)
end (*r50$asc*);

function asctor50(ascname:alfa) : r50word;

var
    i: 1..linknamesize;
    cur$ch: char;
    r50ch:r50chtype;	(* range for r50 char *)

begin
  asctor50 := 0;
  for i := 1 to linknamesize do begin	(* only look at first 6 chars *)
    cur$ch := ascname[i];
  exit if cur$ch = ' ';
    asctor50 := asctor50 * 50b;
    case cur$ch of
      'a'..'z':
	r50ch := ord(cur$ch) - 126b;
      'A'..'Z':
	r50ch := ord(cur$ch) - 66b;
      '0'..'9':
	r50ch := ord(cur$ch) - 57b;
      '.' :
	r50ch := r50period;
      '$' :
	r50ch := r50dollar;
      '_' :
	r50ch := r50percent;	(* converts to percent *)
      others :
	r50ch := 0  (* includes blank *)
    end;	    (* case *)
    asctor50 := asctor50 + r50ch;
  end;		    (* for *)
end;		    (* asctor50 *)


$PAGE mod$lookup -- look up module in link symbol table

public function first$module:linkentryptr;

(* this routine initializes first$module to point to the first *)
(* (actually the last symbol) in the link symboltable. it also *)
(* initializes the global var linkhead *)


const
    jbsym = 116b;

begin
  linkhead := ptr(jbsym);
  with linkhead^ do
    if linkstsize = 0 then
      first$module := nil
    else
      first$module := ptr(ord(firstsymbol) + (1000000b - linkstsize) - entrysize)
	;
end;		    (* first$module *)


public function next$module(curmodule:linkentryptr):linkentryptr;

(* this routine returns a pointer to the next module entry or *)
(* nil if we reach the end of the chain, or if a modules st size*)
(* = 0 *)


begin
  with curmodule^ do
    if stsize = 0 then
      next$module := nil
    else begin
      next$module := ptr(ord(curmodule) - (1000000b - stsize));
      if ord(next$module) < ord(linkhead^.firstsymbol) then
	next$module := nil;
    end;
end;		    (* next$module *)


public function mod$lookup(name:alfa):linkentryptr;

(* this routine searches through module entries in the link *)
(* symboltable for name. nil is returned if name is not found *)



var
    r50name:r50word;

begin
  mod$lookup := first$module;
  r50name := asctor50(name);
  while mod$lookup <> nil do
    with mod$lookup^ do begin
      if stype = modtype then begin
	if modname = r50name then
	  return;
	mod$lookup := next$module(mod$lookup);
      end ;
    end;	    (* with *)
end;		    (* mod$lookup *)

$PAGE sym$lookup -- look up external symbol in link symbol table

public function sym$lookup(name:alfa;var modname:alfa):linkentryptr;

(* this routine searches the link symboltable for the specified symbol *)
(* and returns the address of that symbol table entry if found, otherwise it *)
(* returns nil. if modname = null string, the entire st is searched, *)
(* otherwise the module is searched first, then that section is searchd *)
(* for the symbol and the name of the module found in is returned*)



var
    modptr : linkentryptr;


  function search_module(name:alfa; curmodule:linkentryptr):linkentryptr;

  (* this routine searches the section of the link symboltable *)
  (* which contains symbols for that module, for name specified.*)
  (* if found core address of name is returned, else zero is returned *)



  var
      r50name:r50word;

  begin		    (* search_module *)
    search_module := next$module(curmodule);
    if search_module <> nil then begin
      search_module := ptr (ord(search_module) + entrysize);
      r50name := asctor50(name);
      while (search_module <> curmodule) do begin
	with search_module^ do
	  if stype <> modtype then begin
	    if symname = r50name then
	      return
	    else
	      search_module := ptr(ord(search_module) + entrysize)
	  end ;
      end;	    (*while *)
      search_module := nil;
    end;
  end;		    (* search_module *)


begin		    (* sym$lookup *)
  if modname <> '' then
    sym$lookup := search_module(name,mod$lookup(modname))
  else begin
    modptr := first$module;
    while modptr <> nil do begin
      sym$lookup := search_module(name,modptr);
    exit if sym$lookup <> nil;
      modptr := next$module(modptr);
    end;	    (* loop *)
    if sym$lookup <> nil then
      modname := r50$asc(modptr^.modname);
  end;		    (* if *)
end;		    (* sym$lookup *)
$PAGE find$module -- find module name given program block

public function find$module(pb: progblkptr): alfa;

(* routine searches link symbol table for pascal module whose program
   block address is the same as that passed.  if no such module is found,
   a blank name is returned. *)


var
    lnkptr: linkentryptr;

begin
  lnkptr:= first$module;    (*get entry of first module in link st*)
  while lnkptr<> nil do begin	(*loop through modules*)
  exit if ( (lnkptr^.modname mod 1000000b) = lnkptr^.firstword^.right_r50 )
      andif ( lnkptr^.firstword^.prog_blk = pb );   (*is pascal and pb matches*)
    lnkptr:= next$module(lnkptr)
  end;
  if lnkptr=nil then
    find$module:= ''
  else
    find$module:= r50$asc(lnkptr^.modname)  (*convert radix-50 name*)
end;
$PAGE routines to follow caller and parent chain back one

public function call$basis(stkframe:stkframeptr):stkframeptr;

begin
  if ord(stkframe^.caller_basis) = 0 then
    call$basis := nil	(* caller is mainline !!! *)
  else
    call$basis := stkframe^.caller_basis;
end;		    (* call$basis *)


function parent_basis(stkframe:stkframeptr):stkframeptr;

var
    procblk:procblkptr;

begin
  parent_basis := nil;
  procblk := stkframe^.link_addr^.proc_blk;
  if (procblk <> nil) andif (procblk^.pflev >=2) then
    parent_basis := stkframe^.parent_basis;
end;		    (*parent_basis *)

$PAGE open_module

function open_module (pb:progblkptr):display;

(* create a display for level 0 module, identified by its *)
(* program block *)


begin		    (* open_module *)
  with open_module do begin
    prog_blk := pb;
    stackbase := nil;
    proc_ptr := ord(nil);
    if pb <> nil then
      staticbase := pb^.lowseg_base;
    idtree := level1ctp;    (* the first st entry *)
  end;		    (* with *)
end;		    (* open_module *)

$PAGE build scope$stack for specified stackframe

public function stack$scope (stkframe:stkframeptr;var scope$stack:scope_type)
  :scopereturn;

  (* assumes stkframe is not for main; wont return notpascal. *)
  (* builds displays for scope$stack to reflect state of world from *)
  (* the stack frame specified *)


var
    procid: ctp;
    d:display;
    procblk:procblkptr;
    dl:displaylevel;


  function open_stack_frame(stkframe:stkframeptr):display;

  (* fill in as much of the display as possible for the *)
  (* stkframe passed *)


  var
      procid:ctp;
      procblk:procblkptr;

  begin		    (* open_stack_frame *)
    with open_stack_frame do begin
      procblk := stkframe^.link_addr^.proc_blk;
      prog_blk := procblk^.prog_blk;
      proc_ptr := procblk^.proc_ptr;
      stackbase := stkframe;
      if prog_blk=nil then (*not in debug mode*)
	idtree := ord(nil)
      else
	begin
	  procid:= deref$ctp(proc_ptr);
	  if procid<>nil then idtree:= procid^.next
	  else idtree:= ord(nil)
	end;
      if prog_blk = nil then
	staticbase := 0
      else
	staticbase := prog_blk^.lowseg_base;
    end;	    (* with *)
  end;		    (* open_stack_frame *)



  function open_parent(child:display): display;

  (* create a display for the parent of an active procedure or func *)



  var
      stkframe:stkframeptr;

  begin		    (* open_parent *)
    with child do begin
      stkframe := parent_basis(stackbase);
      if stkframe = nil then
	open_parent := open_module(prog_blk)
      else
	open_parent := open_stack_frame(stkframe);
    end;	    (* with *)
  end;		    (* open_parent *)


begin		    (* stack$scope *)
  stack$scope := noscope;
  procblk := stkframe^.link_addr^.proc_blk;
  if procblk = nil then
    return;
  if procblk^.prog_blk = nil then
    stack$scope := notdebug
  else begin
    st$open(procblk^.prog_blk);
    stack$scope := debug;
  end;
  with scope$stack do begin
    d := open_stack_frame(stkframe);
    display_levels := procblk^.pflev + 1;
    displays[display_levels] := d;
    for dl := display_levels - 1 downto 1 do
      displays[dl] := open_parent(displays[dl + 1]);
  end;		    (* with *)
end;		    (* stack$scope *)


public function frame$scope(var n:stklevel;var scope$stack:scope_type):scopereturn;

(* open scope$stack for stack frame n, where 1 is the first frame *)
(* on the stack, and basis points to the last one *)
(* if n is > last frame on the stack then n is set to the number of *)
(* the frame opened, ie the last frame *)


var
    stkframe:stkframeptr;
    lastframe,frame:stklevel;
    procblk:procblkptr;
    procblklk:^procblklink;

begin		    (* frame$scope *)
  if (ba$i$.mainflag = 0)  or (n = 1) then begin
    frame$scope := noscope;
    if ba$i$.mainflag = 0
      then stkframe := ba$i$.basis  (* have main's frame immediately *)
      else begin    (* must search up stack for it *)
	stkframe := ba$i$.basis;
	while call$basis (stkframe) <> nil do
	  stkframe := call$basis (stkframe);	(* strframe now points to level 2 frame *)
	stkframe := stkframe^.parent_basis; (* left in rh *)
      end;
    procblklk := ptr(stkframe^.rtn_addr);
    procblk := procblklk^.proc_blk;
    if procblk = nil then
      return;
    if procblk^.prog_blk = nil then
      frame$scope := notdebug
    else begin
      frame$scope := debug;
      st$open(procblk^.prog_blk);
      with scope$stack do begin
	display_levels := 1;
	displays[1] := open_module(procblk^.prog_blk);
	n := 1;
      end;	    (* with *)
    end;
  end
  else begin
    stkframe := ba$i$.basis;
    lastframe := 1; (* when caller is main, callerbasis = nil *)
    while stkframe <> nil   (* derive lastframe number *)
    do begin
      lastframe := lastframe + 1;
      stkframe := call$basis(stkframe);
    end;	    (* while *)

    if n > lastframe then n := lastframe;   (* open last possible frame *)
    stkframe := ba$i$.basis;
    for frame := 1 to lastframe - n do
      stkframe := call$basis(stkframe);
    frame$scope := stack$scope(stkframe,scope$stack);
  end;		    (* if *)
end;		    (* frame$scope *)
$PAGE mod$scope

(* routines mod$scope and ext$scope open a scope$stack from a list of names.
   in the former case, the first name is that of a module and subsequent ones
   are nested routines in the module.  in the latter, the first name is a
   public pascal procedure or function, and the remaining names are nested
   local routines of it.  external_scope merely locates the first routine's
   module name and calls mod$scope.  parameters of both routines are:

   names -- a name_list containing the names from outermost to innermost.
   invocation -- the desired invocation of the innermost routine for which
      scope$stack is to be built.  a value of 1 is the invocation closest to the top
      (the most recent) of the stack, 2, the next closest, etc.
   scope -- the array of display entries to be built.

   the function value indicates the result, as follows:
   = notdefined ... the first name is not a module, or external
      procedure/function.
   = notpascal ... the first name is not a pascal module,
      procedure, or function as indicated by the call.
   = notdebug ... the module is not compiled in debug mode, and hence, no scope$stack
      can be built for it.
   = badnest ... the names are not properly nested routines.  this is also
      returned if a name is found but is not a procedure/function.  on this
      return, the name_count in names is set to the index of the offending
      name.
   = wronginvocation ... the desired invocation of the innermost routine could
      not be found on the stack.  on this return, invocation is set to the
      index of the invocation found, and name_count in names is set to the
      index of the routine whose invocation was used.  the algorithm for finding
      an invocation is as follows:
	(1) try to find the desired invocation of the innermost routine.
	(2) if this can not be found, use the deepest (earliest) invocation
	    of the routine.
	(3) if no invocation exists, repeat (1) and (2) with all routine
	    names, moving outwards in scope$stack.
	(4) if no invocation of any routine is found, set invocation to zero.
	    in this case, stackbase of all display entries will be nil. *)


public function mod$scope( var names: name_list; var invocation: stklevel;
  var scope$stack: scope_type): scopereturn;
$PAGE open_named_module

  (*routine to open the module's symbol table and set up its display.
    clearly, this routine must be called before any calls to deref_xxx,
    i.e., attempts to access the symbol table, are made.

    parameters are:

      module_name ... the module name from the names vector.
      status ... return code, possible values = (notpascal,notdebug,debug (*cool*))

    function value is display entry on debug return *)



  function open_named_module(module_name: alfa; var status: scopereturn)
    : display;

  var
      mod_st_entry: linkentryptr;

  begin
    mod_st_entry:= mod$lookup(module_name); (*get link symbol table entry*)
    if mod_st_entry=nil then
      status:= notdefined   (*no such module name found*)
    else
      with mod_st_entry^ do

      (*pascal module has right halfword of radix-50 name in left half
	of word at address of module to allow pascal modules to be recognized*)

	if (modname mod 1000000b) <> firstword^.right_r50 then
	  status:= notpascal
	else if firstword^.prog_blk=nil then
	  status:= notdebug (*or at least not debug*)
	else
	  with firstword^ do begin
	    open_named_module:= open_module(prog_blk);	(*fill in display entry*)
	    st$open(prog_blk);	(*open the symbol table itself*)
	    status:= debug  (*indicate things are hunky-dory*)
	  end
  end (*open_named_module*);
$PAGE open_kids

  (*routine to open the nested routines within a module.
    parameter indicates status of operation, possible values = (badnest,debug).
    routine uses parameters names and scope of module_scope, filling in scope,
    and on the badnest return, setting name_count in names to index of crummy
    name. *)



  procedure open_kids(var status: scopereturn);


  (*local routine to set up the display entry for a passed name.  the display
    entry for its parent is passed, so that a search of the routines defined
    within its parent can be made.  note that the idtree of the parent is
    searched for level 1 routines, while the pflower chain is searched for
    local routines.  the status parameter of open_kids is set to badnest
    when the name cannot be found in the symbol table.*)



    function open_child(parent: display; name: alfa): display;

    var
	namectp: ctp;

    begin
      with open_child do begin
	stackbase:= nil;    (*to be filled in later (maybe)*)
	prog_blk:= parent.prog_blk; (*children (we assume) are in parent's program*)
	staticbase:= parent.staticbase;	(*all statics in program allocated off same base*)

	(*now find the symbol table entry. if parent is main (proc_ptr=nil),
		  then search idtree, else search procedure/function chain.*)

	if parent.proc_ptr=ord(nil) then begin
	  proc_ptr:= pas$lookup(parent.idtree,name,namectp);
	  if (namectp=nil) orif	(*make sure this name passes inspection* not (namectp^.klass in [proc,func])	(*if not routine name*)
	  orif (namectp^.pfkind=formal)	(*if "routine" is parameter*)
	  orif (namectp^.pfclass=externalsc) (*"routine" is external reference*)
	  then
	    status:= badnest	(*we don't like it*)
	  else begin	(*we like it*)
	    idtree:= namectp^.next; (*fill in display symbol table ptr*)
	    status:= debug  (*and indicate success*)
	  end
	end	    (*parent is mainline*)
	else begin  (*parent is routine -- search chain*)
	  namectp:= deref$ctp(parent.proc_ptr);	(*acquire parent's ctp record*)
	  proc_ptr:= namectp^.pflower;	(*we assume parent has ctp record*)
	  status:= badnest; (*set for chain search*)
	  while (proc_ptr<>ord(nil)) and (status=badnest) do begin  (*search chain*)
	    namectp:= deref$ctp(proc_ptr);
	    if namectp^.name=name then begin
	      status:= debug;
	      idtree:= namectp^.next
	    end
	    else
	      proc_ptr:= namectp^.pfchain
	  end
	end	    (*parent is a routine*)
      end	    (*with open_child*)
    end (*open_child*);

  var
      i: displaylevel;

  begin		    (*open_kids*)
    status:= debug;
    for i:= 2 to names.name_count do begin
      scope$stack.displays[i]:= open_child(scope$stack.displays[i-1],names.name[i]);
    exit if status<>debug do
	names.name_count:= i	(*remember bogus name*)
    end
  end (*open_kids*);
$PAGE subr_invocation

  (*routine to find the desired invocation of a passed routine.  if desired
    invocation cannot be found, deepest invocation is returned.  if no
    invocation is found, the function value is nil.
    parameters are:

    subr -- the (undereferenced) pointer to the routine's symbol table entry.
    pb -- a pointer to the block of the program containing the routine.  this
      value along with subr serve to uniquely identify the routine.
    invoc_found -- returned index of the invocation found (if one was).

    the function value is the stack frame pointer of the invocation found, or
    nil if none was found.*)



  function subr_invocation(subr: intctp; pb: progblkptr;
    var invoc_found: stklevel): stkframeptr;

  var
      stkframe: stkframeptr;

  begin
    invoc_found:= 0;	(*initialize for loop*)
    subr_invocation:= nil;
    if ba$i$.mainflag <> 0 then begin	(*if only main on stack, don't bother*)
      stkframe:= ba$i$.basis;	(*fetch most recent stack frame*)
      while (invoc_found<invocation) and (stkframe<>nil) do begin
	with stkframe^, link_addr^ do
	  if proc_blk<>nil then
	    with proc_blk^ do	(*now looking at procedure block*)
	      if (prog_blk=pb) and (proc_ptr=subr) then begin
		invoc_found:= invoc_found+1;
		subr_invocation:= stkframe
	      end;
	stkframe:= call$basis(stkframe)	(*move up the stack*)
      end	    (*while search*)
    end		    (*if more than main on stack*)
  end (*subr_invocation*);
$PAGE begin mod$scope

var		    (*local variables of mod$scope*)
    invoc_found: stklevel;
    stkframe: stkframeptr;
    i (*ubsequeous loop index*), rtn_invoked: displaylevel;

begin		    (*mod$scope*)

(*open the module heading the list*)

  scope$stack.displays[1]:= open_named_module(names.name[1],mod$scope);
  if mod$scope=debug then begin	(*got module open successfully*)
    open_kids(mod$scope);
    if mod$scope=debug then begin   (*all names were kosher routines*)

    (*loop through each routine from inner to outer, looking for an invocation*)

      rtn_invoked:= names.name_count;
      scope$stack.display_levels:= rtn_invoked;	(*set count of displays filled*)
      stkframe:= nil;
      while (rtn_invoked>=2) and (stkframe=nil) do begin
	with scope$stack.displays[rtn_invoked] do
	  stkframe:= subr_invocation(proc_ptr,prog_blk,invoc_found);
	if stkframe=nil then
	  rtn_invoked:= rtn_invoked-1
      end;
      if stkframe=nil then begin    (*nary an invocation found*)
	invocation:= 0;
	mod$scope:= wronginvocation
      end
      else begin
	if (rtn_invoked=names.name_count) and (invocation=invoc_found) then
	  mod$scope:= debug (*found exact routine and invocation*)
	else begin
	  mod$scope:= wronginvocation;
	  invocation:= invoc_found;
	  names.name_count:= rtn_invoked
	end;
	scope$stack.displays[rtn_invoked].stackbase:= stkframe;	(*fill in stack frames*)
	for i:= rtn_invoked-1 downto 2 do
	  scope$stack.displays[i].stackbase:= parent_basis(scope$stack.displays[i+1].
	    stackbase)
      end
    end
  end
end (*mod$scope*);
$PAGE ext$scope

public function ext$scope( var names: name_list; var invocation: stklevel;
  var scope$stack: scope_type): scopereturn;

  var mod_and_names: name_list; i: displaylevel;

begin
  mod_and_names.name[1] := '';	(* as required by symbol lookup *)
  if sym$lookup(names.name[1],mod_and_names.name[1]) = nil then
    ext$scope:= notdefined
  else begin
    for i:= 1 to names.name_count do	(*append rtn names after module name*)
      mod_and_names.name[i+1]:= names.name[i];
    mod_and_names.name_count:= names.name_count+1;
    ext$scope:= mod$scope(mod_and_names,invocation,scope$stack);
    if ext$scope in [badnest,wronginvocation] then
      names.name_count:= mod_and_names.name_count-1
  end
end (*ext$scope*).
  f3_Mš