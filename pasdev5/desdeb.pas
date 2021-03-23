$title DEBUG$ - the PASCAL Debugger
$options special,nocheck,notrace

(************************************************************
  *							    *
  *							    *
  *              P A S C A L   D e b u g g e r	            *
  *							    *
  *			M a i n l i n e			    *
  *							    *
  *							    *
  ************************************************************)


module debug$;

const
  version_id: string := 'Version 1.1';
$PAGE external declarations and includes

$include debtyp.inc
$include debrun.inc
$include debio.inc
const uuo_maxac = 1;			(* needed for douuo *)
$include douuo.inc

external procedure dump;	(* requested addition for DESIGN, 6/14/79 *)

external procedure in$symbol;			(* following vars set by in$symbol call *)

external var	cur$id: alfa;
		cur$val: constant;
		cur$sy: symbol;

external function stack$scope
		(  basis: stkframeptr;
		   var scope$stack: scope_type  ): scopereturn;

external function frame$scope
		(  var n: stklevel;
		   var scope$stack: scope_type  ): scopereturn;

external function mod$scope
		(  var names: name_list;
		   var n: stklevel;
		   var scope$stack: scope_type  ): scopereturn;

external function ext$scope
		(  var names: name_list;
		   var n: stklevel;
		   var scope$stack: scope_type  ): scopereturn;

external function find$block
		( name: alfa;
		  var err: scopereturn ): progblkptr;

external function find$file
		( pb: progblkptr;
		  name: alfa;
		  number: integer ): fileblkptr;

external function find$page
		( fb: fileblkptr;
		  name: alfa;
		  number: integer ): pageblkptr;

external function find$stmt
		( pb: pageblkptr;
		  number: integer ): stmtblkptr;

external procedure info$stmt
		( sb: stmtblkptr;
		  var info: source_id );

type brknumtype = 0..maxbrkpt;

external function set$brkpt
		( sb: stmtblkptr;
		  var brknumber: brknumtype ): boolean;

external function clr$brkpt
		(  brknumber: brknumtype  ): boolean;

external procedure clr$all$brkpt;

external procedure dmp$scope;

external procedure dmp$files
		( pb: progblkptr );

external procedure dmp$pages
		( fb: fileblkptr );

external procedure dmp$modules;

external procedure dmp$breaks;

external procedure dmp$stack 
		( levels: stklevel );

external procedure dmp$location;

external procedure lhs$ref
		( var desc: descriptor;
		  var status: parse_error );

external procedure rhs$value
		( var desc: descriptor;
		  var status: parse_error    );

external function check$type
		( item: descriptor;
		  required_type: intstp ): parse_error;

external procedure store$scalar
		( desc: descriptor;
		  value: integer    );

external procedure copy$words
		( dest: addrrange;
		  source: addrrange;
		  wd_cnt: integer    );

external function deref$stp ( intstp ): stp;

external function named$const
		( cst: constant;
		  iptr: intstp   ): alfa;

external procedure set$kind
		( var desc: descriptor;
		  iptr: intstp		);

external procedure st$open(pb: progblkptr);

external function sym$lookup(name: alfa; var modname: alfa): linkentryptr;

external const v$rtual: boolean;	(* true if virtual program *)

external function vtran$ (integer): integer;	(* translate to real address *)

external const v$nil: integer;		(* virtual NIL *)
$PAGE public vars, prompt, next$char

public var
  scope$stack: scope_type;				(* used in other portions of debugger *)
  with$stack: with_type;
  cur$ch: char;					(* required by scanner, INSYMBOL *)


(* PROMPT is called to acquire a new command line.  It either discards the remainder
   of the previous line, if any, and reads the new line into "input_line", or,
   it the passed line is non-null, uses it as the new line.  The
   "cursor" is set to the first character of the line; this indicates the first
   character to be returned by "next$char" below.  Finally, it primes INSYMBOL
   to call next$char when in$symbol is next called. *)

const max_ll := 132;	(* reader variables *)
var
  input_line: string;
  cursor: 0..max_ll;
  first_cursor: 0..max_ll;		(* set by process_db_commands *)

procedure prompt(line: string);
 begin
  if length(line)=0 then begin
    read$nl;					(* get to the start of a line *)
    writ$str (' ?? ');
    input_line := '';
    repeat	(* read line, including <cr> into input_line *)
      read$ch (cur$ch);
      input_line := input_line || cur$ch;
    until at$eoln;
  end
  else begin (*set up passed line as new line*)
    at$eoln:= true;
    input_line:= line || chr(15b); (*which read$ch returns on end of line*)
  end;
  cursor := 1;					(* cause next$char to start at beginning *)
  cur$ch := ' ';					(* prime in$symbol *)
  cur$sy := eofsy;
 end;


(* NEXTCH returns the next character in the input line and advances the cursor
   to the next character if not at the end of the line.  Note: cursor may be
   saved in order to back up the parse. *)

public procedure next$char;
 begin
  cur$ch := input_line [cursor];
  if cursor < length (input_line)
    then cursor := cursor + 1;
 end;


(* Parallel table to brk$table (the table of stmtblkptrs of breakpoints shared
   with the run-time) containing strings to execute on a breakpoint *)

public var brk$strings: array[0..maxbrkpt] of string;
$PAGE debug_header, initdb, error, check_debug
(* DEBUG$ is the entry point into the debugger.  It is called directly from the
   runtime.  There are no parameters; all information has been placed in locations
   shared with the runtime. *)


public procedure debug$;

  label 100;					(* entry to command loop *)

  var scope_state: scopereturn;		(* error state of opened scope$stack *)



  (* INIT DB initializes all global variables required by the debugger system
     on the first call to the debugger. *)

  procedure init_db;
   begin
    clr$all$brkpt;				(* no break points are set *)
   end;



  (* ERROR prints out an error message and aborts the rest of the command line.
     The command loop is then reentered. *)

  procedure error ( s: message );
   begin
    writ$nl (s);
    goto 100;
   end;



  (* CHECK DEBUG should be called before all operations that require a valid
     display.  It checks scope_state to see that what we have opened is compiled
     in debug mode. *)

  procedure check_debug;
   begin
    if scope_state <> debug
      then error ('Not in debug mode.');
   end;
$PAGE check_status
(* CHECK STATUS examines a status code returned by the address package, and
   determines if the current operation can continue.  Acceptable error 
   conditions (such as not modifiable for where) are passed in a set.  These
   statuses plus "success" will cause check_status to return; otherwise, an
   error is signalled. *)

type status_set = set of parse_error;

procedure check_status (status: parse_error; okay: status_set);
 begin
  if status in okay then return;
  case status of
    success: return;
    ixtype_wrong:  error ('Index type wrong.');
    too_many_subs:  error ('Too many subscripts for array.');
    not_array_string:  error ('Subscript follows non-array/string.');
    not_record:  error ('Field follows non-record.');
    not_ptr_file:  error ('Non-pointer/file dereferenced.');
    no_such_field:  error ('Field not in record.');
    low_scalar:  error ('Scalar out of range.');
    high_scalar:  error ('Scalar out of range.');
    nil_value:  error ('Pointer value is NIL.');
    not_id:  error ('Identifier expected.');
    not_scalar:  error ('Scalar value expected.');
    not_modifiable:  error ('Cannot assign to this item.');
    not_defined:  error ('Identifier not declared.');
    right_bkt_expected:  error ('"]" expected.');
    not_addressible:  error ('Procedure containing local variable not active.');
    ptr_uninitialized:  error ('Pointer value is not initialized.');
    wrong_type:  error ('Type conflict of operands.');
    not_variable:	error ('Not a variable.');
    bad_set:	error ('Set elements out of order.');
    others:	error ('Error in reference.')	(* in case we missed one *)
  end (* case *) ;
 end;
$PAGE query
(* QUERY asks a question and waits for a yes (<cr>) or no reply.  This is a
   copy of the routine in CMDUTL, except that it uses the special I/O routines. *)

function query (question: message): boolean;

 procedure ask;
  begin
   writ$str (question || '?  ');
  end;

 var response: (good, bad);
 var line: string;
 var cur$ch: char;

 begin
   ask;						(* print question *)
   repeat
     read$nl;
     line := '';
     loop
       read$ch (cur$ch);
     exit if at$eoln;
       line := line || uppercase (cur$ch);
     end;

     response := good;				(* process, assume ok *)
     if (line = 'YES') or (line = 'Y') or (line = '')
	then query := true
     else if (line = 'NO') or (line = 'N')
	then query := false
     else if (line = 'REPEAT')
	then begin
	  ask;
	  response := bad
	end
     else
	begin
	  writ$str ('YES/NO?  ');
	  response := bad
	end
   until response = good
 end;
$PAGE fetch, check_end, check
(* FETCH checks that the current symbol belongs to a particular set of symbols.
   If so, it advances to the next symbol; otherwise, it errors and reenters the
   command loop. *)

type sym_set = set of symbol;

procedure fetch (okay: sym_set);
 begin
  if not (cur$sy in okay) then begin
    writ$nl ('Syntax error.');
    goto 100
  end;
  in$symbol;
 end;



(* CHECK END checks for the end of a command.  Both semicolon and eofsy (meaning
   end of line are accepted.  By convention, this never advances the current 
   token past the end of command. *)

procedure check_end;
 begin
  if not (cur$sy in [semicolon, eofsy]) then begin
    writ$nl ('Command not terminated.');
    goto 100;
  end
 end;



(* CHECK checks for a specified symbol.  If found, it returns true and advances
   to the next symbol; otherwise, it just returns false *)

function check (sym: symbol): boolean;
 begin
  check := (cur$sy = sym);
  if check then in$symbol
 end;
$PAGE mfpl_id
(* MFPL ID is used in parsing module, file, page, and line number designations.
   It accepts the following syntax:

       ( <identifier> | <intconst> ) [ '@' | '-' | '/' ]

   It returns (as global variables), "name" set to the <identifier> or '';
   "number" set to the value of the <intconst> or -1; and "delim" set to either
   atsign, minus, slash or othersy (for all other delimiters).  When exited
   the token points to the first following symbol. *)

var		(* global return values *)
  name: alfa;
  number: integer;
  delim: symbol;

procedure mfpl_id;
 begin
  name := ''; number := -1;		(* set defaults *)
  if cur$sy=ident	(* get leading <identifier> or <intconst> *)
    then begin name:= cur$id; in$symbol end
  else if cur$sy=intconst
    then begin number:= cur$val.intval; in$symbol end;

  if cur$sy in [atsign, minus, slash]
    then begin 					(* recognizable symbol *)
      delim := cur$sy;
      in$symbol
    end
    else delim := othersy;			(* for all others *)
 end;
$PAGE modulename
(* MODULENAME parses a module specification in a source address (i.e. <ident> '@').
   If not present, the current module is returned; if present, the module is
   looked up and returned.  An error is reported if the module selected is not
   in debug mode.  This exits with the information for the next MFPL syllable
   in name, number and delim. *)

function modulename(mustscan: boolean): progblkptr;
 var moderr: scopereturn;
 begin
  if mustscan then mfpl_id;
  if delim <> atsign
    then begin					(* not a module name, use current *)
      check_debug;				(* must be in debug mode *)
      modulename := scope$stack.displays[1].prog_blk;
    end
    else begin					(* user specified modulename *)
      if name = ''				(* must have had <intconst> '@' *)
	then error ('Invalid modulename name.');
      modulename := find$block (name, moderr);	(* look up the modulename *)
      if modulename = nil then begin
	if moderr = notdefined
	  then error ('Module not defined.')
	  else error ('Module not compiled in debug mode.');
      end;
      mfpl_id;					(* exit pointing to next syllable *)
    end;
 end;
$PAGE filename
(* FILENAME parses a file specification in a source address.  The syntax is:
   ( <identifier> | <intconst> ) [ '-' ].  The parameter file_last indicates
   that a file address is wanted, and that the "-" is not required.  If the
   file name is present, it is looked up; otherwise, the first (zeroth) file
   in the module is used.  It is assumed that mfpl_id has been called prior
   to entering this routine; on exit, if not file_last then we advance to 
   the next mfpl syllable. *)

function filename (pb: progblkptr; file_last: boolean): fileblkptr;


  procedure default;		(* get the default (i.e. 1st) file *)
   var testnum: integer;
   begin
    testnum := 0;		(* search for 1st file *)
    loop
      filename := find$file (pb, '', testnum);
    exit if filename <> nil;	(* assume that there is at least one file *)
      testnum := testnum + 1;
    end;
    if testnum <> 0 then begin	(* query user, if not file 0 *)
      writ$str ('File ');
      with filename^ do begin
	writ$int (file_number, decimal);
        if file_name <> '' then
	  writ$str (' (' || substr (file_name, 1, index (file_name, ' ', 11) - 1) || ')');
      end;
      if not query (' instead') then goto 100;
    end
   end;


 begin	(* file name *)
  if file_last then begin
    if delim <> othersy then error ('Invalid file address.');
    if (number = -1) and (name = '')
      then default
      else begin
	filename := find$file (pb, name, number);
	if filename = nil then error ('No such file.');
      end;
  end

  else (* not file_last *) begin
    if delim <> minus
      then default
      else begin				(* use file specified, and advance *)
	filename := find$file (pb, name, number);
	if filename = nil then error ('No such file.');
	mfpl_id;
      end
  end;
 end;
$PAGE pagename
(* PAGENAME parses a page number part of a source address (i.e. <page> '/' where
   <page> ::= <identifier> | <intconst>).  The page specification need not be
   present.  If it is present, it is looked up; otherwise, the 1st page of the
   file is returned.  If this page is not page zero, the user is queried.  This
   assumes that mfpl_id has been called prior to entry, and it exits with the
   mfpl_id called to fetch the next syllable. *)

function pagename (fb: fileblkptr): pageblkptr;
 var testnum: integer;
 begin
  if delim <> slash
    then begin					(* none specified, use default *)
      testnum := 0;				(* search for 1st page *)
      loop
        pagename := find$page (fb, '', testnum);
      exit if pagename <> nil;			(* assume there is at least one page in file *)
        testnum := testnum + 1;
      end;
      if testnum <> 0 then begin		(* query if 1st page not page 0 *)
	writ$str ('Page ');
	with pagename^ do begin
	  writ$int (page_number, decimal);
	  if subtitle <> '' then
	    writ$str (' (' || substr (subtitle, 1, index (subtitle, ' ', 11) - 1) || ')');
	end;
	if not query (' instead') then goto 100;
      end;
    end
    else begin
      pagename := find$page (fb, name, number);	(* get specified page *)
      if pagename = nil then error ('No such page.');
      mfpl_id;	(* leave pointing to next MFPL syllable *)
    end
 end;
$PAGE line_address
(* LINE ADDRESS parses a full source line address down to the line number,
   and returns the pointer to the statement block of the identified stmt. If
   there is no such line, the next available line is used if the user
   approves. *)

function line_address: stmtblkptr;
 var pb: pageblkptr; cur_info: source_id; moderr: scopereturn;
 begin
  (* Find page block.  If only line number given, and c$tmt defined, and
     current module whose scope is open contains c$tmt, then assume file
     and page containing c$tmt.  Else, go through normal procedure, which
     assumes page number = file number = 0 *)
  mfpl_id;
  if (delim=othersy) and (rea$on in [trap..intrp]) and (ord(c$tmt)<>0) then begin
    info$stmt(c$tmt,cur_info);
    if find$block(cur_info.mname,moderr)=scope$stack.displays[1].prog_blk then begin
      check_debug;  (*do this to catch nil=nil above*)
      with cur_info do
	pb:= find$page(find$file(scope$stack.displays[1].prog_blk,'',fnumber),
	  '', pnumber)
    end
    else pb:= pagename(filename(modulename(false),false))
  end
  else pb:= pagename(filename(modulename(false),false));

  (* the MFPL info now points to the line number (if any) *)

  if (delim <> othersy) or (number = -1)		(* some kind of error *)
    then error ('Invalid line address.');

  line_address := find$stmt (pb, number);		(* find nearest line *)
  with line_address^ do begin
    if line_number <> number then begin			(* not exact *)
      writ$str ('Line ');
      writ$int (line_number, decimal);
      if not query (' instead') then goto 100;
    end
  end
 end;
$PAGE word_list
type word_list =	(* includes both commands and other reserved words *)
      (	open_wrd, clear_wrd, display_wrd, step_wrd, sstep_wrd, stop_wrd, dump_wrd,
	exit_wrd, proceed_wrd, with_wrd, where_wrd, com_wrd, help_wrd, version_wrd,
	break_wrd, stack_wrd, loc_wrd, scope_wrd, files_wrd, pages_wrd,
	modules_wrd							  );

     command_wrds = open_wrd..break_wrd;
     display_opts = break_wrd..modules_wrd;

type wn_type = array[word_list] of record
		 name: packed array[1..10] of char;
		 abbrev: 1..10
	       end;
const words: wn_type :=
      (	( 'OPEN      ', 1 ),
	( 'CLEAR     ', 1 ),
	( 'DISPLAY   ', 1 ),
	( 'STEP      ', 1 ),
	( 'SSTEP     ', 1 ),
	( 'STOP      ', 4 ),
	( 'DUMDES    ', 3 ),
	( 'EXIT      ', 4 ),
	( 'PROCEED   ', 1 ),
	( 'WITH      ', 1 ),
	( 'WHERE     ', 2 ),
	( 'COMMANDS  ', 3 ),
	( 'HELP      ', 1 ),
	( 'VERSION   ', 3 ),
	( 'BREAKPOINT', 1 ),
	( 'STACK     ', 3 ),
	( 'LOCATION  ', 3 ),
	( 'SCOPE     ', 2 ),
	( 'FILES     ', 1 ),
	( 'PAGES     ', 1 ),
	( 'MODULES   ', 1 )  );
$PAGE keyword
(* KEYWORD parsesserved word symbol.  First it checks that the current token
   is an identifier; then, it looks up the identifier in the keyword table
   (allowing for abbreviations).  If a valid word is found, it advances to the
   next symbol; otherwise, an error is reported and the command line aborted. *)

function keyword: word_list;

 var l: 0..11;
     idx: word_list;

 begin
  if cur$sy <> ident			(* must be identifier to be keyword wrod *)
    then cur$id := '';				(* force no match in search below *)

  l := index (cur$id, ' ', 11) - 1;		(* get length of non-white chars *)
  for idx := minimum (word_list) to maximum (word_list) do	(* scan list *)
    if words[idx].abbrev <= l then 		(* must be longer than minimum *)
      if substr (words[idx].name, 1, l) = cur$id	(* must match to length of token *)
	then begin				(* word found *)
	  keyword := idx;			(* return scalar for word *)
	  in$symbol;				(* advance to following token *)
	  return;				(* <<<<<< exit sideways *)
	end;

  (* if we get here, the lookup fails *)

  error ('Keyword expected.');		(* abort command *)
 end;
$PAGE print_words
(* PRINT WORDS outputs a list of keywords.  As many as possible are placed on a
   single line, with a width of 71 (a good quess for most terminals). *)

procedure print_words ( s: message; minwrd, maxwrd: word_list );

 var
   cnt: 0..71;
   len: 0..10;
   word: word_list;

 begin
  writ$nl (s);

  cnt := maximum (cnt);			(* cause a new line for first word *)
  for word := minwrd to maxwrd do begin
    len := index (words[word].name, ' ', 11) - 1;	(* get lenght of non-blank chars *)
    if (cnt + len + 1) > 71 then begin		(* move to new line *)
      writ$nl ('');
      writ$str ('  ');				(* indent two spaces *)
      cnt := 2;
    end;
    writ$str (substr (words[word].name, 1, len));
    writ$str (' ');				(* follow word with blank *)
    cnt := cnt + len + 1;
  end;

  writ$nl ('');
  writ$nl ('');
 end;
$PAGE scope_ref
(* SCOPE REF parses a scope$stack reference.  It is entered with the current token
   presumably being the initial identifier:

     <scope$stack ref> ::= <module ref> | <subr ref>
	<module ref> ::= <identifier> '@' [ <subr ref> ]
	<subr ref> ::= { <identifier ':' }... [ '(' <intconst> ')' ]

   The identifiers found are placed in the name array; the first delimiter
   seen (i.e. either '@' or ':') placed in delim; and the level <intconst>
   placed in level.  It exits with token advanced to the next following 
   token.  If any error occurs, the parse is aborted. *)

procedure scope_ref ( var names: name_list;
		      var delim: symbol;
		      var level: stklevel   );

 begin
  fetch ([ident]);				(* 1st token must be ident *)
  delim := cur$sy;					(* assume it is '@' or ':' *)
  if delim = atsign then begin			(* have module name *)
    names.name_count := 1;			(* record cur$id *)
    names.name[1] := cur$id;			(* preserved if punctuation seen *)
    in$symbol;					(* get next symbol *)
    if not check (ident) then return;	(* just have <module ref> *)
  end
  else names.name_count := 0;			(* first delim is ':', we hope *)

  repeat
    names.name_count := names.name_count + 1;	(* record name *)
    names.name [names.name_count] := cur$id;
    fetch ([colon]);				(* loop entered with cur$id scanned *)
  until not check (ident);			(* until not <cur$id> follows *)

  level := 1;					(* default *)
  if check (lparent) then begin		(* have a level specification *)
    fetch ([intconst]);
    level := cur$val.intval;				(* get level number *)
    fetch ([rparent]);
  end;
 end;
$PAGE octal_value
(* OCTAL VALUE parses an integer value in default base 8.  If the constant is
   present, true is returned, and the value is assigned to the parameter "num".
   Otherwise, false is returned.  Algorithm: this first checks if the current
   symbol is an <intconst>, if so we know that a number is present.  However,
   unless the last character is a 'B', it has been converted in base 10. So,
   the conversion is redone in base 8, if necessary. *)

function octal_value (var num: integer): boolean;
 var power, temp: integer;
 begin
  octal_value := (cur$sy = intconst);
  if octal_value then begin
    if uppercase (input_line [cursor-1]) = 'B'
      then num := cur$val.intval			(* converted correctly *)
      else begin				(* must reconvert *)
	power := 1;
	temp := cur$val.intval;
	num := 0;
	while temp > 0 do begin
	  if (temp mod 10) >= 8 then error ('Conversion error.');
	  num := num + ((temp mod 10) * power);
	  power := power * 8;
	  temp := temp div 10;
	end;
      end;
    in$symbol;
  end
  else if (cur$sy=ident) andif (cur$id='NIL') then begin
    octal_value:= true;
    num:= ord(nil);
    in$symbol
  end;
 end;
$PAGE clear_pointer

  (* CLEAR_POINTER converts a virtual pointer to a real address
     if necessary, clearing the area field as well for string operations *)

  function clear_pointer (point: integer): integer;

  var convert: packed record
	case boolean of
	true:  (left: addrrange; right: addrrange);
	false: (val: integer)
      end;

  begin
    if v$rtual then
      convert.val := vtran$ (point)
    else convert.val := point;
    convert.left := 0;
    clear_pointer := convert.val;
  end;
$PAGE assign_simple
(* ASSIGN SIMPLE copies values of arbitrary kind from location to another, as
   specified by descriptors, after checking that the type are compatible. *)

procedure assign_simple (lhs, rhs: descriptor);

 type stroverlay = packed array[1..1] of char;
 var
   status: parse_error;
   lstroffset, stroffset: integer;
   strptr, lstrptr: ^ stroverlay;


 procedure prepare_string_rhs;		(* get info for rhs string *)
  begin
   if rhs.kind = char_dt then begin	(* make it a string *)
     rhs.value.sval[1] := chr (rhs.value.intval);
     rhs.addr.wordoffset := ord (address (rhs.value.sval));
     rhs.addr.packedflag := false;
     rhs.charsize := 1;
   end;
   strptr := ptr (clear_pointer (rhs.addr.wordoffset));
   if rhs.addr.packedflag	(* and offset of 1st char *)
     then stroffset := (rhs.addr.bitoffset div 7) + 1
     else stroffset := 1;
  end;

 begin
  status := check$type (rhs, lhs.typtr);	(* check that assignment is okay *)
  check_status (status, []);

  case lhs.kind of 

    scalar_dt, int_dt, real_dt, pointer_dt:
      store$scalar (lhs, rhs.value.intval);	(* possible assignment to packed field *)

    set_dt:
      copy$words (lhs.addr.wordoffset, rhs.addr.wordoffset, 2);	(* sets always 2 words *)

    char_dt:
      begin
	prepare_string_rhs;			(* put in standard form *)
	store$scalar (lhs, ord (strptr^[stroffset]));
      end;

    fstring_dt:
      begin
	prepare_string_rhs;
	lstrptr := ptr (clear_pointer (lhs.addr.wordoffset));
	if lhs.addr.packedflag
	  then lstroffset := (lhs.addr.bitoffset div 7) + 1
	  else lstroffset := 1;
	lstrptr^ [lstroffset : lhs.charsize] := strptr^[stroffset : rhs.charsize];
      end;

    vstring_dt:
      begin
	prepare_string_rhs;
	lstrptr := ptr (clear_pointer (lhs.addr.wordoffset +1));
	lstrptr^[1 : lhs.charsize] := strptr^[stroffset : rhs.charsize];
	if lhs.charsize <= rhs.charsize		(* store the length *)
	  then store$scalar (lhs, lhs.charsize)
	  else store$scalar (lhs, rhs.charsize);
      end;

    others:
      error ('Unable to assign to value.')

  end (* case kind *) ;
 end;
$PAGE assign
(* ASSIGN processes an assignment statement: <ref> := <expr>.  The right hand side
   expression is parsed in a manner dependent on the kind of the left hand side,
   as special syntax is allowed for pointers (and ultimately, records and arrays). *)

procedure assign;

 var
  lhs: descriptor;
  temp: descriptor;
  status: parse_error;


 function rhs: descriptor;		(* parses and returns simple value or reference *)
  begin
   rhs$value (rhs, status);
   check_status (status, []);
   check_end;
  end;


 begin
  lhs$ref (lhs, status);
  check_status (status, []);
  fetch ([becomes]);

  if lhs.kind in [scalar_dt, int_dt, real_dt, char_dt, fstring_dt, vstring_dt, set_dt]
    then assign_simple (lhs, rhs ())		(* no special syntax required *)

  else if lhs.kind = pointer_dt
    then begin					(* permit := <octal value> *)
      if v$rtual then
	error ('Assignment to a virtual pointer is not allowed.')
      else if not octal_value (temp.value.intval)
	then assign_simple (lhs, rhs ())
	else begin
	  set$kind (temp, lhs.typtr);		(* make the octal const a ptr of correct type *)
	  check_end;
	  assign_simple (lhs, temp);
        end;
    end

  else error ('Unable to assign to value.');	(* can't or not supported *)
 end;
$PAGE print_char
(* PRINT CHAR is an utility to print a single character value.  It checks if the
   character is printable; if not, it is displayed in escaped fashion. *)

procedure print_char ( cur$ch: char );
 begin
  if (cur$ch >= ' ') and (cur$ch <= '~')
    then if cur$ch = ''''				(* is printable *)
      then writ$str ('''''')			(* double prime *)
      else writ$str (cur$ch)
    else begin
      writ$str ('\');				(* <esc>nnn where nnn is the octal *)
      writ$int (ord (cur$ch), octal);
    end
 end;
$PAGE print
(* PRINT displays a value given as a descriptor. *)

procedure print (desc: descriptor);

 type
   stroverlay = packed array[1..1] of char;
   setoverlay = set of 0..71;

 var
   scalarname: alfa;
   strptr: ^ stroverlay;
   setelem: descriptor;
   first, is_nil: boolean;
   setptr: ^ setoverlay;
   offset: integer;
   idx: integer;

 begin
  with desc do begin
    case kind of

      real_dt: begin (*********note this code must change someday ***********)
	rewrite(tty); (*if ttyoutput wasn't open before, it is now*)
	write(tty,value.rval);
	break
      end;

      int_dt:
	writ$int (value.intval, decimal);

      char_dt:
	begin
	  if (value.intval>=ord(minimum(char)))and(value.intval<=ord(maximum(char))) then begin
	    writ$str (''''); print_char (chr (value.intval)); writ$str ('''');
	  end
	  else writ$str('?Bad Value')
        end;

      scalar_dt:
	begin
	  scalarname:= named$const(value,basetype);
	  writ$str(substr(scalarname,1,
	    index(scalarname,' ',length(scalarname)+1) - 1))
	end;

      pointer_dt: begin
	if v$rtual then
	  is_nil := value.intval = v$nil
	else is_nil := value.intval = ord (nil);
	if is_nil
	  then writ$str ('NIL')
	  else writ$int (value.intval, octal);
      end;

      fstring_dt:
	begin
	  strptr := ptr (clear_pointer (addr.wordoffset));
	  if addr.packedflag
	    then offset := (addr.bitoffset div 7) + 1	(* and offset of 1st char in first word *)
	    else offset := 1;
	  writ$str ('''');
	  for idx := offset to charsize + offset - 1 do
	    print_char (strptr^[idx]);
	  writ$str ('''');
	end;

      set_dt:
	begin
	  set$kind (setelem, sel);		(* make a desc for printing elems of set *)
	  if setelem.kind = char_dt
	    then offset := ord (' ')		(* origin of set *)
	    else offset := 0;
	  setptr := ptr (clear_pointer (addr.wordoffset));

	  writ$str ('[');
	  first := true;			(* controls printing ',' *)
	  idx := 0;
	  loop
	    (* search for first (next) element in set *)

	    while (idx <= 71) andif (not (idx in setptr^)) do idx := idx + 1;
	  exit if idx > 71;
	    if not first then writ$str (',');
	    first := false;
	    setelem.value.intval := idx + offset;
	    print (setelem);

	    (* search for the end of a range of elements *)

	    while (idx <= 71) andif (idx in setptr^) do idx := idx + 1;
	    if (idx - 1) <> (setelem.value.intval - offset) then begin
	      writ$str ('..');
	      setelem.value.intval := idx - 1 + offset;
	      print (setelem);
	    end;
	  end;
	  writ$str (']');
	end;

      others:
	error ('Cannot display value.')

    end (* case *) ;
  end;
 end;
$PAGE assign_or_print
(* ASSIGN OR PRINT interprets a command line not preceeded by a ".", that is,
   <ref> [ ':=' <expr> ].  Since we cannot determine which of the addressing
   routines to call until we know whether or not it is an assignment or a 
   print command, this routine performs an unsemanticated prescan of the LHS
   <ref> and then checks the terminating symbol. *)

procedure assign_or_print;

  (* The below is used to prescan a reference.  If there is an error in the
     reference, no attempt is made to process it.  The result is that the 
     delimiter of the reference will be garbage.  This does not matter as
     either the assign or print code will detect the error as well (if they
     don't find another first. *)

  procedure reference;
   begin
    if not check (ident) then return;
    while cur$sy in [arrow, period, lbrack] do begin
      if check (arrow) then		(* no action *)
      else if check (period) then begin	(* '.' <identifier> *)
	if not check (ident)
	  then return;
      end
      else if check (lbrack) then begin		(* '[' <expr> [ ',' <expr> ]* ']' *)
	repeat
	  if check (minus) orif check (plus) then ;	(* trivial <expr>s allowed *)
	  if check (intconst) then
	  else if check (stringconst) then
	  else reference;
	until not check (comma);
	if not check (rbrack) then return;
      end
    end (* while *) ;
   end;


 var
  delim: symbol;
  desc: descriptor;
  status: parse_error;

 begin
  check_debug;				(* module must be in debug to access variables *)
  reference;				(* prescan LHS *)
  delim := cur$sy;				(* right hand delim of reference *)
  cursor := first_cursor;		(* backup *)
  cur$ch := ' '; in$symbol;			(* re-lex 1st token *)

  if delim = becomes	(* make choice *)
    then assign
    else begin		(* print value *)
      rhs$value (desc, status);		(* process the value reference *)
      check_status (status, []);
      check_end;
      print (desc); writ$nl ('');
    end;
 end;
$PAGE do_command
(* DO COMMAND executes a single command.  When called, the current token is the
   token following the identifying "."; when exited, the current token is either
   a semicolon or eofsy (=> eol).  This returns true if more commands are to be
   read; false, if program execution is to be resumed.  The skip$ and p$skip
   flags are set appropriately on a false return. *)

function do_command: boolean;

 var
   names: name_list;
   delim: symbol;
   level: stklevel;
   pb: progblkptr;
   fb: fileblkptr;
   sb: stmtblkptr;
   brknum: brknumtype;
   scope_err: scopereturn;
   wrd: word_list;
   new_scope: scope_type;
   new_with: with_type;
   acs: uuo_acblk;
   desc: descriptor;
   status: parse_error;

 begin
  do_command := true;				(* only set false by step, etc. *)
  wrd := keyword;				(* get a keyword *)
  case wrd of

    dump_wrd:		(* DUMP *)
      dump;

    open_wrd:		(* OPEN [ <scope$stack ref> | <intconst> ] *)
      begin
	if cur$sy = eofsy				(* open scope$stack of entry *)
	  then begin
	    level := 1;				(* need a var argument below *)
	    if ba$i$.mainflag = 0
	      then scope_err := frame$scope (level, new_scope)
	      else scope_err := stack$scope (ba$i$.basis, new_scope);
	    names.name_count := 1;		(* dummy up for error reporting *)
	    names.name[1] := '*initial*';
	  end
	else if check (intconst)		(* open specified level of stack *)
	  then begin
	    check_end;
	    level := cur$val.intval;		(* place where error printer expects *)
	    if level <= 0 then error ('Invalid frame number.');
	    scope_err := frame$scope (level, new_scope);
	    if (scope_err = debug) and (level <> cur$val.intval) then begin
	      names.name_count := 1;		(* for error reporting *)
	      names.name[1] := '';
	      scope_err := wronginvocaion;
	    end;
	  end
	else
	  begin
	    scope_ref (names, delim, level);	(* get scope$stack reference *)
	    check_end;
	    if delim = atsign
	      then scope_err := mod$scope (names, level, new_scope)
	      else scope_err := ext$scope (names, level, new_scope);
	  end;
	case scope_err of

	  notdefined:
	    writ$nl ('External name not defined: ' || names.name[1]);

	  notpascal:
	    writ$nl ('Not a PASCAL module: ' || names.name[1]);

	  badnest:
	    writ$nl ('Invalid subroutine: ' || names.name[names.name_count]);

	  wronginvocation:
	    begin
	      if level <> 0 then begin		(* lesser invocation found *)
		writ$str ('Level not found, level ');
		writ$int (level, decimal);
		writ$nl (' used instead.');
	      end
	      else writ$nl ('Not active.');
	    end;

	  noscope, notdebug:
	    writ$nl ('Module not compiled in debug mode.')

	end (* case *) ;
	if not (scope_err in [debug,wronginvocation]) then begin
	  if scope_state = debug then st$open(scope$stack.displays[1].prog_blk);
	  goto 100 (*must reset current module to previous after error*)
	end;
	scope$stack := new_scope;
	scope_state := debug;
	with$stack.active_withs := 0;
      end;

    with_wrd:		(* WITH [ <ref> [ ',' <ref> ]* ] *)
      begin
	check_debug;			(* must be in debug to have record vars *)
	with new_with do begin
	  active_withs := 0;
	  if not (cur$sy in [semicolon, eofsy]) then begin
	    repeat			(* get list of record vars *)
	      lhs$ref (desc, status);
	      check_status (status, [not_modifiable]);
	      if desc.kind <> record_dt
		then error ('Not a record.');
	      if active_withs = maxwithlevel
		then error ('Too many with''s.');
	      active_withs := active_withs + 1;
	      with withs [active_withs] do begin	(* fill in info for level *)
		rectype := desc.typtr;		(* record type *)
		recbase := desc.addr;		(* address of record *)
		fldidtree := desc.fst;		(* first field *)
	      end;
	    until not check (comma);
	  end (* if end of command *) ;
	end (* with *) ;
	check_end;
	with$stack := new_with;
      end;

    break_wrd:		(* BREAK <line address> *)
      begin
	sb := line_address;			(* parse line address *)
	if cur$sy=stringconst then in$symbol (*note cur$val unchanged below*)
	else cur$val.slgth:= 0; (*to null associated break string*)
	check_end;
	if not set$brkpt (sb, brknum)			(* only fails if too many *)
	  then error ('Breakpoint not set: too many.')
	  else begin
	    brk$strings[brknum]:= substr(cur$val.sval,1,cur$val.slgth);
	    writ$str ('Break #'); writ$int (brknum, decimal);
	    writ$nl (' set.');
	  end;
      end;

    clear_wrd:		(* CLEAR [ <intconst> ] *)
      begin
	if check (intconst) then begin		(* clear specified breakpoint *)
	  check_end;
	  if not clr$brkpt (cur$val.intval)
	    then error ('No such breakpoint.');
	end
	else begin				(* clear all break points *)
	  check_end;
	  if query ('Clear all')		(* check before doing it *)
	    then clr$all$brkpt;
	end
      end;

    proceed_wrd:		(* PROCEED *)
      begin
	if check(intconst) then br$kip:= cur$val.intval
	else br$kip:= 1;
	check_end;
	do_command := false;				(* cause debugger to return *)
      end;

    step_wrd,	(* STEP [ <intconst> ] *)
    sstep_wrd:	(* SSTEP [ <intconst> ] *)
      begin
	if check (intconst)
	  then step$ := cur$val.intval			(* assumed presrved *)
	  else step$ := 1;
	check_end;
	p$kip := (wrd = step_wrd);		(* count calls as single statements for STEP *)
	do_command := false;				(* cause debugger to return *)
      end;

    exit_wrd,				(* EXIT *)
    stop_wrd:				(* STOP *)
      begin
	check_end;
	rea$on := badreason;			(* so runtime knows what's up *)
	if douuo (047B (* calli *), acs, ord (wrd = exit_wrd), 0, 12B (* calli ,12 *)) = noskip
	  then ;				(* exit 0, for step; exit 1, for exit.  Note well:
						   this should not return *)
      end;

    where_wrd:			(* WHERE <reference> *)
      begin
	check_debug;			(* scope$stack must be in debug mode *)
	lhs$ref (desc, status);	(* parse the reference *)
	check_status (status, [not_modifiable]);
	check_end;
	with desc.addr do begin		(* display the address *)
	  writ$int (wordoffset, octal);
	  if packedflag then begin	(* bit offset significant *)
	    writ$str (' bit ');
	    writ$int (bitoffset, decimal);
	  end;
	  writ$nl ('');
	end;
      end;

    help_wrd,			(* HELP *)
    com_wrd:			(* COMMANDS *)
      begin
	while not (cur$sy in [semicolon, eofsy]) do 	(* for these commands, don't be picky *)
	  in$symbol;
	writ$nl ('');
	print_words ('Available commands:', minimum (command_wrds), maximum (command_wrds));
	print_words ('Display options:', minimum (display_opts), maximum (display_opts));
       end;

    version_wrd:		(* VERSION *)
      begin
	check_end;
	writ$str (version_id);
	writ$str (' of ');
	writ$nl (compdate);
      end;

    display_wrd:		(* DISPLAY options *)
      begin
	wrd := keyword;		(* get option *)
	case wrd of

	  break_wrd:	(* DISPLAY BREAKPOINTS *)
	    begin
	      check_end;
	      dmp$breaks;
	    end;

	  stack_wrd:	(* DISPLAY STACK [ <intconst> ] *)
	    begin
	      if check (intconst)
		then level := cur$val.intval
		else level := maximum (stklevel);
	      check_end;
	      dmp$stack (level);
	    end;

	  loc_wrd:	(* DISPLAY LOCATION *)
	    begin
	      check_end;
	      dmp$location;
	    end;

	  scope_wrd:	(* DISPLAY SCOPE *)
	    begin
	      check_end;
	      check_debug;
	      dmp$scope;
	    end;

	  files_wrd:	(* DISPLAY FILES [ <module cur$id> '@' ] *)
	    begin
	      pb := modulename (true);		(* parse and lookup module *)
	      check_end;
	      dmp$files (pb);
	    end;

	  pages_wrd:	(* DISPLAY PAGES [ <module cur$id> '@'] [ <file cur$id> ] *)
	    begin
	      fb := filename (modulename (true), true);
	      check_end;
	      dmp$pages (fb);
	    end;

	  modules_wrd:	(* DISPLAY MODULES *)
	    begin
	      check_end;
	      dmp$modules;
	    end;

	  others:
	    error ('Invalid option.')

	end (* case *) ;
      end;

    others:
      error ('Invalid command.')

  end (* case *) ;
 end;
$PAGE process_db_commands
(* PROCESS DB COMMANDS is the debuggers central command loop.  Lines are read
   from the terminal, and the contained command(s) processed one at a time by
   calling db_command. *)

procedure process_db_commands;
 begin
  loop						(* exits by return *)
    first_cursor:= cursor;  (*for backing up in print/assign*)
    loop
      in$symbol;					(* get 1st symbol *)
      if cur$sy = eofsy				(* just <cr> *)
        then
      else if cur$sy <> period
	then assign_or_print			(* <ref> [ := <expr> ] *)
	else begin				(* .command line *)
	  in$symbol;				(* get token after "." *)
	  if not do_command			(* returns false for step, proceed, etc. *)
	    then return
	end;
    exit if cur$sy <> semicolon;   (*repeat until end of line*)
      first_cursor:= cursor-1; (*as cursor is 1 ahead of cur$ch*)
    end;
    prompt('');
  end (* loop *) ;
 end;
$PAGE debug$ mainline
var dummy: integer; new_scope: scope_type; modname: alfa;

begin
  if ba$i$.mainflag = 0				(* open scope$stack of procedure interrupted *)
    then begin					(* is main *)
      dummy := 1;				(* need a var *)
      scope_state := frame$scope (dummy, new_scope);
    end
    else scope_state := stack$scope (ba$i$.basis, new_scope);	(* is subr *)
  if with$stack.active_withs<>0 then begin (*see if we can preserve them*)
    if (rea$on=init) orif (scope_state<>debug) orif
      (new_scope.display_levels<>scope$stack.display_levels) orif
      (new_scope.displays[new_scope.display_levels] <>
       scope$stack.displays[scope$stack.display_levels]) then begin
	scope$stack:= new_scope;  (*context has changed from last time here*)
	with$stack.active_withs:= 0
    end
  end
  else scope$stack:= new_scope; (*if no withs to preserve, don't check*)

  case rea$on of (* per entry initialization *)
    badreason: begin
      writ$nl('Debugger entered abnormally -- not properly initialized');
      stop
    end;

    init:					(* initialization entry *)
      begin
	init_db;				(* perform per job initialization *)

	(* If overlay manager is present, then resume execution until first
	   statement in debug mode is encountered (pony up step 1), instead
	   of stopping now.  Manager's name is OVLTV. *)

	modname:= '';
	if sym$lookup('OVLTV.',modname)<>nil then begin
	  rea$on:= badreason; br$kip:= -1;
	  step$:= 1; p$kip:= false;  (*pony up step command*)
	  return
	end;

	writ$str ('PASCAL Debugger, ');
	writ$nl (version_id);
	writ$nl ('');
      end;

      brkpt: begin (*execute associated string (and later commands)*)
	dmp$location;
	at$eoln:= true;
	step$:= -1; br$kip:= -1;
	prompt(brk$strings[break$]);
	process_db_commands;
	rea$on:= badreason;
	return;   (*return point if no errors seen*)
      end;

    others:		(* tell user how entered *)
	dmp$location
  end (* case *) ;

  at$eoln := true;				(* fake out READNL, like open is to reset *)

100 (* reenter cl *) :
  step$ := -1; br$kip:= -1;			(* clear step count *)
  prompt('');
  process_db_commands;				(* returns with step$ and p$kip set *)
  if rea$on in [trap,rterr] then begin
    writ$nl('Can''t continue after trap or run-time error');
    goto 100  (*don't let him out*)
  end;
  rea$on := badreason;				(* until reentered, don't know why *)
end.
   !s k