$TITLE pmfdef -- macro definition and lookup routines
$LENGTH 43
$OPTIONS special, nocheck
$INCLUDE pmf.typ
$INCLUDE pmferr.inc

(*  Macro definitions are stored in a hash table of linked lists of nodes,
    each containing a pointer to a macro name, a pointer to a defining text
    string block, and a link.  *)

const
    hash_size = 113;				(* 113 is a prime number.  Assume 1000 defined
						   symbols for a run; then the average chain
						   length will be under 10 symbols--under 20,
						   even if only half the chains are populated. *)
    hash_lim = 112;				(* = hash_size - 1 *)

type
    hash_index = 0 .. hash_lim;

var
    def_chain: packed array [hash_index] of definition;
    first_time: boolean := true;
$PAGE definit
public procedure definit;

var
    p: definition;
    i: hash_index;

begin
  if first_time then				(* Initialize the hash table. *)
    for i := 0 to hash_lim do
      def_chain [i] := nil
  else						(* Get rid of any leftovers from the last file. *)
    for i := 0 to hash_lim do
      while def_chain [i] <> nil do begin
	p := def_chain [i];
	with p^ do begin
	  if def_text <> nil then dispose (def_text);
	  def_chain [i] := chain;
	end;
	dispose (p);
      end;
  first_time := false;
end (* definit *);
$PAGE lookup
(*  LOOKUP will scan the definition chain for a macro whose name matches
    the given string.  If there is one, LOOKUP will return a pointer to the
    definition node; otherwise, LOOKUP will return nil.  The local vari-
    able LAST will be left pointing to the node which points to the
    returned node, or to nil if the returned node is the first in the list.
    The local variable HASH will be left indexing the chain for this
    name, regardless of whether it is defined.  *)

var
    last: definition;
    hash: hash_index;

public function lookup ( s: sym_string ): definition;

var
    sum: integer;
    i: string_index;

begin
  (*  Compute the hash table index.  *)

  sum := 0;
  for i := 1 to length(s) do
    sum := sum + ord(s[i]);
  hash := sum mod hash_size;

  (*  Scan the selected chain.  *)

  lookup := def_chain [hash];
  last := nil;
  while lookup <> nil do
    with lookup^ do begin
      if substr(name_text,1,name_len) = s then return;
      last := lookup;
      lookup := chain;
    end;
end (* lookup *);
$OPTIONS check
$PAGE define
(*  DEFINE will attach a new macro definition at the head of the definition
    chain, with the specified name, definition text, and kind.  *)

public procedure define ( name: sym_string;
			  dkind: macro_kinds;
			  text: str_pointer );

var
    sum: integer;
    i: string_index;
    hash: hash_index;
    new_def: definition;

begin
  (*  Compute the hash table index.  *)

  sum := 0;
  for i := 1 to length(name) do
    sum := sum + ord(name[i]);
  hash := sum mod hash_size;

  (* Attach the definition node to the front of the selected chain.  *)

  new (new_def:length(name));
  with new_def^ do begin
    kind := dkind;
    chain := def_chain [hash];
    name_len := length(name);
$OPTIONS nocheck
    name_text[1:name_len] := name;
$OPTIONS check
    def_text := text;
  end;
  def_chain [hash] := new_def;
end (* define *);
$OPTIONS nocheck
$PAGE undef
(*  UNDEF will disconnect a definition node with the specified name from
    the definition chain, and will dispose of the node.  If there is no
    node with the specified name, undef will have no effect.  *)

public procedure undef ( name: sym_string );

var def: definition;

begin
  def := lookup(name);
  if def = nil then return;
  with def^ do begin
    if def_text <> nil then
      dispose (def_text);
    if last = nil
      then def_chain [hash] := chain
      else last^.chain := chain;
  end;
  dispose (def);
end (* undef *);
$PAGE definition library file routines
(*  It is possible to maintain a library of macro definitions on a disk file.
    These definitions are then available for use in the processing of any
    source file.  A definition library is created by a call to the macro
    '#SAVE', which causes a call to the 'lib_save' procedure.  Any source
    file can load the definitions from a library with a call to the macro
    '#LIB', which causes a call to the 'lib_load' procedure.  *)

(*  The first line of a definition library is an identification text to
    verify that this really is a definition library file.  This is followed
    by a definition list for each hash chain.  Each definition list contains
    zero or more definitions, followed by a trailer record.  Each definition
    consists of a definition header line, followed by zero or more lines of
    definition text.  A definition header line contains either 'M' for a
    user defined macro or 'L' for a user defined literal, followed by the
    number of characters in the definition, followed by a space, followed
    by the macro name.  The definition text is stored verbatim, except
    that argument markers are represented by a flag character followed by
    the letter 'A', and the flag character is represented as a double flag
    character.  A trailer line simply contains the letter 'E'.  Note that
    only user defined macros and literals are written to the library file.
    System macros are not.  If there is a second argument to #SAVE, its
    text will occur at the end of the library file, and will replace the
    call to #LIB.  *)

const
    flag = chr(126);

public var
    lib_file_id: sym_string;

var
    lib_file: text;

static var
    lib_header: string [22] := 'PMF VERSION 1 LIB FILE';

$INCLUDE pmfput.inc
$INCLUDE pmfinp.inc
external function pathname ( var f: text ): sym_string;
$PAGE lib_save
public function lib_save: boolean;

var
    hash: hash_index;
    ind: string_index;
    c: char;
    p: definition;

begin
  if lib_file_id <> ''
    then rewrite (lib_file,'.PML '||lib_file_id)
    else rewrite (lib_file,pathname(output)||'.PML[,]');
  lib_save := eof(lib_file);
  if not lib_save then return;			(* Bad library file. *)
  writeln (lib_file,lib_header);
  for hash := 0 to hash_lim do begin
    p := def_chain [hash];
    while p <> nil do
      with p^ do begin
	if kind in [user_macro,user_literal] then
	  with def_text^ do begin
	    if kind = user_macro
	      then write (lib_file,'M')
	      else write (lib_file,'L');
	    writeln (lib_file,name_len,str_length,' ',
		     substr(name_text,1,name_len));
	    for ind := 1 to str_length do begin
	      c := str_text [ind];
	      if c = eol then
		writeln (lib_file)
	      else if c = arg_marker then
		write (lib_file,flag,'A')
	      else if c = flag then
		write (lib_file,flag,flag)
	      else
		write (lib_file,c);
	    end;
	    writeln (lib_file);
	  end (* with def_text^ *);
	p := chain;
      end (* with p^ *);
    writeln (lib_file,'E');
  end (* for hash := 0 to hash_size *);
  if num_args >= 2 then begin
    writeln (lib_file,arg_length(2));
    for ind := arg_length(2) downto 1 do begin
      c := arg_char(2,ind);
      if c = eol
	then writeln (lib_file)
	else write (lib_file,c);
    end;
    writeln (lib_file);
  end;
  close (lib_file);
end (* lib_save *);
$PAGE lib_load
public function lib_load: boolean;

var
    hash: hash_index;
    ind: string_index;
    nam_len: string_index;
    str_len: string_index;
    attach_ptr: definition;
    def: definition;
    c: char;

begin
  if lib_file_id <> ''
    then reset (lib_file,'.PML '||lib_file_id)
    else reset (lib_file,pathname(output)||'.PML[,]');
  lib_load := not eof(lib_file);
  if not lib_load then return;			(* Bad library file. *)
  for ind := 1 to length(lib_header) do begin
    lib_load := (not eoln(lib_file)) andif (lib_file^ = lib_header [ind]);
    if not lib_load then return;		(* Bad library file. *)
    get (lib_file);
  end;
  readln (lib_file);
  for hash := 0 to hash_lim do begin
    attach_ptr := nil;
    loop
      read (lib_file,c);
    exit if c = 'E';
      read (lib_file,nam_len,str_len);
      get (lib_file);
      new (def:nam_len);
      new (def^.def_text:str_len);
      with def^, def_text^ do begin
	if c = 'M'
	  then kind := user_macro
	  else kind := user_literal;
	name_len := nam_len;
	for ind := 1 to name_len do begin
	  read (lib_file,c);
	  name_text [ind] := c;
	end;
	readln (lib_file);
	str_length := str_len;
	for ind := 1 to str_length do begin
	  if eoln(lib_file) then
	    begin
	      c := eol;
	      readln (lib_file);
	    end
	  else if lib_file^ = flag then
	    begin
	      get (lib_file);
	      if lib_file^ = 'A'
		then c := arg_marker
		else c := flag;
	      get (lib_file);
	    end
	  else
	    read (lib_file,c);
	  str_text [ind] := c;
	end (* for ind := 1 to str_length *);
	readln (lib_file);
	if attach_ptr = nil then
	  begin
	    chain := def_chain [hash];
	    def_chain [hash] := def;
	    attach_ptr := def;
	  end
	else
	  begin
	    chain := attach_ptr^.chain;
	    attach_ptr^.chain := def;
	    attach_ptr := def;
	  end;
      end (* with def^, def_text^ *);
    end (* loop *);
    readln (lib_file);
  end (* for hash := 0 to hash_lim *);
  if not eof(lib_file) then begin
    readln (lib_file,str_len);
    for ind := 1 to str_len do begin
      if eoln(lib_file) then begin
	readln (lib_file);
	put_back (eol);
      end
      else begin
	read (lib_file,c);
	put_back (c);
      end;
    end;
  end;
  close (lib_file);
end (* lib_load *).
  