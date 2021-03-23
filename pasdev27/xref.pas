program xref;

$OPTIONS special
$INCLUDE getiof.inc
$INCLUDE fio.typ[31024,320155]
$INCLUDE fio.inc[31024,320155]
$INCLUDE cmdutl.typ[31024,320220]
$INCLUDE filutl.inc[31024,320220]


var title: string [60];
$PAGE symbol table declarations
type

    proc = ^ proc_node;
    mod_list = ^ mod_node;
    calls_list = ^ calls_node;
    called_list = ^ called_node;
    mod_tree = ^ mod_tree_node;

    proc_string = packed array [1..10] of char;

    proc_node = packed record
	proc_name: proc_string;
	llink, rlink: proc;
	r_terminal: boolean;
	sys_proc: boolean;
	unique: boolean;
	modules: mod_list
    end;

    mod_node = packed record
	mod_ref: mod_tree;
	owner: proc;
	next: mod_list;
	next_proc: mod_list;
	calls: calls_list;
	called_by: called_list
    end;

    calls_node = packed record
	next: calls_list;
	case boolean of
	    true: ( called_proc: proc );
	    false: ( called_mod: mod_list )
    end;

    called_node = packed record
	next: called_list;
	calling_mod: mod_list
    end;


    mod_string = packed array [1..6] of char;

    mod_number = 0 .. 255;

    mod_tree_node = packed record
	mod_name: mod_string;
	proc_list: mod_list;
	llink, rlink: mod_tree;
	r_terminal: boolean;
	calling_tag: boolean;
	called_tag: boolean;
	mod_num: mod_number
    end;

    proc_count = 0 .. 32767;

    use_record = packed record
	calling_procs: proc_count;
	called_procs: proc_count
    end;

    mod_vector = ^ mod_vec_node;

    mod_vec_node = array [mod_number] of use_record;

    mod_array = ^ mod_array_node;

    mod_array_node = array [mod_number] of mod_vector;

var
    n_mods: mod_number;
    mod_table: mod_tree;
    use_table: mod_array;
    proc_tree: proc;
$PAGE enter_proc

(*  EnterProc returns a pointer to a proc tree node with a specified name.  *)

function enter_proc ( name: proc_string ): proc;

  (*  AddProc creates a new proc tree node with a specified right thread.  *)

  function add_proc ( thread: proc ): proc;
  begin
    new (add_proc);
    with add_proc^ do begin
      proc_name := name;
      llink := nil;
      rlink := thread;
      r_terminal := true;
      sys_proc := false;
      modules := nil;
    end;
  end (* add_proc *);

begin
  if proc_tree = nil then begin
    proc_tree := add_proc (nil);
    enter_proc := proc_tree;
    return; (* <---- return with newly created proc node *)
  end;

  enter_proc := proc_tree;
  loop
    with enter_proc^ do begin
      if (proc_name = name) and not sys_proc then
	return; (* <---- return with node found in tree *)

      if (proc_name > name) then begin
	if llink = nil then begin
	  llink := add_proc (enter_proc);
	  enter_proc := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_proc := llink
      end

      else (* proc_name <= name *) begin
	if r_terminal then begin
	  rlink := add_proc (rlink);
	  r_terminal := false;
	  enter_proc := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_proc := rlink;
      end;
    end (* with enter_proc^ *);
  end (* loop *);
end (* enter_proc *);
$PAGE enter_sys_proc

(*  EnterSysProc is the same as EnterProc, except that it enters a SysProc in
    the name tree.  *)

function enter_sys_proc ( name: proc_string ): proc;

  (*  AddSysProc performs the same function as AddProc in EnterProc.  *)

  function add_sys_proc ( thread: proc ): proc;
  begin
    new (add_sys_proc);
    with add_sys_proc^ do begin
      proc_name := name;
      llink := nil;
      rlink := thread;
      r_terminal := true;
      sys_proc := true;
      modules := nil;
    end;
  end (* add_sys_proc *);

begin
  if proc_tree = nil then begin
    proc_tree := add_sys_proc (nil);
    enter_sys_proc := proc_tree;
    return; (* <---- node added as root of tree - return *)
  end;

  enter_sys_proc := proc_tree;
  loop
    with enter_sys_proc^ do begin
      if (proc_name = name) and sys_proc then
	return; (* <---- return with node found in tree *)

      if (proc_name >= name) then begin
	if llink = nil then begin
	  llink := add_sys_proc (enter_sys_proc);
	  enter_sys_proc := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_sys_proc := llink
      end

      else (* proc_name < name *) begin
	if r_terminal then begin
	  rlink := add_sys_proc (rlink);
	  r_terminal := false;
	  enter_sys_proc := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_sys_proc := rlink;
      end;
    end (* with enter_sys_proc^ *);
  end (* loop *);
end (* enter_sys_proc *);
$PAGE add_mod_node

(*  AddModNode creates a new module table node with a specified right thread.  *)

function add_mod_node ( name: mod_string; thread: mod_tree ): mod_tree;

begin
  new (add_mod_node);
  with add_mod_node^ do begin
    mod_name := name;
    proc_list := nil;
    llink := nil;
    rlink := thread;
    r_terminal := true;
    called_tag := false;
    calling_tag := false;
  end;
end (* add_mod_node *);
$PAGE enter_module

(*  EnterModule returns a pointer to a module table node with a specified name. *)

function enter_module ( name: mod_string ): mod_tree;

begin
  if mod_table = nil then begin
    mod_table := add_mod_node (name, nil);
    enter_module := mod_table;
    return; (* <---- return with newly created module node *)
  end;

  enter_module := mod_table;
  loop
    with enter_module^ do begin
      if mod_name = name then
	return; (* <---- return with node found in tree *)

      if mod_name > name then begin
	if llink = nil then begin
	  llink := add_mod_node (name, enter_module);
	  enter_module := llink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_module := llink;
      end

      else (* mod_name < name *) begin
	if r_terminal then begin
	  rlink := add_mod_node (name, rlink);
	  r_terminal := false;
	  enter_module := rlink;
	  return; (* <---- return with newly created node *)
	end
	else
	  enter_module := rlink;
      end;
    end (* with enter_module^ *);
  end (* loop *);
end (* enter_module *);
$PAGE add_module

(*  AddModule returns a pointer to an entry with a given name on the module
    list for a specified proc node.  *)

function add_module ( m: mod_tree; pr: proc ): mod_list;

var last: mod_list;
    name: mod_string;

begin
  name := m^.mod_name;
  add_module := pr^.modules;
  last := nil;
  while (add_module <> nil) andif (add_module^.mod_ref^.mod_name <= name) do begin
    last := add_module;
    add_module := add_module^.next;
  end;
  if (add_module = nil) orif (add_module^.mod_ref^.mod_name <> name) then begin
    if last = nil then begin
      new (pr^.modules);
      last := pr^.modules;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      mod_ref := m;
      next_proc := m^.proc_list;
      owner := pr;
      next := add_module;
      calls := nil;
      called_by := nil;
    end;
    m^.proc_list := last;
    add_module := last;
  end;
end (* add_module *);
$PAGE add_call

(*  AddCall adds an entry for a specified proc to the call list of a given
    proc/module.  *)

procedure add_call ( m: mod_list; pr: proc );

var call, last: calls_list;

begin
  call := m^.calls;
  last := nil;
  while (call <> nil) andif
	( (call^.called_proc^.proc_name < pr^.proc_name) or
	  ( (call^.called_proc^.proc_name = pr^.proc_name) and
	    (call^.called_proc^.sys_proc < pr^.sys_proc) ) ) do begin
    last := call;
    call := call^.next;
  end;
  if (call = nil) orif (call^.called_proc <> pr) then begin
    if last = nil then begin
      new (m^.calls);
      last := m^.calls;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      called_proc := pr;
      next := call;
    end;
  end;
end (* add_call *);
$PAGE add_called_by

(*  AddCalledBy adds an entry for a specified proc/module to the called_by list
    of a specified proc/module.  *)

procedure add_called_by ( called, caller: mod_list );

var call, last: called_list;

begin
  call := called^.called_by;
  last := nil;
  while (call <> nil) andif (call^.calling_mod <> caller) do begin
    last := call;
    call := call^.next;
  end;
  if call = nil then begin
    if last = nil then begin
      new (called^.called_by);
      last := called^.called_by;
    end
    else begin
      new (last^.next);
      last := last^.next;
    end;
    with last^ do begin
      calling_mod := caller;
      next := call;
    end;
  end;
end (* add_call *);
$PAGE get_sym_line

(*  GetSymLine reads a cross-reference file line.  If the line has the form
	*procedure  module
    it sets Kind to DeclLine, ProcId to 'procedure', and ModId to 'module'.
    If the line has the form
	+procedure
    it sets Kind to SysLine, ProcId to 'procedure', and ModId to blanks.
    If the line has the form
	 procedure
    it sets Kind to CallLine, ProcId to 'procedure', and ModId to blanks.  *)

type sym_line_kind = ( decl_line, sys_line, call_line );

procedure get_sym_line ( var kind: sym_line_kind;
			 var proc_id: proc_string;
			 var mod_id: mod_string );

var i: 1 .. 10;
    ch: char;

begin
  if input^ = '*' then
    kind := decl_line
  else if input^ = '+' then
    kind := sys_line
  else if input^ = ' ' then
    kind := call_line
  else begin
    writeln (tty, '? Bad line in SYM file:');
    while not eoln (input) do begin
      read (input, ch);
      write (tty, ch);
    end;
    writeln (tty);
    stop;
  end;

  get (input);
  for i := 1 to 10 do
    if eoln (input) then
      proc_id [i] := ' '
    else begin
      proc_id [i] := input^;
      get (input);
    end;
  if not eoln (input) then
    get (input);
  for i := 1 to 6 do
    if eoln (input) then
      mod_id [i] := ' '
    else begin
      mod_id [i] := input^;
      get (input);
    end;
  readln (input);
end (* get_sym_line *);
$PAGE build_proc_tree

(*  BuildProcTree will read the symbol file and create the symbol table with
    the complete calls relation.  *)

procedure build_proc_tree;

var
    cur_proc_mod: mod_list;
    kind: sym_line_kind;
    proc_name: proc_string;
    mod_name: mod_string;

begin
  proc_tree := nil;
  mod_table := nil;
  while not eof (input) do begin
    get_sym_line (kind, proc_name, mod_name);
    case kind of
      decl_line:
	cur_proc_mode := add_module (enter_module (mod_name),
				     enter_proc (proc_name));
      call_line:
	add_call (cur_proc_mod, enter_proc (proc_name));
      sys_line:
	add_call (cur_proc_mod, enter_sys_proc (proc_name))
    end (* case *);
  end;
end (* build_proc_tree *);
$PAGE linearize_trees

(*  LinearizeTrees turns the proc tree and module table from trees into simple
    linked lists, chained on the rlink fields of their nodes.  *)

procedure linearize_proc_tree;

var pr: proc;
    m: mod_tree;

begin
  if proc_tree = nil then return;
  while proc_tree^.llink <> nil do
    proc_tree := proc_tree^.llink;
  pr := proc_tree;
  while pr^.rlink <> nil do begin
    with pr^ do begin
      if not r_terminal then
	while rlink^.llink <> nil do
	  rlink := rlink^.llink;
      pr := rlink;
    end (* with pr^ *);
  end (* while pr^.rlink <> nil *);

  n_mods := 0;
  if mod_table = nil then return;
  while mod_table^.llink <> nil do
    mod_table := mod_table^.llink;
  m := mod_table;
  loop
    with m^ do begin
      n_mods := n_mods + 1;
      mod_num := n_mods;
  exit if rlink = nil;
      if not r_terminal then
	while rlink^.llink <> nil do
	  rlink := rlink^.llink;
      m := rlink;
    end (* with m^ *);
  end (* while m^.rlink <> nil *);
end (* linearize_trees *);
$PAGE resolve_modules

(*  ResolveModules ensures that every proc node has at least one module node.
    If it doesn't, then it gets a module with a blank name.  In addition, the
    unique field of the proc node is set if the proc has exactly one module
    node.  *)

procedure resolve_modules;

var
    pr: proc;
    m_sys, m_undef: mod_tree;
    m: mod_list;

begin
  m_sys := add_mod_node ('*SYS*  ', nil);
  m_undef := add_mod_node ('*UNDEF', nil);
  pr := proc_tree;
  while pr <> nil do begin
    if pr^.modules = nil then
      if pr^.sys_proc
	then m := add_module (m_sys, pr)
	else m := add_module (m_undef, pr);
    pr^.unique := (pr^.modules^.next = nil);
    pr := pr^.rlink;
  end;
end (* resolve_modules *);
$PAGE const_called_by_relation

(*  ConstCalledByRelation adds called_by lists to the symbol table to
    complete the called_by relation.  *)

procedure const_called_by_relation;

var
    pr: proc;
    m: mod_list;
    call: calls_list;
    called_mod: mod_list;

begin
  pr := proc_tree;
  while pr <> nil do begin
    m := pr^.modules;
    while m <> nil do begin
      call := m^.calls;
      while call <> nil do begin
	called_mod := call^.called_proc^.modules;
	  while (called_mod^.mod_ref <> m^.mod_ref) and
		(called_mod^.next <> nil) do
	  called_mod := called_mod^.next;
	if called_mod^.mod_ref <> m^.mod_ref then
	  called_mod := call^.called_proc^.modules;
	call^.called_mod := called_mod;
	add_called_by (called_mod, m);
	call := call^.next;
      end;
      m := m^.next;
    end;
    pr := pr^.rlink;
  end;
end (* const_called_by_relation *);
$PAGE const_use_table

(*  ConstUseTable creates the vectors containing use_records for each
    calling module/called module pair.  *)

procedure const_use_table;

var m1, m2: mod_number;
    m, mm: mod_tree;
    pr: mod_list;
    pr_calls: calls_list;
    pr_called: called_list;

begin
  new (use_table: n_mods);
  for m1 := 1 to n_mods do begin
    new (use_table^[m1]: n_mods);
    for m2 := 1 to n_mods do begin
      with use_table^[m1]^[m2] do begin
	called_procs := 0;
	calling_procs := 0;
      end;
    end;
  end;

  m := mod_table;
  while m <> nil do begin
    pr := m^.proc_list;
    while pr <> nil do begin
      pr_calls := pr^.calls;
      while pr_calls <> nil do begin
	pr_calls^.called_mod^.mod_ref^.called_tag := true;
	pr_calls := pr_calls^.next;
      end;
      pr_called := pr^.called_by;
      while pr_called <> nil do begin
	pr_called^.calling_mod^.mod_ref^.calling_tag := true;
	pr_called := pr_called^.next;
      end;
      mm := mod_table;
      while mm <> nil do begin
	with use_table^[m^.mod_num]^[mm^.mod_num] do begin
	  if mm^.called_tag then
	    calling_procs := calling_procs + 1;
	  if mm^.calling_tag then
	    called_procs := called_procs + 1;
	end;
	mm^.calling_tag := false;
	mm^.called_tag := false;
	mm := mm^.rlink;
      end;
      pr := pr^.next_proc;
    end;
    m := m^.rlink;
  end;

end (* const_use_table *);
$PAGE width
(* WIDTH returns the minimum number of character positions required to
   print its integer parameter *)

public function width (val: integer): integer;

var
    absval: integer;

begin
  width := ord (val < 0);
  absval := abs (val);
  repeat
    width := width + 1;
    absval := absval div 10
  until absval = 0;
end;
$PAGE cv_int
(* CV_INT converts an integer to a minimal length string *)

public function cv_int (val: integer): string;

var
    abs_val: integer;
    cv_array: packed array[1..16] of char;
    cv_ix: 0..16;
    cv_len: 0..16;

begin
  if val < 0 then
    cv_int := '-'
  else
    cv_int := '';
  abs_val := abs(val);
  cv_len := width (abs_val);
  for cv_ix := cv_len downto 1 do begin
    cv_array[cv_ix] := chr (abs_val mod 10 + ord ('0'));
    abs_val := abs_val div 10;
  end;
  cv_int := cv_int || cv_array[1:cv_len];
end; (* cv_int *)
$PAGE cvf_int

(* CVF_INT converts an integer to a fixed-length string *)

function cvf_int (val, columns: integer): string;

const
    blanks = '                    ';
    stars  = '********************';

begin
  cvf_int := cv_int (val);
  if length (cvf_int) > columns then
    cvf_int := substr (stars, 1, columns)
  else
    cvf_int := substr (blanks, 1, columns - length (cvf_int)) || cvf_int;
end (* cvf_int *);
$PAGE dump_relations

(*  DumpRelations will create the output cross-reference listing from the
    calls and called_by lists in the symbol table.  *)

procedure dump_relations;

var
    fname: string [40];
    fb: file_block;
    dumping: boolean;
    pr: proc;
    m: mod_list;
    calls: calls_list;
    called: called_list;
    none: mod_list;
    mm, mm1, mm2: mod_tree;
$PAGE print_proc_title - in dump_relations

(*  PrintProcTitle prints the page title information for each page of the cross
    reference listing.  *)

procedure print_proc_title ( var fb: file_block );

begin
  fio_write (fb, 'Cross Reference Listing for ' || title);
  fio_tab (fb, 100 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (fb.pageno));
  fio_skip (fb);
  fio_line (fb, '     Procedure   in             Called By   in       ' ||
		'Calls       in');
  fio_line (fb, '     ----------  ------         ----------  ------   ' ||
		'----------  ------');
  fio_skip (fb);
  if dumping then
    fio_write (fb, '     ' || pr^.proc_name || '  ' || m^.mod_ref^.mod_name ||
	       ' (cont.)');
end (* print_proc_title *);
$PAGE dump_proc_relations

(*  DumpProcRelations dumps the procedural cross reference listing.  *)

procedure dump_proc_relations;

begin
  fb.page_header := fio_proc (print_proc_title);
  dumping := false;
  print_proc_title (fb);
  pr := proc_tree;
  while pr <> nil do begin
    m := pr^.modules;
    while m <> nil do begin
      if pr^.unique
	then fio_write (fb, '     ')
	else fio_write (fb, '  *  ');
      fio_write (fb, pr^.proc_name || '  ' || m^.mod_ref^.mod_name);
      if m^.called_by = nil then
	add_called_by (m, none);
      calls := m^.calls;
      called := m^.called_by;
      dumping := true;
      while (calls <> nil) or (called <> nil) do begin
	if called <> nil then begin
	  fio_tab (fb, 33);
	  fio_write (fb, c^.calling_mod^.owner^.proc_name || '  ' ||
			 called^.calling_mod^.mod_ref^.mod_name);
	  called := called^.next;
	end;
	if calls <> nil then begin
	  fio_tab (fb, 54);
	  fio_write (fb, calls^.called_mod^.owner^.proc_name || '  ' ||
			 calls^.called_mod^.mod_ref^.mod_name);
	  if not calls^.called_mod^.owner^.unique then
	    if calls^.called_mod^.mod_ref^.mod_name = m^.mod_ref^.mod_name
	      then fio_write (fb, '#')
	      else fio_write (fb, '##');
	  calls := calls^.next;
	end;
	fio_skip (fb);
      end (* while (calls <> nil) or (called <> nil) *);
      dumping := false;
      fio_skip (fb);
      m := m^.next;
    end;
    pr := pr^.rlink;
  end;
end (* dump_proc_relations *);
$PAGE print_mod_title - in dump_relations

(*  PrintModTitle prints the title for the module cross-reference listing.  *)

procedure print_mod_title ( var fb: file_block );

begin
  fio_write (fb, 'Module Cross Reference Listing for ' || title);
  fio_tab (fb, 100 - width (fb.pageno));
  fio_line (fb, 'Page ' || cv_int (fb.pageno));
  fio_skip (fb);
  fio_line (fb, '     Module         #Procs  Called  #Procs ' ||
		'     #Procs  Call    #Procs');
  fio_line (fb, '     ------         ------  ------  ------ ' ||
		'     ------  ------  ------');
  fio_skip (fb);
  if dumping then
    fio_write (fb, '     ' || mm^.mod_name || ' (cont.)');
end (* print_mod_title *);
$PAGE dump_mod_relations - in dump_relations

(*  DumpModRelations prints the module cross reference listing.  *)

procedure dump_mod_relations;

begin
  fb.pageno := 0;
  fb.page_header := fio_proc (print_mod_title);
  dumping := false;
  fio_page (fb);

  mm := mod_table;
  while mm <> nil do begin
    fio_write (fb, '     ' || mm^.mod_name);
    mm1 := mod_table;
    mm2 := mod_table;
    dumping := true;
    loop
      while (mm1 <> nil) andif (use_table^[mm^.mod_num]^[mm1^.mod_num].called_procs = 0) do
	mm1 := mm1^.rlink;
      while (mm2 <> nil) andif (use_table^[mm^.mod_num]^[mm2^.mod_num].calling_procs = 0) do
	mm2 := mm2^.rlink;
    exit if (mm1 = nil) andif (mm2 = nil);
      if mm1 <> nil then begin
	fio_tab (fb, 21);
	fio_write (fb, cvf_int (use_table^[mm^.mod_num]^[mm1^.mod_num].called_procs, 6) ||
		       '  ' || mm1^.mod_name || '  ' ||
		       cv_int (use_table^[mm1^.mod_num]^[mm^.mod_num].calling_procs));
	mm1 := mm1^.rlink;
      end;
      if mm2 <> nil then begin
	fio_tab (fb, 49);
	fio_write (fb, cvf_int (use_table^[mm^.mod_num]^[mm2^.mod_num].calling_procs, 6) ||
		       '  ' || mm2^.mod_name || '  ' ||
		       cv_int (use_table^[mm2^.mod_num]^[mm^.mod_num].called_procs));
	mm2 := mm2^.rlink;
      end;
      fio_skip (fb);
    end;
    dumping := false;
    fio_skip (fb);
    mm := mm^.rlink;
  end;
end (* dump_mod_relations *);
$PAGE dump_relations - main routine

begin
  fname := filename (output);
  close (output);
  fio_open (fb, fname);
  fb.plength := 44;
  none := add_module (add_mod_node ('', nil), enter_proc ('<NONE>'));

  dump_proc_relations;

  dump_mod_relations;
end (* dump_relations *);
$PAGE xref - main program

begin
  open (tty);
  rewrite (tty);
  writeln (tty, 'XREF Version 2.0');
  writeln (tty);
  getiofiles (input, output, 'SYM', 'XRF');
  write (tty, 'Title?  ');
  break;
  readln (tty);
  title := '';
  while not eoln (tty) do begin
    title := title || tty^;
    get (tty);
  end;
  build_proc_tree;
  linearize_trees;
  resolve_modules;
  const_called_by_relation;
  const_use_table;
  dump_relations;
end.
5@~Æ