$TITLE pasdmp -- symbol table dump routines
$LENGTH 42

(*   +--------------------------------------------------------------+
     |                                                              |
     |                P A S D M P   -   P a s s   1                 |
     |                - - - - - - - - - - - - - - -                 |
     |                                                              |
     +--------------------------------------------------------------+
     
     MDSI, Company Confidential
     
     SYSTEM:  PASCAL Compiler
     
     STARTED:  5/31/78
     
     PURPOSE:  This is the  debugging  dump  module.  It  contains  a
        collection  of  procedures  to  dump  portions  of the symbol
        table, intermediate form code, etc., to a .DMP file.
     
     ENTRY POINTS:
     
        dump_name_table
                    prints out all the names  in  the  compiler  name
                    table,   in  alphabetical  order.  Each  name  is
                    printed with the address of its name table  node.
                    Statistics about the name table are also printed.
     
        dmpsymbol   produces a formatted dump of a single symbol node
                    (a  SYM  node).  This  dump  includes  the   node
                    address  and  the  symbol  name,  type, and kind;
                    where the symbol was declared; the identification
                    of the containing block; and information specific
                    to this particular kind of symbol.
     
        dmpstable   invokes  dmpsymbol  to  dump  all   the   symbols
                    declared  in  the current block.  The symbols are
                    dumped in alphabetical order.
     
        dmptype     produces a formatted dump of a single  type  node
                    (a   TYP  node).  This  dump  includes  the  node
                    address;  the  type  name  and  kind;  its  size,
                    alignment,    packed,   flexible,   and   generic
                    attributes;  and  information  specific  to  this
                    particular kind of type.
     
        dmpconst    produces  a  formatted  dump of a single constant
                    node (a VAL node).  The dump indicates  the  kind
                    of constant, and the constant value itself.
     
        dmpblock    produces  a formatted dump of a single block node
                    (a BLK node).  The dump identifies the block  and
                    includes all the pointers and miscellaneous items
                    of information that are included in scope block.
     
        dmp_ptree   produces a formatted dump of a  parse  tree.  The
                    dump  contains one line for each parse tree node,
                    with tree structure indicated by indentation  and
                    vertical lines.
     
        dmp_close   will close the dump file, if it is open, and will
                    reset its status flag.
     
     CHANGES:
     
        11/7/78  RNF    PASDMP  split  into  PA1DMP  and  PA2DMP   to
                        facilitate  multi-pass  implementation of the
                        compiler.
     
     RESPONSIBLE:  Compiler Group
     
     ---------------------------------------------------------------- *)

$PAGE includes, externals, forwards

$OPTIONS nocheck, special

$INCLUDE pascal.inc
$INCLUDE pasfil.inc
$INCLUDE pasist.inc
$INCLUDE paspt.typ
$INCLUDE passw.inc
$INCLUDE pascv.inc
$INCLUDE paslex.inc
$INCLUDE passpf.nam
$INCLUDE pasbnf.nam

const
  bits_per_unit = 32;


public procedure dmptype ( node: typ );
forward;

public procedure dmpconst ( con: val );
forward;
$PAGE cv_oct
(* CV_OCT returns a fixed length string containing the octal representation of
   an integer.  Leading zeros are included. *)

function cv_oct ( val: integer; columns: line_index ): parm_string;

var
    i: 0 .. 12;
    digits: packed array [1..12] of char;
    a: integer;

begin
  a := val;
  for i := columns downto 1 do begin
    digits [i] := substr ('01234567', (a mod 8) + 1, 1);
    a := a div 8;
  end;
  cv_oct := substr (digits, 1, columns);
end; (* cv_oct *)
$PAGE start_dump

(*  START_DUMP will open the dump file if it is not currently open, and will
    start a new page on the dump file if the file is open and the 'separator'
    parameter is true.  A new page is simply a line of dashes if TTY has been
    specified as a dump switch.  *)


procedure tty_page ( var fb: file_block );
begin
  fio_skip (fb);
  fio_skip (fb);
  fio_line (fb, '----------------------------------------');
  fio_skip (fb);
  fio_skip (fb);
end;


procedure start_dump ( separator: boolean );

begin
  if df_status = unopened then begin
    fio_open (dumpfb, main_file || ' DSK:[,].DMP');
    if switch (prog_options.dump_switches, 'TTY') then begin
      dumpfb.width := 80;
      dumpfb.new_page := fio_proc (tty_page);
    end
    else begin
      dumpfb.width := 100;
      dumpfb.plength := 45;
    end;
  end
  else begin
    if df_status = prev_opened then
      fio_reopen (dumpfb);
    if separator then
      fio_page (dumpfb);
  end;
  df_status := now_open;
end (* start_dump *);
$PAGE dmp_close

(*  DMP_CLOSE will close the dump file, if it is open, and change its status
    from "now open" to "previously opened".  *)


public procedure dmp_close;

begin
  if df_status = now_open then begin
    fio_close (dumpfb);
    df_status := prev_opened;
  end;
end;
$PAGE prt_type
(*  prt_type prints out a message of the form:

	<msg><name>(<address>)

    if the type node has a name, or a message of the form:

	<msg><address>

    if the type node doesn't have a name.  if the eol flag is true,
    it will all be terminated by a new line.  *)

type msg_parm = string [40];

procedure prt_type
    (	msg: msg_parm;
	node: typ;
	eol:  boolean  );

begin
  fio_write (dumpfb, msg);
  if node = nil then
    fio_write (dumpfb, '*NIL*')
  else
    with node^ do
      if (type_id <> nil) andif (type_id^.name <> nil) then
	with type_id^.name^ do begin
	  if (type_id^.kind <> types) then fio_write (dumpfb, 'TYPE OF ');
	  fio_write (dumpfb, substr(text, 1, len) || '(' ||
			    cv_oct (ord(node), 6) || ')' );
	end
      else
	fio_write (dumpfb, cv_oct (ord(node), 6));
  if eol then
    fio_skip (dumpfb);
end (* prt_type *);
$PAGE prt_blk_num:  print a block number and level
(*  prt_blk_num will print the block number, block level, and
    block node address for a given block.  *)

procedure prt_blk_num ( block: blk );
begin
  if block = nil
    then fio_line (dumpfb,  '*NIL BLOCK*')
    else with block^ do begin
	fio_write (dumpfb, 'BLOCK ' || cv_int(number) || ' AT LEVEL ' ||
			   cv_int(level) || ' (' || cv_oct(ord(block),6) ||
			   '): ');
	case kind of
	    root_blk:
		fio_line (dumpfb, '<ROOT>');
	    program_blk:
		if id = nil
		    then fio_line (dumpfb, '<PROGRAM>')
		    else with id^ do
			fio_line (dumpfb, 'PROGRAM ' || text[1:len]);
	    module_blk:
		if id = nil
		    then fio_line (dumpfb, '<MODULE>')
		    else with id^ do
			fio_line (dumpfb, 'MODULE ' || text[1:len]);
	    data_blk:
		if id = nil
		    then fio_line (dumpfb,  '<DATA MODULE>')
		    else with id^ do
		        fio_line (dumpfb,  'DATA MODULE ' ||  text[1:len]);
	    subr_blk:
		if (subr_sym = nil) orif (subr_sym^.name = nil)
		    then fio_line (dumpfb, '<SUBR>')
		    else with subr_sym^.name^ do
			fio_line (dumpfb, text[1:len]);
	    class_blk:
		prt_type ('CLASS OF ', class_type, true);
	    extern_blk:
		fio_line (dumpfb, '<EXTERNAL>')
	end (* case kind *);
    end (* with block *);
end (* prt_blk_num *);
$PAGE prt_sym
(*  prt_sym will skip the specified number of spaces in the output
    file, and then print a message of the form:

	<address>  <msg><name>/<number>  *)

procedure prt_sym
    (	skip: line_index;
	msg: parm_string;
	symbol: sym;
	eol: boolean  );

begin
    fio_tab (dumpfb, skip);
    fio_write (dumpfb, cv_oct(ord(symbol),6) || '  ' || msg);
    if symbol <> nil then begin
      if symbol^.name = nil
	then fio_write (dumpfb, '<NO NAME>')
	else fio_write (dumpfb, symbol^.name^.text[1:symbol^.name^.len]);
      fio_write (dumpfb, '/' || cv_int (symbol^.id_number));
    end;
    if eol then fio_skip (dumpfb);
end (* prt_sym *);
$PAGE prt_const
(*  prt_const prints a value node.  *)

type vki_type = array[real_cst..record_cst] of string[10];
const val_kind_id: vki_type :=
      ( 'REAL', 'STRING', 'SET', 'POINTER', 'ARRAY', 'RECORD' );

procedure prt_const ( con: val );

var
    ind: int_type;

begin
  if con.kind = scalar_cst then
    fio_line (dumpfb, cv_int(con.ival))
  else if con.kind = no_value then
    fio_line (dumpfb, '*NO VALUE*')
  else if con.kind = subr_cst then
    prt_blk_num (con.blkp)
  else begin
    fio_write (dumpfb, val_kind_id[con.kind] || ' @ ' || cv_oct(ord(con.valp),6) || ' = ');
    with con.valp^ do
      case con.kind of
	real_cst:
	    fio_line (dumpfb, ' ' || cv_real(real_val) ||  ' prec ' ||  cvf_int(real_prec,2));
	string_cst:
	  begin
	    if str_varying_ref
	      then fio_write (dumpfb, ' VAR');
	    fio_line (dumpfb, ' ''' || substr(str_val, 1, str_len) || '''');
	  end;
	set_cst:
	  begin
	    fio_write (dumpfb, cv_int(set_origin) || ':[');
	    for ind := 0 to set_len - 1 do
	      if set_val[ind]
		then fio_write (dumpfb, '1')
		else fio_write (dumpfb, '0');
	    fio_line (dumpfb, ']');
	  end;
	ptr_cst:
	    fio_line (dumpfb, 'NIL');
	array_cst,
	record_cst:
	  begin
	    fio_skip (dumpfb);
	    for ind := 1 to n_elems do
	      dmpconst (elem_vals[ind]);
	    fio_tab (dumpfb, 9);
	    fio_line (dumpfb, 'END ' || val_kind_id[con.kind] || ' @ ' || cv_oct(ord(con.valp),6));
	  end
      end (* case *);
    end;
end (* prt_const *);
$PAGE dump_name_table
(*  dump_name_table will perform a post-order traversal of the
    name tree, printing, for each name in the table, the name itself
    and its address in the table.  *)

public procedure dump_name_table;

type
    cptr = ^ cnode;
    cnode = record  num: int_type; next: cptr end;

var
    count: cptr;
    c: cptr;
    depth_sum: int_type;
    index: int_type;
    name_count: int_type;

    procedure dump_names ( root: nam;		(* recursive tree traversal *)
			   depth: int_type;
			   count: cptr );
    begin
	with root^, count^ do begin
	    if next = nil then begin
		new (next);
		next^.num := 0;
		next^.next := nil;
	    end;
	    if alink <> nil then dump_names (alink,depth+1,next);
	    fio_line (dumpfb, cv_oct(ord(root),6) || ': ' || cvf_int(depth,5) ||
			      cv_oct(ord(alink),6) || ' ' || cv_oct(ord(zlink),6) ||
			      ' ' || substr (text, 1, len) );
	    num := num + 1;
	    name_count := name_count + 1;
	    depth_sum := depth_sum + depth;
	    if zlink <> nil then dump_names (zlink,depth+1,next);
	end;
    end (* dump_names *);

begin
    start_dump (true);
    fio_line (dumpfb, 'NAME TABLE');
    fio_line (dumpfb, '---- -----');
    fio_skip (dumpfb);
    fio_skip (dumpfb);
    fio_line (dumpfb, 'ADDRESS DEPTH  ALINK  ZLINK NAME');
    fio_line (dumpfb, '------- -----  -----  ----- ----');
    fio_skip (dumpfb);
    new (count);
    count^.num := 0;
    count^.next := nil;
    depth_sum := 0;
    name_count := 0;
    dump_names (root_name,0,count);
    fio_skip (dumpfb);
    fio_line (dumpfb, cv_int(name_count) || ' NAMES');
    index := 0;
    while count <> nil do begin
	fio_line (dumpfb, cv_int(count^.num) || ' NAMES AT DEPTH ' ||
		  cv_int (index) || ', SATURATION = ' ||
		  cv_real ( float (count^.num) / ( float (2**index))));
	index := index + 1;
	c := count;
	count := count^.next;
	dispose (c);
    end;
    fio_line (dumpfb, 'AVERAGE DEPTH = ' || cv_real( float (depth_sum) / float (name_count)));
end (* dump_name_table *);
$PAGE dmpsymbol:  dump a symbol table node
(*  dmpsymbol will produce a formatted dump of a symbol node.  the
    dump will always include the node address, symbol name, declaration,
    containing block, type, and kind of symbol.  information specific
    to the symbol kind will also be printed.  *)

public procedure dmpsymbol ( symbol: sym );

type ski_type = array [sym_kind] of string [20];
const sym_kind_id: ski_type :=
     (  'LABEL', 'FIELD', 'TYPE', 'CONSTANT', 'VARIABLE', 'VALUE',
	'FOR LOOP INDEX', 'STANDARD PROCEDURE', 'STANDARD FUNCTION',
	'CONDITION', 'BLOCK'  );

type dci_type = array [storage_class] of string [11];
const dcl_class_id: dci_type :=
     (  'LOCAL', 'PARAMETER', 'STATIC', 'CONSTANT', 'EXTERNAL', 'DYNAMIC',
	'FILE BLOCK', 'OPT', 'CODE', 'ABSOLUTE', 'TEMP', 'REGISTER', 'RUNTIME',
	'DEF', 'SELF', 'UNALLOCATED'  );

begin
  start_dump (false);
  fio_skip (dumpfb);
  if symbol = nil then begin
    fio_line (dumpfb, '********  NIL SYMBOL');
    return;
  end;
  with symbol^ do begin
    prt_sym (0,'',symbol,false);
    fio_write (dumpfb, ': ' || sym_kind_id[kind] ||  ', IN ');
    prt_blk_num (block);
    prt_type ('        TYPE = ',type_desc,false);
    fio_line (dumpfb, ', NEXT = ' || cv_oct(ord(next),6) ||
		      ', SCOPECHAIN = ' || cv_oct(ord(scopechain),6));
    case kind of

      labels:
	begin
	  fio_tab (dumpfb, 9);
	  fio_write (dumpfb, 'DECLARED AT LINE ' || cv_source_id(lab_declaration));
	  if lab_defined
	    then fio_line (dumpfb, ', DEFINED')
	    else fio_skip (dumpfb);
	end;

      fields:
	begin
	  prt_type ('        FIELD '||cv_int(fld_number)||' OF RECORD ',  fld_record,false);
	  prt_type (', IN VARIANT ', fld_variant, false);
	  fio_line (dumpfb, ' OFFSET= ' || cv_int(fld_offset div bits_per_unit) ||
			    '+' || cv_int(fld_offset mod bits_per_unit) ||
			    ', WIDTH= ' || cv_int(fld_width));
       end;

      consts,
      vars,
      values,
      for_inds,
      conditions:
	begin
	  fio_tab (dumpfb, 9);
	  if public_dcl then
	    fio_write (dumpfb, 'PUBLIC, ');
	  fio_write (dumpfb, 'STORAGE CLASS = ' || dcl_class_id [dcl_class]);
	  if standard then
	    fio_write (dumpfb, ', STANDARD');
	  if maskable then
	    fio_write (dumpfb, ', MASKABLE');
	  fio_write (dumpfb, ', VALUE = ');
	  prt_const (init_value);
	  fio_tab (dumpfb, 9);
	  if abnormal_use then
	    fio_write (dumpfb, 'ABNORMAL, ');
	  if allocated then
	    fio_write (dumpfb, 'ALLOCATED, ');
	  fio_line (dumpfb, 'ADDR = ' || cv_int(item_addr));
	end;

      std_procs,
      std_funcs:
	begin
	  fio_tab (dumpfb, 9);
	  fio_line (dumpfb, sym_kind_id [kind] || ' ' || std_pf_id [std_pf_code]);
	end

    end (* case kind *);
    if (type_desc <> nil) andif
       ((type_desc^.type_id = nil) or (type_desc^.type_id = symbol)) then
	  dmptype (type_desc);
  end (* with symbol *);
end (* dmpsymbol *);
$PAGE dmpstable:  dump all the symbols in a block
(*  dmpstable will traverse the name table, and for each name,
    will check if there is a symbol with that name in the specified
    block, and if so, will dump it with dmpsymbol.  *)

public procedure dmpstable ( block: blk );

    procedure traverse (rt:nam);		(* do the name tree traversal *)
    var
	root: nam;
	symbol: sym;
    begin
	root := rt;
	while root <> nil do
	    with root^ do begin
		if alink <> nil then traverse (alink);
		symbol := scopechain;
		while (symbol <> nil) andif (symbol^.block <> block) do
		    symbol := symbol^.scopechain;
		if (symbol <> nil) andif (symbol^.block = block) then
		    dmpsymbol (symbol);
		root := zlink;
	    end;
    end (* traverse *);

begin
    start_dump (true);
    fio_write (dumpfb, 'SYMBOL TABLE FOR ');
    prt_blk_num (block);
    fio_line (dumpfb, '------ -----');
    fio_skip (dumpfb);
    traverse (root_name);
end (* dmpstable *);
$PAGE dmptype:  display a type node
(*  dmptype will produce a formatted dump of a type node.  the dump will
    always include the node address, packed and flexible attributes, and
    kind of type node.  information specific to the type node kind will
    also be printed.  *)

type tpki_type = array [type_kind] of string [10];
const tp_kind_id: tpki_type :=
      ( 'SCALAR', 'BOOLEAN', 'CHAR', 'INTEGER', 'REAL', 'SET', 'POINTER',
	'FILE', 'STRING', 'ARRAY', 'RECORD', 'VARIANT', 'TAG',
	'PROCEDURE', 'FUNCTION', '<UNKNOWN>', '<INDIRECT>' );

type mode_name_type = array [file_modes] of string [10];
const mode_name: mode_name_type :=
      ( 'TEXT', 'TYPED', 'BINARY', 'ANY' );

public procedure dmptype (* node: typ *);

var
    csym: sym;
    var_ptr: typ;  last_var: typ;
    i: parm_range;


  procedure dmpfields ( rv: typ );
   var csym: sym;
   begin
    with rv^ do begin
      csym := field_list;
      while (csym <> nil) andif (csym^.fld_variant = rv) do begin
	dmpsymbol (csym);
	csym := csym^.next;
      end;
      if variant_tag <> nil then dmptype (variant_tag);
      fio_skip (dumpfb);
      fio_line (dumpfb, cv_oct(ord(rv),6) || ': END ' || tp_kind_id [kind]);
    end
   end;
begin
  start_dump (false);
  fio_skip (dumpfb);
  if node = nil then begin
    fio_line (dumpfb, '********   NIL TYPE');
    return;
  end;
  with node^ do begin
    fio_write (dumpfb, cv_oct(ord(node),6) || '  ');
    if (type_id <> nil) andif (type_id^.name <> nil) then
      with type_id^.name^ do
	fio_write (dumpfb, text[1:len] || ': ');
    fio_write (dumpfb, tp_kind_id[kind] || ' TYPE,' ||
		       ' SIZE= ' || cv_int(size div bits_per_unit) ||
		       '+' || cv_int(size mod bits_per_unit) ||
		       ', ALIGNED MOD ' || cv_int(alignment));
    if packable then
      fio_write (dumpfb, ', PACKED');
    if flexible then
      fio_write (dumpfb, ', FLEXIBLE');
    if generic then
      fio_write (dumpfb, ', GENERIC');
    fio_skip (dumpfb);
    fio_tab (dumpfb, 9);
    fio_write (dumpfb, tp_kind_id [kind] || ' ');
    case kind of

      bools,
      ints,
      chars,
      scalars:
	begin
	  prt_type (cv_int(minval)||' .. '||cv_int(maxval)||
		    ' OF BASE TYPE ',base_type,false);
	  if (kind inls,scalars]) and (base_type = node) then begin
	    fio_line (dumpfb, ' WITH ELEMENTS:');
	    csym := cst_list.first;
	    while csym <> nil do begin
	      prt_sym (10,'',csym,true);
	    exit if csym = cst_list.last;
	      csym := csym^.next;
	    end;
	  end
	  else
	    fio_skip (dumpfb);
	end;

      reals:
	begin
	  fio_write (dumpfb, cv_real(rminval) || ' .. ' || cv_real(rmaxval));
	  fio_line (dumpfb, ' PREC ' || cv_int(precision));
	end;

      sets:
	begin
	  prt_type ('OF TYPE ',set_element_type,true);
	  if (set_element_type <> nil) andif (set_element_type^.type_id = nil) then
	    dmptype (set_element_type);
	end;

      pointers:
	begin
	  prt_type ('TO TYPE ',target_type,true);
	  if (target_type <> nil) andif (target_type^.type_id = nil) then
	    dmptype (target_type);
	end;

      arrays:
	begin
	  prt_type ('[TYPE ',index_type,false);
	  prt_type ('] OF TYPE ',element_type,false);
	  fio_line (dumpfb, ', ELEMENT SIZE = ' || cv_int (element_size));
	  if (index_type <> nil) andif (index_type^.type_id = nil) then
	    dmptype (index_type);
	  if (element_type <> nil) andif (element_type^.type_id = nil) then
	    dmptype (element_type);
	end;

      files:
	begin
	  fio_write (dumpfb, '[' || mode_name [file_kind] || '] ');
	  prt_type ('OF TYPE ',component_type,true);
	  if (component_type <> nil) andif (component_type^.type_id = nil) then
	    dmptype (component_type);
	end;

      strings:
	  if str_kind = varying
	      then fio_line (dumpfb, '[' || cv_int(str_length) || '] VARYING')
	      else fio_line (dumpfb, '[' || cv_int(str_length) || '] NONVARYING');

      records:
	begin
	  fio_line (dumpfb, ', WITH FIELDS:');
	  dmpfields (node);
	end;

      variants:
	begin
	  if others_var
	    then fio_write (dumpfb, 'OTHERS')
	    else fio_write (dumpfb, cv_int (minlab) || ' .. ' || cv_int (maxlab));
	  fio_write (dumpfb, ' OF TAG ' || cv_oct(ord(tag),6));
	end;


      tags:
	begin
	  prt_type ('TYPE ',tag_type,false);
	  prt_type (' IN RECORD ',tag_recvar,true);
	  prt_sym (10,'TAG NAME ',tag_field,true);
	  var_ptr := first_variant;
	  while var_ptr <> nil do begin
	    last_var := var_ptr;
	    loop
	      dmptype (var_ptr);
	      var_ptr := var_ptr^.next_variant;
	    exit if (var_ptr = nil) orif (var_ptr^.field_list <> last_var^.field_list);
	      fio_skip (dumpfb);
	    end;
	    fio_line (dumpfb, ', WITH FIELDS:');
	    dmpfields (last_var);
	  end;
	end;

      procs,
      funcs:
	begin
	  if fortran_call then fio_write (dumpfb, '(FORTRAN) ');
	  if kind = funcs then begin
	    prt_type ('RETURNS TYPE ',return_type,false);
	    fio_write (dumpfb, ' ');
	  end;
	  fio_line (dumpfb, 'WITH ' || cv_int(nparms) || ' PARAMETERS:');
	  for i := 1 to nparms do
	    if params[i].parm_kind = values
	      then prt_type ('VALUE ', params[i].parm_type, true)
	      else prt_type ('VAR   ', params[i].parm_type, true);
	end;

      indirect_type:
	begin
	  prt_type ('TO ACTUAL TYPE ',actual_type,true);
	  if (actual_type <> nil) andif
	    ((actual_type^.type_id = nil) or (actual_type^.type_id = type_id))
	      then dmptype (actual_type);
	end;

      others:
	  fio_skip (dumpfb)

    end (* case *);
  end (* with node *);
end (* dmptype *);
$PAGE dmpconst:  dump a constant table node
public procedure dmpconst (* con: val *);

begin
  start_dump (false);
  fio_tab (dumpfb, 9);
  prt_const (con);
end (* dmp_const *);
$PAGE dmpblock: dump a block node
public procedure dmpblock ( block: blk );

    procedure dump_list (list:sym_list;description:parm_string);
    var symbol: sym;
    begin
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, description || ' (' || cv_oct(ord(list.first),6) ||
			  ', ' || cv_oct(ord(list.last),6) || '):');
	symbol := list.first;
	while symbol <> nil do begin
	  prt_sym (10,'',symbol,true);
	exit if symbol = list.last;
	  symbol := symbol^.next;
	end;
    end (* dump_list *);

type on_type = array [optionlist] of string [10];
const opt_names: on_type :=
      ( 'CHECK ASS', 'CHECK CAS', 'CHECK COM', 'CHECK FIE', 'CHECK FIL', 'CHECK INP',
	'CHECK POI', 'CHECK STR', 'CHECK SUB', 'CHECK VAL', 'MAP', 'SYMBOLS', 'CALLS',
	'ASSEMBLY', 'XREF', 'SPEC COE', 'SPEC PTR', 'SPEC WOR', 'TRACE',
	'QBLOCKS', 'OPTIMIZE' );

var
    any: boolean;
    opt: optionlist;
    sw: switch_ptr;

begin
    start_dump (true);
    with block^ do begin
	prt_blk_num (block);
	fio_line (dumpfb,  '-----');
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, 'PARENT = ' || cv_oct(ord(parent),6) ||
			  ', PEER = ' || cv_oct(ord(peer),6) ||
			  ', CHILD = ' || cv_oct(ord(children),6));
	if (kind = subr_blk) andif forward_dcl then begin
	  fio_tab (dumpfb, 9);
	  fio_line (dumpfb, 'FORWARD');
	end;
	fio_tab (dumpfb, 9);
	fio_line (dumpfb, 'RETURN SYMBOL =' || cv_oct(ord(return_sym),6));
	if return_sym <> nil then dmpsymbol (return_sym);
	dump_list (parm_list,'PARAMETERS');
	dump_list (label_list,'LABELS');
	dump_list (type_list,'TYPE SYMBOLS');
	dump_list (id_list,'IDENTIFIERS');
	fio_skip (dumpfb);
	fio_tab (dumpfb, 9);
	fio_write (dumpfb, 'OPTIONS = [');
	any := false;
	for opt := minimum (optionlist) to maximum (optionlist) do
	    if opt in semantic_options then begin
		if any then fio_write (dumpfb, ', ');
		any := true;
		fio_write (dumpfb, opt_names[opt]);
	    end;
	if not any then fio_write (dumpfb, ' ');
	fio_line (dumpfb, ']');
	fio_write (dumpfb, 'DUMP = (');
	sw := dump_switches;
	while sw <> nil do begin
	    if sw <> dump_switches then fio_write (dumpfb, ', ');
	    fio_write (dumpfb, sw^.name);
	    if sw^.enabled then fio_write (dumpfb, '!');
	    sw := sw^.next_switch;
	end;
	if dump_switches = nil then fio_write (dumpfb, ' ');
	fio_line (dumpfb, ')');
    end;
end (* dmpblock *);
$PAGE dmp_ptree
type op_name_vec = array [operators] of string [10];

const op_names: op_name_vec =
	( '*', '/', 'DIV', '**', 'MOD', 'AND', 'ANDIF', '+', '-', 'OR',
	  'ORIF', '||', '<=', '<', '>', '>=', '=', '<>', 'IN',
	  'READ', 'WRITE', 'READLN', 'WRITELN', 'READRN', 'WRITERN',
	  'GETSTRING', 'PUTSTRING', '*NOP*' );


public procedure dmp_ptree ( node: parse_node );

  procedure do_dump ( node: parse_node; level: line_index );

    procedure indent;
    var i: line_index;
    begin
      for i := 1 to level do fio_write (dumpfb, '| ');
    end;

    procedure display_node;
    begin
      with node^ do begin
	fio_write (dumpfb, symbol_names [sym]);
	if dummy then fio_write (dumpfb, ' (dummy)')
	else begin
	  fio_write (dumpfb, ' (on line ' || cv_source_id (source));
	  fio_write (dumpfb, ' || col ' || cv_int (column) || ')');
	end;
	if sym in [ident, intconst, realconst, stringconst,
		   notsy, powerop, mulop, addop, relop, iosy] then
	  fio_write (dumpfb, ': ');
	case sym of
	  ident:
	    if name = nil then
	      fio_write (dumpfb, 'NO NAME')
	    else
	      with name^ do
		fio_write (dumpfb, 'NAME = ' || substr(text, 1, len));
	  intconst:
	    fio_write (dumpfb, 'VALUE = ' || cv_int(value.ival));
	  realconst:
	    with value.valp^ do
	      fio_write (dumpfb, 'VALUE = ' || cv_real(real_val) ||
				 ' PREC ' || cv_int(real_prec));
	  stringconst:
	    if value.kind = scalar_cst then
	      fio_write (dumpfb, 'VALUE = CHAR ''' || chr(value.ival) || '''')
	    else
	      with value.valp^ do
		fio_write (dumpfb, 'VALUE = ''' || substr(str_val, 1, str_len) || '''');
	  notsy,
	  powerop,
	  mulop,
	  addop,
	  relop,
	  iosy:
	    fio_write (dumpfb, 'OPERATOR = ' || op_names[op])
	end (* case *);
      end (* with node^ *);
      fio_skip (dumpfb);
    end (* display_node *);

  var def: parse_node;

  begin (* do_dump *);
    indent;
    fio_skip (dumpfb);
    indent;
    display_node;
    def := node^.defn;
    while def <> nil do begin
      do_dump (def,level+1);
      def := def^.next;
    end;
  end (* do_dump *);

begin (* dump_parse_table *);
  if node = nil then return;
  start_dump (true);
  do_dump (node,0);
end (* dump_parse_table *).
>@|å