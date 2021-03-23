$TITLE pasdmp -- dump routines common to both passes 1 and 2
$LENGTH 42

module pasdmp;
$PAGE includes
$SYSTEM pascal.inc
$SYSTEM pasfil.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM paspt.typ
$SYSTEM pasif.typ
$SYSTEM pasopt.typ
$SYSTEM pascv.inc
$SYSTEM passw.inc
$SYSTEM pasifu.inc
$SYSTEM passet.inc
$SYSTEM pasbnf.nam
$PAGE dmp_open

(*  DMP_OPEN will open the dump file if it is not currently open, and will
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


public procedure dmp_open ( separator: boolean );
begin
  if df_status = unopened then begin
    fio_open (dumpfb, main_file || ' DSK:[,].DMP');
    if switch (prog_options.dump_switches, 'TTY') then begin
      dumpfb.width := 80;
      dumpfb.new_page := tty_page;
    end
    else begin
      dumpfb.width := 100;
      dumpfb.plength := 44;
    end;
  end
  else begin
    if df_status = prev_opened then
      fio_reopen (dumpfb);
    if switch (prog_options.dump_switches, 'TTY')
      then dumpfb.new_page := tty_page
      else dumpfb.new_page := fio_eject;
    dumpfb.page_header := fio_nop;
    if separator then
      fio_page (dumpfb);
  end;
  df_status := now_open;
end (* dmp_open *);
$PAGE cv_ptr
  
(* CvPtr will return a string representing an address in octal. *)
  
public function cv_ptr (ptr_val: integer): packed array [1..6] of char;
  
begin
  putstring (cv_ptr, ptr_val:6:o)
end;
$PAGE sym_text

(*  SymName will return the name of a symbol, if the symbol has a name.  If
    the symbol is a heap class, then 'Class of <type>' will be returned.
    Otherwise, the symbol's address will be returned, with an '@'.  *)


public function typ_text ( t: typ ): line_string;
  forward;


public function sym_text ( s: sym ): line_string;

begin
  if s = nil then
    sym_text := '*NIL*'
  else
  with s^ do
    if name <> nil then
      sym_text := name^.text
    else if dcl_class = dynamic_sc then
      sym_text := 'Heap class of ' || typ_text (type_desc)
    else if dcl_class = fileblk_sc then
      sym_text := 'File class of ' || typ_text (type_desc)
    else if kind in [vars, labels] then
      sym_text := 'V.' || cv_int (id_number)
    else
      sym_text := 'C.' || cv_int (id_number);
end (* sym_text *);
$PAGE typ_text

(*  TypText is called with a pointer to a type node, and returns the
    type name, if it has a name; 'TYPE OF <symbol>', if it is an
    unnamed scalar type; and the octal pointer value if all else fails.  *)


function typ_text (* t: typ ): line_string *);

begin
  if t = nil then
    typ_text := '*NIL*'
  else
    with t^ do begin
      if type_id <> nil then begin
	typ_text := sym_text (type_id);
	if type_id^.kind <> types then
	  typ_text := 'Type of ' || typ_text;
      end
      else
	typ_text := '@' || cv_ptr (ord (t));
    end (* with t^ *);
end (* typ_text *);
$PAGE block_id

(*  BlockId will return the block number, level, type, and name of a block node.  *)


public function block_id ( block: blk ): line_string;

type
    blk_names = array [block_kind] of string [11];

const
    block_kind: blk_names =
     (  'ROOT', 'PROGRAM', 'MODULE', 'SUBROUTINE', 'CLASS', 'EXTERNAL'  );

begin
  if block = nil then
    block_id := '*NIL*'
  else
    with block^ do begin
      block_id := cv_int (number) || ' AT LEVEL ' || cv_int (level) ||
		  ':  ' || block_kind [kind] || ' ';
      case kind of

	root_blk:
	  ;

	program_blk,
	module_blk:
	  if id <> nil then
	    block_id := block_id || id^.text;

	subr_blk:
	  block_id := block_id || sym_text (subr_sym);

	class_blk:
	  block_id := block_id || typ_text (class_type);

	others:
	  (* no action *)

      end (* case kind *);
    end (* with case^ *);
end (* block_id *);
$PAGE prt_title

(*  PrtTitle is called with a string.  It prints the string, centered,
    underlined, and followed by a blank line.  In addition, if the dump
    mode is not "TTY", then the new-page routine is set to print the
    title and a page number at the start of each new page.  *)


var saved_title: line_string;


procedure title_page ( var fb: file_block );
var col: line_index;
begin
  fio_write (fb, saved_title);
  col := fb.width - 5 - width(fb.pageno);
  if fb.column >= col
    then fio_skip (fb)
    else fio_tab (fb, col+1);
  fio_line (fb, 'PAGE ' || cv_int(fb.pageno));
  fio_skip (fb);
end;


public procedure prt_title ( ttl: line_string );

var
    dashes: line_string;
    i: line_index;

begin
  dmp_open (true);
  i := search (ttl, ['$']);
  if i = 0
    then saved_title := ttl
    else saved_title := substr (ttl, 1, i-1) || block_id (cur_block) || substr (ttl, i+1);
  i := max (0, (dumpfb.width - length(saved_title)) div 2) + 1;
  dumpfb.c_column := 0;
  fio_tab (dumpfb, i);
  fio_line (dumpfb, saved_title);
  fio_tab (dumpfb, i);
  dumpfb.page_header := title_page;
  dumpfb.pageno := 1;
  dashes := saved_title;
  for i := 1 to length (dashes) do
    if dashes [i] <> ' ' then
      dashes [i] := '-';
  fio_line (dumpfb, dashes);
  fio_skip (dumpfb);
end (* prt_title *);
$PAGE dmp_close

(*  DMP_CLOSE will close the dump file, if it is open, and change its status
    from "now open" to "previously opened".  *)

public procedure dmp_close;

begin
  if df_status = now_open then begin
    fio_close (dumpfb);
    df_status := prev_opened;
  end;
end.
