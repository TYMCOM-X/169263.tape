$TITLE RELDMP - Relocatable Binary File Dump Program

program reldmp;

$SYSTEM cmdutl
$SYSTEM fio

var relfile: file of *;

type
    line = string [120];

    line_index = 0 .. upperbound (line);

    dump_options =
     (  opt_ascii, opt_decimal, opt_floating, opt_octal, opt_symbolic,
	opt_sixbit, opt_radix50, opt_help, opt_exit );

    option_set = set of dump_options;

    option_list = array [1..11] of cmd_lookup_record;

const
    dump_names: option_list =
     (  ( 'ASCII     ', 1 , ord (opt_ascii)),
	( 'DECIMAL   ', 1 , ord (opt_decimal)),
	( 'FLOATING  ', 1 , ord (opt_floating)),
	( 'OCTAL     ', 1 , ord (opt_octal)),
	( 'SYMBOLIC  ', 1 , ord (opt_symbolic)),
	( 'SIXBIT    ', 2 , ord (opt_sixbit)),
	( '6         ', 1 , ord (opt_sixbit)),
	( 'RADIX50   ', 1 , ord (opt_radix50)),
	( '5         ', 1 , ord (opt_radix50)),
	( 'HELP      ', 1 , ord (opt_help)),
	( 'EXIT      ', 1 , ord (opt_exit)) );

    standard_defaults: option_set = [opt_symbolic, opt_octal];

var default_options: option_set;
$PAGE print_help
procedure print_help;

begin
  writeln (tty, 'Command format: [<output file>=]<input file>[/<switches>]');
  writeln (tty, 'Input default:  .REL');
  writeln (tty, 'Output default:  Terminal (if omitted completely), or .LST');
  writeln (tty, 'Dump mode switches:');
  writeln (tty, '   ASCII, DECIMAL, FLOATING, OCTAL, SYMBOLIC,');
  writeln (tty, '   SIXBIT or 6, RADIX50 or 5');
  writeln (tty, 'Other switches:');
  writeln (tty, '   HELP, EXIT');
  writeln (tty, 'Default switches: /SYMBOLIC /OCTAL');
  writeln (tty, 'Any unique abbreviation of a switch may be used');
end (* print_help *);
$PAGE get_command
procedure get_command ( var in_file, out_file: file_name; var switches: option_set );

var command: line;
    i, opt_select: integer;
    parsed: boolean;

  function get_file_name ( var name: file_name; mode: string [6] ): boolean;
  begin
    get_file_name := cmd_file_name (command, i, true, name);
    if not get_file_name then
      writeln (tty, '?Bad ', mode, ' file name.  Type /HELP for help.');
  end;

begin
  repeat
    parsed := true;
    cmd_getline ('*', command, i);

    if command = '' then
      parsed := false
    else if cmd_check_punct (command, i, '=') then begin
      out_file := '';
      parsed := get_file_name (in_file, 'input');
    end
    else if cmd_check_punct (command, i, '/') then begin
      in_file := '';
      out_file := '';
      i := i - 1;
    end
    else begin
      if get_file_name (out_file, 'output') then begin
	if cmd_check_punct (command, i, '=') then
	  parsed := get_file_name (in_file, 'input')
	else begin
	  in_file := out_file;
	  out_file := 'TTY:';
	end;
      end
      else
	parsed := false;
    end;

    if parsed andif cmd_check_punct (command, i, '/') then begin
      switches := [];
      repeat
	exit if not cmd_lookup (command, i, ['A'..'Z'], dump_names, opt_select) do begin
	  writeln (tty, '?Bad switch.  Type /HELP for help.');
	  parsed := false;
	end;
	switches := switches + [dump_options (opt_select)];
      until not cmd_check_punct (command, i, '/');
    end
    else
      switches := default_options;

    if parsed andif not cmd_eol (command, i) then begin
      writeln (tty, '?Bad command syntax.  Type /HELP for help.');
      parsed := false;
    end;
  until parsed;
end (* get_command *);
$PAGE open_files
function open_files ( in_file, out_file: file_name ): boolean;

begin
  reset (relfile, '.REL ' || in_file);
  if iostatus <> io_ok then begin
    writeln (tty, '?Unable to open rel file ', in_file);
    open_files := false;
    close (relfile);
  end
  else begin
    rewrite (output, filename (relfile) || '.LST[,]' || out_file);
    if iostatus <> io_ok then begin
      writeln (tty, '?Unable to open output file ', out_file);
      open_files := false;
      close (output);
      close (relfile);
    end
    else
      open_files := true;
  end;
end (* open_files *);
$PAGE dump_it
procedure dump_it ( switches: option_set ) options special (word);
type
    relocation_syllable = ( none, right, left, both );

    relocation_word = packed array [0..17] of relocation_syllable;

    halfword = 0 .. 777777b;

    header_word = packed record
	block_type, block_count: halfword;
    end;

    ascii_word = packed array [1..5] of char;

    decimal_word = machine_word;

    floating_word = real;

    octal_word = packed record
	left, right: halfword;
    end;

    symbolic_word = packed record
	opcode: 0 .. 777b;
	acc: 0 .. 17b;
	indirect: boolean;
	index: 0 .. 17b;
	offset: halfword;
    end;

    sixbit_char = 0 .. 77b;

    sixbit_word = packed array [1..6] of sixbit_char;

    r50_text = 0 .. 37777777777b;

    radix50_word = packed record
	code: 0 .. 17b;
	text: r50_text;
    end;
$PAGE
type
    overlay_word = record
	case integer of
	  0: ( relocation: relocation_word );
	  1: ( header: header_word );
	  2: ( ascii: ascii_word );
	  3: ( decimal: decimal_word );
	  4: ( floating: floating_word );
	  5: ( octal: octal_word );
	  6: ( symbolic: symbolic_word );
	  7: ( sixbit: sixbit_word );
	  8: ( radix50: radix50_word );
    end;

    count_kind = ( short_count, long_count );

const
    rel_flag: array [boolean] of char = ( ' ', '''' );

label 999; (* premature end of file *)

var listfb: file_block;
$PAGE get_a_word
function get_a_word : overlay_word;

begin
  if eof (relfile) then begin
    writeln (tty, '%Premature EOF in rel file');
    fio_skip (listfb);
    fio_skip (listfb);
    fio_line (listfb, ' ***  END-OF-FILE ***');
    goto 999;
  end;
  read (relfile, get_a_word);
end;
$PAGE check_count
function check_count ( name: packed array [1..*] of char;
			count, lowlim, highlim: integer ): boolean;

begin
  if count < lowlim then begin
    fio_line (listfb, ' ***  COUNT TOO SMALL  ***');
    writeln (tty, '%', name, ' record size <', lowlim);
    check_count := false;
  end
  else if count > highlim then begin
    fio_line (listfb, ' ***  COUNT TOO LARGE  ***');
    writeln (tty, '%', name, ' record size >', highlim);
    check_count := false;
  end
  else
    check_count := true;
end;
$PAGE dmp_header
procedure dmp_header ( var fb: file_block );

var pagenum: string [10];

begin
  fio_write (fb, 'Dump of Relocatable Binary File ' || filename (relfile));
  putstring (pagenum, fb.pageno: 0);
  fio_tab (fb, 75 - length (pagenum));
  fio_line (fb, 'Page ' || pagenum);
  fio_skip (fb);
end (* dmp_header *);
$PAGE text conversion functions
function ascii_text ( word: ascii_word ): line;
var i: 1 .. 5;
begin
  ascii_text := word;
  for i := 1 to 5 do
    if (ascii_text [i] < ' ') or (ascii_text [i] > '~') then
      ascii_text [i] := '~';
end;


function decimal_text ( word: decimal_word ): line;
begin
  putstring (decimal_text, word: 12);
end;


function floating_text ( word: floating_word ): line;
begin
  putstring (floating_text, word);
  while length (floating_text) < 14 do
    floating_text := floating_text || ' ';
end;


function octal_text ( word: octal_word; relocation: relocation_syllable ): line;
begin
  putstring (octal_text, word.left: 6: o, rel_flag [relocation in [left, both]], ' ',
			 word.right: 6: o, rel_flag [relocation in [right, both]]);
end;


function left_text ( word: octal_word; relocation: relocation_syllable ): line;
begin
  putstring (left_text, word.left: 6: o, rel_flag [relocation in [left, both]]);
end;


function right_text ( word: octal_word; relocation: relocation_syllable ): line;
begin
  putstring (right_text, word.right: 6: o, rel_flag [relocation in [right, both]]);
end;
$PAGE
function symbolic_text ( word: symbolic_word; relocation: relocation_syllable ): line;

var name: packed array [1..6] of char;
    index_field: packed array [1..4] of char;

const
    indirect_flag: array [boolean] of char = ( ' ', '@' );

    opcodes: array [0..700b div 10] of packed array [1..60] of char =
      ( 'Z     001   002   003   004   005   006   007   010   011   ',
	'012   013   014   015   016   017   020   021   022   023   ',
	'024   025   026   027   030   031   032   033   034   035   ',
	'036   037   CALL  INIT  042   043   044   045   046   CALLI ',
	'OPEN  TTCALL052   053   054   RENAMEIN    OUT   SETSTSSTATO ',
	'STATUSSTATZ INBUF OUTBUFINPUT OUTPUTCLOSE RELEASMTAPE UGETF ',
	'USETI USETO LOOKUPENTER UJEN  101   102   103   104   ADJSP ',
	'106   107   DFAD  DFSB  DFMP  DFDV  DADD  DSUB  DMUL  DDIV  ',
	'DMOVE DMOVN FIX   123   DMOVEMDMOVNMFIXR  FLTR  UFA   DFN   ',
	'FSC   IBP   ILDB  LDB   IDPB  DPB   FAD   FADL  FADM  FADB  ',
	'FADR  FADRI FADRM FADRB FSB   FSBL  FSBM  FSBB  FSBR  FSBRI ',
	'FSBRM FSBRB FMP   FMPL  FMPM  FMPB  FMPR  FMPRI FMPRM FMPRB ',
	'FDV   FDVL  FDVM  FDVB  FDVR  FDVRI FDVRM FDVRB MOVE  MOVEI ',
	'MOVEM MOVES MOVS  MOVSI MOVSM MOVSS MOVN  MOVNI MOVNM MOVNS ',
	'MOVM  MOVMI MOVMM MOVMS IMUL  IMULI IMULM IMULB MUL   MULI  ',
	'MULM  MULB  IDIV  IDIVI IDIVM IDIVB DIV   DIVI  DIVM  DIVB  ',
	'ASH   ROT   LSH   JFFO  ASHC  ROTC  LSHC  247   EXCH  BLT   ',
	'AOBJP AOBJN JRST  JFCL  XCT   257   PUSHJ PUSH  POP   POPJ  ',
	'JSR   JSP   JSA   JRA   ADD   ADDI  ADDM  ADDB  SUB   SUBI  ',
	'SUBM  SUBB  CAI   CAIL  CAIE  CAILE CAIA  CAIGE CAIN  CAIG  ',
	'CAM   CAML  CAME  CAMLE CAMA  CAMGE CAMN  CAMG  JUMP  JUMPL ',
	'JUMPE JUMPLEJUMPA JUMPGEJUMPN JUMPG SKIP  SKIPL SKIPE SKIPLE',
	'SKIPA SKIPGESKIPN SKIPG AOJ   AOJL  AOJE  AOJLE AOJA  AOJGE ',
	'AOJN  AOJG  AOS   AOSL  AOSE  AOSLE AOSA  AOSGE AOSN  AOSG  ',
	'SOJ   SOJL  SOJE  SOJLE SOJA  SOJGE SOJN  SOJG  SOS   SOSL  ',
	'SOSE  SOSLE SOSA  SOSGE SOSN  SOSG  SETZ  SETZI SETZM SETZB ',
	'AND   ANDI  ANDM  ANDB  ANDCA ANDCAIANDCAMANDCABSETM  SETMI ',
	'SETMM SETMB ANDCM ANDCMIANDCMMANDCMBSETA  SETAI SETAM SETAB ',
	'XOR   XORI  XORM  XORB  IOR   IORI  IORM  IORB  ANDCB ANDCBI',
	'ANDCBMANDCBBEQV   EQVI  EQVM  EQVB  SETCA SETCAISETCAMSETCAB',
	'ORCA  ORCAI ORCAM ORCAB SETCM SETCMISETCMMSETCMBORCM  ORCMI ',
	'ORCMM ORCMB ORCB  ORCBI ORCBM ORCBB SETO  SETOI SETOM SETOB ',
	'HLL   HLLI  HLLM  HLLS  HRL   HRLI  HRLM  HRLS  HLLZ  HLLZI ',
	'HLLZM HLLZS HRLZ  HRLZI HRLZM HRLZS HLLO  HLLOI HLLOM HLLOS ',
	'HRLO  HRLOI HRLOM HRLOS HLLE  HLLEI HLLEM HLLES HRLE  HRLEI ',
	'HRLEM HRLES HRR   HRRI  HRRM  HRRS  HLR   HLRI  HLRM  HLRS  ',
	'HRRZ  HRRZI HRRZM HRRZS HLRZ  HLRZI HLRZM HLRZS HRRO  HRROI ',
	'HRROM HRROS HLRO  HLROI HLROM HLROS HRRE  HRREI HRREM HRRES ',
	'HLRE  HLREI HLREM HLRES TRN   TLN   TRNE  TLNE  TRNA  TLNA  ',
	'TRNN  TLNN  TDN   TSN   TDNE  TSNE  TDNA  TSNA  TDNN  TSNN  ',
	'TRZ   TLZ   TRZE  TLZE  TRZA  TLZA  TRZN  TLZN  TDZ   TSZ   ',
	'TDZE  TSZE  TDZA  TSZA  TDZN  TSZN  TRC   TLC   TRCE  TLZE  ',
	'TRCA  TLCA  TRCN  TLCN  TDC   TSC   TDCE  TSCE  TDCA  TSCA  ',
	'TDCN  TSCN  TRO   TLO   TROE  TLOE  TROA  TLOA  TRON  TLON  ',
	'TDO   TSO   TDOE  TSOE  TDOA  TSOA  TDON  TSON  700         ' );

begin
  with word do begin
    if opcode < 700b
      then name := substr (opcodes [opcode div 10], (opcode mod 10) * 6 + 1, 6)
      else putstring (name, opcode: 3: o);
    if index = 0
      then index_field := '    '
      else putstring (index_field, '(', index: 2: o, ')');
    putstring (symbolic_text, name, ' ', acc: 2: o, ',', indirect_flag [indirect],
			      offset: 6: o, rel_flag [relocation in [right, both]],
			      index_field);
  end;
end;
$PAGE
function sixbit_text ( word: sixbit_word ): line;
var i: 1 .. 6;
begin
  sixbit_text := '';
  for i := 1 to 6 do
    if word [i] <> 0 then
      sixbit_text := sixbit_text || chr (word [i] + ord (' '));
end;



function radix50_text ( word: radix50_word ): line;
var word1: r50_text;
const
    rad50_cnv: packed array [1 .. 50b] of char =
      ' 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.$%';
begin
  word1 := word.text;
  radix50_text := '';
  while word1 <> 0 do begin
    radix50_text := rad50_cnv [word1 mod 50b + 1] || radix50_text;
    word1 := word1 div 50b;
  end;
end;
$PAGE dump_ascii_text
procedure dump_ascii_text ( header: header_word );

var dump_word: ascii_word;
    xword: overlay_word;
    i: 1 .. 5;

const
    nul = chr (0b);
    cr = chr (15b);
    lf = chr (12b);
    del = chr (177b);

    char_table: array [succ (nul) .. pred (' ')] of string [5] =
      ( '<SOH>', '<STX>', '<ETX>', '<EOT>', '<ENQ>', '<ACK>', '<BEL>',
	'<BS>', '<HT>', '<LF>', '<VT>', '<FF>', '<CR>', '<SO>', '<SI>',
	'<DLE>', '<DC1>', '<DC2>', '<DC3>', '<DC4>', '<NAK>', '<SYN>',
	'<ETB>', '<CAN>', '<EM>', '<SUB>', '<ESC>', '<FS>', '<GS>',
	'<RS>', '<US>' );

begin
  xword.header := header;
  dump_word := xword.ascii;
  i := 1;
  while dump_word [i] <> nul do begin
    case dump_word [i] of
      ' '..'~':
	fio_write (listfb, dump_word [i]);
      del:
	fio_write (listfb, '<DEL>');
      lf:
	fio_line (listfb, '<LF>');
      others:
	fio_write (listfb, char_table [dump_word [i]]);
    end (* case dump_word [i] *);
    if i = 5 then begin
      dump_word := get_a_word.ascii;
      i := 1;
    end
    else
      i := i + 1;
  end (* while dump_word [i] <> nul *);
  fio_skip (listfb);
end (* dump_ascii_text *);
$PAGE dump_uninterpreted
procedure dump_uninterpreted ( name: packed array [1..*] of char;
			       count: integer; kind: count_kind );

var rel_word: relocation_word;
    i: integer;

begin
  fio_line (listfb, ' ' || name);
  if kind = long_count then begin
    for i := 0 to 17 do
      rel_word [i] := none;
  end;
  for i := 0 to count - 1 do begin
    if (kind = short_count) and (i mod 18 = 0) then
      rel_word := get_a_word.relocation;
    if i mod 4 = 0 then begin
      fio_skip (listfb);
      fio_write (listfb, '  ');
    end
    else
      fio_write (listfb, '   ');
    fio_write (listfb, octal_text (get_a_word.octal, rel_word [i mod 18]));
  end;
  fio_skip (listfb);
end (* dump_uninterpreted *);
$PAGE dump_code
procedure dump_code ( count: integer );

var rel_word: relocation_word;
    dump_word: overlay_word;
    offset: octal_word;
    addr_base: line;
    addr_relocation: relocation_syllable;
    i: integer;
    work_line: line;

begin
  if not check_count ('CODE', count, 1, maximum (integer)) then
    return;
  fio_write (listfb, ' CODE AT ');
  rel_word := get_a_word.relocation;
  dump_word := get_a_word;
  if dump_word.radix50.code = 14b then begin
    addr_base := radix50_text (dump_word.radix50) || '+';
    offset := get_a_word.octal;
    i := 2;
  end
  else begin
    addr_base := '';
    offset := dump_word.octal;
    i := 1;
  end;
  addr_relocation := rel_word [i-1];
  fio_line (listfb, addr_base || right_text (offset, addr_relocation));
  fio_skip (listfb);
  for i := i to count - 1 do begin
    if i mod 18 = 0 then
      rel_word := get_a_word.relocation;
    dump_word := get_a_word;
    fio_write (listfb, addr_base || right_text (offset, addr_relocation));
    offset.right := offset.right + 1;
    if opt_symbolic in switches then
      fio_write (listfb, '  ' || symbolic_text (dump_word.symbolic, rel_word [i mod 18]));
    if opt_octal in switches then
      fio_write (listfb, '  ' || octal_text (dump_word.octal, rel_word [i mod 18]));
    if opt_decimal in switches then
      fio_write (listfb, '  ' || decimal_text (dump_word.decimal));
    if opt_floating in switches then
      fio_write (listfb, '  ' || floating_text (dump_word.floating));
    if opt_ascii in switches then
      fio_write (listfb, '  ' || ascii_text (dump_word.ascii));
    if opt_sixbit in switches then begin
      putstring (work_line, '  ', sixbit_text (dump_word.sixbit): 6: l);
      fio_write (listfb, work_line);
    end;
    if opt_radix50 in switches then begin
      putstring (work_line, '  ', dump_word.radix50.code * 4: 2: o, ' ',
			    radix50_text (dump_word.radix50): 6: l);
      fio_write (listfb, work_line);
    end;
    fio_skip (listfb);
  end;
end (* dump_code *);
$PAGE dump_symbols
procedure dump_symbols ( count: integer );

var sym_word: radix50_word;
    rel_word: relocation_word;
    reloc: relocation_syllable;
    fixup_word: overlay_word;
    symbol: line;
    sym_code: 0 .. 17b;
    i: integer;
    work_line: line;

const
    symbol_code: array [0..17b] of string [12] =
      ( ' *ILLEGAL* ', ' =: ', ' = ', '<<BLOCK>>', ' *ILLEGAL* ', ' =: ',
	' *ILLEGAL* ', ' *ILLEGAL* ', ' *ILLEGAL* ', ' ==: ', ' == ',
	' *ILLEGAL* ', '<<REQUEST>>', ' ==: ', ' *ILLEGAL* ', ' *ILLEGAL* ' );
    partial_code: array [0..17b] of string [3] =
      ( '', '', '', '', '', ' ..', '', '', '', '', '', '', '', ' ..', '', '' );

begin
  if odd (count) then begin
    writeln (tty, '%SYMBOL record size not a multiple of 2');
    dump_uninterpreted ('***  SYMBOL RECORD COUNT MOD 2 <> 0  ***', count, short_count);
    return;
  end;
  fio_line (listfb, ' SYMBOLS');
  fio_skip (listfb);
  for i := 0 to (count div 2) - 1 do begin
    if i mod 9 = 0 then
      rel_word := get_a_word.relocation;
    reloc := rel_word [(i * 2 + 1) mod 18];
    sym_word := get_a_word.radix50;
    sym_code := sym_word.code;
    symbol := radix50_text (sym_word);
    putstring (work_line, '  ', sym_code*4: 2: o, '  ');
    fio_write (listfb, work_line);
    case sym_code of
      00b..02b,
      04b..13b,
      15b..17b:
	fio_line (listfb, symbol || symbol_code [sym_code] ||
			  octal_text (get_a_word.octal, reloc) || partial_code [sym_code]);
      03b:
	fio_line (listfb, 'BLOCK ' || symbol || ' AT LEVEL ' ||
			  decimal_text (get_a_word.decimal));
      14b:
	begin
	  fio_write (listfb, 'REQUEST ' || symbol || ':  ');
	  fixup_word := get_a_word;
	  case fixup_word.radix50.code of
	    00b:  fio_line (listfb, 'BACKCHAIN ' ||
				    right_text (fixup_word.octal, reloc) || ' RIGHT');
	    10b:  fio_line (listfb, 'ADJUST LOCATION ' ||
				    right_text (fixup_word.octal, reloc) || ' RIGHT');
	    12b:  fio_line (listfb, 'ADJUST SYMBOL ' ||
				    radix50_text (fixup_word.radix50) || ' RIGHT');
	    14b:  fio_line (listfb, 'ADJUST LOCATION ' ||
				    right_text (fixup_word.octal, reloc) LEFT');
	    16b:  fio_line (listfb, 'ADJUST SYMBOL ' ||
				    radix50_text (fixup_word.radix50) || ' LEFT');
	    others:  fio_line (listfb, '*ILLEGAL* ' ||
				    octal_text (fixup_word.octal, reloc));
	  end;
	end;
    end (* case sym_code *);
  end (* for i *);
end (* dump_symbols *);
$PAGE dump_hiseg
procedure dump_hiseg ( count: integer );

var rel_word: relocation_word;
    dump_word: octal_word;
    i: integer;

begin
  if not check_count ('HISEG', count, 1, 2) then
    return;
  fio_line (listfb, ' HISEG');
  fio_skip (listfb);
  rel_word := get_a_word.relocation;
  dump_word := get_a_word.octal;
  fio_line (listfb, '  HIGH SEGMENT ORIGIN AT ' || right_text (dump_word, rel_word [0]) ||
		      ', BREAK AT ' || left_text (dump_word, rel_word [0]));
  if count = 2 then begin
    dump_word := get_a_word.octal;
    fio_line (listfb, '  LOW SEGMENT ORIGIN AT  ' || right_text (dump_word, rel_word [1]) ||
		       ', BREAK AT ' || left_text (dump_word, rel_word [1]));
  end;
end (* dump_hiseg *);
$PAGE dump_entry
procedure dump_entry ( count: integer );

var rel_word: relocation_word;
    i: integer;

begin
  fio_line (listfb, ' ENTRY');
  fio_skip (listfb);
  for i := 0 to count - 1 do begin
    if i mod 18 = 0 then
      rel_word := get_a_word.relocation;
    fio_line (listfb, '  ' || radix50_text (get_a_word.radix50));
  end;
end (* dump_entry *);
$PAGE dump_end
procedure dump_end ( count: integer );

var rel_word: relocation_word;

begin
  if not check_count ('END', count, 2, 2) then
    return;
  fio_line (listfb, ' END');
  fio_skip (listfb);
  rel_word := get_a_word.relocation;
  fio_write (listfb, '  HIGH SEGMENT BREAK AT ' || right_text (get_a_word.octal, rel_word [0]));
  fio_line (listfb, ', LOW SEGMENT BREAK AT ' || right_text (get_a_word.octal, rel_word [1]));
end (* dump_end *);
$PAGE dump_name
procedure dump_name ( count: integer );

var rel_word: relocation_word;
    other_word: octal_word;

const
    compiler_name: array [0..15b] of string [10] :=
      ( 'UNKNOWN', 'F40', 'COBOL', 'ALGOL', 'NELIAC', 'PL/I', 'BLISS',
	'SAIL', 'FORTRAN', 'MACRO', 'FAIL', 'BCPL', 'MIDAS', 'SIMULA' );

begin
  if not check_count ('NAME', count, 1, 2) then
    return;
  rel_word := get_a_word.relocation;
  fio_line (listfb, ' NAME:  ' || radix50_text (get_a_word.radix50));
  if count = 2 then begin
    fio_skip (listfb);
    other_word := get_a_word.octal;
    fio_write (listfb, '  PROCESSORS: ');
    if other_word.left div 10000b = 0 then
      fio_line (listfb, ' ANY')
    else begin
      if odd (other_word.left div 10000b) then
	fio_write (listfb, ' KA10');
      if odd (other_word.left div 20000b) then
	fio_write (listfb, ' KI10');
      if odd (other_word.left div 40000b) then
	fio_write (listfb, ' KL10');
      fio_skip (listfb);
    end;
    fio_write (listfb, '  COMPILER:  ');
    if other_word.left mod 10000b > 15b then
      fio_line (listfb, 'UNKNOWN')
    else
      fio_line (listfb, compiler_name [other_word.left mod 10000b]);
    fio_line (listfb, '  BLANK COMMON SIZE:  ' || right_text (other_word, rel_word [1]));
  end;
end (* dump_name *);
$PAGE dump_start
procedure dump_start ( count: integer );

var rel_word: relocation_word;
    start_address: octal_word;
    name: radix50_word;

begin
  if not check_count ('START', count, 1, 2) then
    return;
  fio_write (listfb, ' START AT ');
  rel_word := get_a_word.relocation;
  start_address := get_a_word.octal;
  if count = 2 then begin
    name := get_a_word.radix50;
    if name.code <> 60b then begin
      fio_write (listfb, ' ***  BAD BASE SYMBOL  *** ');
      writeln (tty, '%Bad base symbol in START record');
    end;
    fio_write (listfb, radix50_text (name) || ' + ');
  end;
  fio_line (listfb, right_text (start_address, rel_word [0]));
end (* dump_start *);
$PAGE dump_internal_request
procedure dump_internal_request ( count: integer );

var rel_word: relocation_word;
    dump_word: octal_word;
    left_flag: boolean;
    i: integer;

begin
  fio_line (listfb, ' INTERNAL REQUEST');
  fio_skip (listfb);
  left_flag := false;
  for i := 0 to count - 1 do begin
    if i mod 18 = 0 then
      rel_word := get_a_word.relocation;
    dump_word := get_a_word.octal;
    if (dump_word.left = 777777b) and (dump_word.right = 777777b) then
      left_flag := true
    else begin
      fio_write (listfb, '  VALUE ' || right_text (dump_word, rel_word [i mod 18]) ||
			 ':  BACKCHAIN ' || left_text (dump_word, rel_word [i mod 18]));
      if left_flag
	then fio_line (listfb, ' LEFT')
	else fio_line (listfb, ' RIGHT');
      left_flag := false;
    end;
  end;
  if left_flag then begin
    fio_line (listfb, '  ***  LEFT FLAG  ***');
    writeln (tty, '%INTERNAL REQUEST record ends with left-backchain flag');
  end;
end (* dump_internal_request *);
$PAGE dump_polish
procedure dump_polish ( count: integer );

var work_word: overlay_word;
    dump_word: octal_word;
    this_byte: halfword;
    rel_word: relocation_word;
    reloc: relocation_syllable;
    i: integer;
    dump_next: ( polish_codes, octal_left, octal_right, symbol_left, symbol_right,
		 store_address, fixup_symbol_4, fixup_symbol_3, fixup_symbol_2,
		 fixup_symbol_1, finished, skip_rest );
    work_line: line;
    store_code: 1b .. 7b;

const
    opcodes: array [0b..24b] of string [10] =
      ( '  HALFWORD', '  WORD', '  SYMBOL', '  ADD', '  SUB', '  MUL', '  DIV',
	'  AND', '  OR', '  SHIFT', '  XOR', '  NOT', '  NEG', '  COUNT',
	'  MOD', '  ABS', '  MAX', '  MIN', '  EQL', '  ???', '  DEF' );
    store_suffix: array [1b..7b] of string [10] =
      ( ' RIGHT', ' LEFT', ' WORD', ' RIGHT', ' LEFT', ' WORD', ' REPLACE' );
    invert: array [relocation_syllable] of relocation_syllable :=
      ( none, left, right, both );

begin
  fio_line (listfb, ' POLISH');
  fio_skip (listfb);
  listfb.width := 80;
  listfb.c_column := 3;
  dump_next := polish_codes;
  for i := 0 to count * 2 - 1 do begin
    if i mod 18 = 0 then
      rel_word := get_a_word.relocation;
    if odd (i) then
      this_byte := dump_word.right
    else begin
      dump_word := get_a_word.octal;
      this_byte := dump_word.left;
    end;
    reloc := rel_word [(i div 2) mod 18];
    if not odd (i) then
      reloc := invert [reloc];
    case dump_next of

      polish_codes:
	begin
	  if this_byte <= 24b then begin
	    fio_write (listfb, opcodes [this_byte]);
	    case this_byte of
	      0b:  dump_next := octal_right;
	      1b:  dump_next := octal_left;
	      2b:  dump_next := symbol_left;
	      others:  ;
	    end;
	  end
	  else if (this_byte >= 400000b) and (this_byte <= 400777b) then begin
	    putstring (work_line, '  PSECT ', this_byte - 400000b: 3: o);
	    fio_write (listfb, work_line);
	  end
	  else if (this_byte >= 1000000b - 7b) then begin
	    fio_skip (listfb);
	    case 1000000b - this_byte of
	      1b..3b:
		begin
		  fio_write (listfb, '  BACKCHAIN ');
		  dump_next := store_address;
		end;
	      4b..7b:
		begin
		  fio_write (listfb, '  FIXUP SYMBOL ');
		  dump_next := fixup_symbol_4;
		end;
	    end;
	    store_code := 1000000b - this_byte;
	  end
	  else begin
	    fio_skip (listfb);
	    work_word.decimal := this_byte;
	    fio_write (listfb, '  ***  BAD POLISH OPCODE ' ||
			       right_text (work_word.octal, reloc) || '  ***');
	    writeln (tty, '?Bad opcode in POLISH record');
	    dump_next := skip_rest;
	  end;
	end;

      octal_left:
	begin
	  work_word.decimal := this_byte;;
	  fio_write (listfb, '  ' || right_text (work_word.octal, reloc));
	  dump_next := octal_right;
	end;

      octal_right:
	begin
	  work_word.decimal := this_byte;
	  fio_write (listfb, '  ' || right_text (work_word.octal, reloc));
	  dump_next := polish_codes;
	end;

      symbol_left:
	begin
	  work_word.octal.left := this_byte;
	  dump_next := symbol_right;
	end;

      symbol_right:
	begin
	  work_word.octal.right := this_byte;
	  fio_write (listfb, '  ' || radix50_text (work_word.radix50));
	  dump_next := polish_codes;
	end;

      store_address:
	begin
	  work_word.decimal := this_byte;
	  fio_write (listfb, right_text (work_word.octal, reloc) ||
			     store_suffix [store_code]);
	  dump_next := finished;
	end;

      fixup_symbol_4:
	begin
	  work_word.octal.left := this_byte;
	  dump_next := fixup_symbol_3;
	end;

      fixup_symbol_3:
	begin
	  work_word.octal.right := this_byte;
	  fio_write (listfb, radix50_text (work_word.radix50));
	  dump_next := fixup_symbol_2;
	end;

      fixup_symbol_2:
	begin
	  work_word.octal.left := this_byte;
	  dump_next := fixup_symbol_1;
	end;

      fixup_symbol_1:
	begin
	  work_word.octal.right := this_byte;
	  if work_word.decimal <> 0 then
	    fio_write (listfb, ' IN BLOCK ' || radix50_text (work_word.radix50));
	  fio_write (listfb, store_suffix [store_code]);
	  dump_next := finished;
    	end;

      finished:
	begin
	  if not odd (i) then begin
	    fio_skip (listfb);
	    fio_write (listfb, '  ***  GARBAGE AT END OF RECORD  ***  ');
	    work_word.decimal := this_byte;
	    fio_write (listfb, right_text (work_word.octal, reloc));
	    writeln (tty, '?Garbage at end of POLISH record');
	    dump_next := skip_rest;
	  end;
	end;

      skip_rest:
	begin
	  work_word.decimal := this_byte;
	  fio_write (listfb, '  ' || right_text (work_word.octal, reloc));
	end;

    end (* case dump_next *);
  end (* for i *);

  fio_skip (listfb);
  fio_skip (listfb);
  case dump_next of
    polish_codes, octal_left, octal_right, symbol_left, symbol_right:
      begin
	fio_line (listfb, '  ***  NO STORE CODE SEEN  ***');
	fio_skip (listfb);
	writeln (tty, '?No store code in POLISH record');
      end;
    store_address, fixup_symbol_1, fixup_symbol_2, fixup_symbol_3, fixup_symbol_4:
      begin
	fio_line (listfb, '  ***  STORE CODE INCOMPLETE  ***');
	fio_skip (listfb);
	writeln (tty, '?Store code incomplete in POLISH record');
      end;
    finished, skip_rest:
      (* no action *);
  end (* case *);
end (* dump_polish *);
$PAGE dump_chain
procedure dump_chain ( count: integer );

begin
  dump_uninterpreted ('CHAIN', count, short_count);
end (* dump_chain *);
$PAGE dump_request_load_library
procedure dump_request_load_library ( name: packed array [1..*] of char; count: integer );

var rel_word: relocation_word;
    device, filename: sixbit_word;
    ppn: octal_word;
    i: integer;
    work_line: line;

begin
  fio_line (listfb, ' REQUEST ' || name);
  fio_skip (listfb);
  if count mod 3 <> 0 then begin
    writeln (tty, '%REQUEST ', name, ' record size not a multiple of 3');
    dump_uninterpreted ('***  COUNT MOD 3 <> 0  ***', count, short_count);
    return;
  end;
  for i := 0 to (count div 3) - 1 do begin
    if i mod 6 = 0 then
      rel_word := get_a_word.relocation;
    filename := get_a_word.sixbit;
    ppn := get_a_word.octal;
    device := get_a_word.sixbit;
    putstring (work_line, '  ', sixbit_text (device), ':', sixbit_text (filename),
			  '[', ppn.left: 6: o, ',', ppn.right: 6: o, ']');
    fio_line (listfb, work_line);
  end;
end (* dump_request_load *);
$PAGE dump_common
procedure dump_common ( count: integer );

begin
  dump_uninterpreted ('COMMON', count, short_count);
end (* dump_common *);
$PAGE dump_sparse_data
procedure dump_sparse_data ( count: integer );

begin
  dump_uninterpreted ('SPARSE DATA', count, short_count);
end (* dump_sparse_data *);
$PAGE dump_psect_origin
procedure dump_psect_origin ( count: integer );

begin
  dump_uninterpreted ('PSECT ORIGIN', count, short_count);
end (* dump_psect_origin *);
$PAGE dump_psect_attributes
procedure dump_psect_attributes ( count: integer );

begin
  dump_uninterpreted ('PSECT ATTRIBUTES', count, short_count);
end (* dump_psect_attributes *);
$PAGE dump_assign
procedure dump_assign ( count: integer );

begin
  dump_uninterpreted ('ASSIGN', count, short_count);
end (* dump_assign *);
$PAGE dump_it - main routine
var record_header: header_word;
    work_line: line;

begin
  fio_attach (listfb, output);
  if output <> ttyoutput then begin
    listfb.page_header := dmp_header;
    listfb.plength := 44;
    dmp_header (listfb);
  end
  else begin
    fio_line (listfb, 'Dump of Relocatable Binary File ' || filename (relfile));
    fio_skip (listfb);
  end;

  while not eof (relfile) do begin
    read (relfile, record_header);
    with record_header do begin
      if block_type > 3777b then
	dump_ascii_text (record_header)
      else begin
	putstring (work_line, '(', block_type: 4: o, '-', block_count: 0, ')');
	fio_write (listfb, work_line);
	case block_type of
	  0b :  dump_uninterpreted ('IGNORED', block_count, short_count);
	  1b :  dump_code (block_count);
	  2b :  dump_symbols (block_count);
	  3b :  dump_hiseg (block_count);
	  4b :  dump_entry (block_count);
	  5b :  dump_end (block_count);
	  6b :  dump_name (block_count);
	  7b :  dump_start (block_count);
	  10b:  dump_internal_request (block_count);
	  11b:  dump_polish (block_count);
	  12b:  dump_chain (block_count);
	  14b:  dump_uninterpreted ('INDEX', block_count, long_count);
	  15b:  dump_uninterpreted ('ALGOL', block_count, short_count);
	  16b:  dump_request_load_library ('LOAD', block_count);
	  17b:  dump_request_load_library ('LIBRARY', block_count);
	  20b:  dump_common (block_count);
	  21b:  dump_sparse_data (block_count);
	  22b:  dump_psect_origin (block_count);
	  23b:  dump_psect_attributes (block_count);
	  37b:  dump_uninterpreted ('COBOL SYMBOLS', block_count, short_count);
	  100b: dump_assign (block_count);
	  others:
		begin
		  fio_line (listfb, ' *** UNKNOWN LINK RECORD TYPE ***');
		  return;
		end;
	end (* case block_type *);
      end (* if block_type <= 3777b *);
      fio_skip (listfb);
    end (* with record_header *);
  end (* while not eof (relfile) *);

999 (* premature end of file *):

end (* dump_it *);
$PAGE reldmp - main program
var in_file, out_file: file_name;
    cmd_options: option_set;

begin
  rewrite (tty);
  open (tty);
  default_options := standard_defaults;

  loop
    get_command (in_file, out_file, cmd_options);
    if opt_help in cmd_options then
      print_help
    else if opt_exit in cmd_options then
      stop
    else if in_file = '' then
      default_options := cmd_options
    else if open_files (in_file, out_file) then begin
      dump_it (cmd_options);
      close (relfile);
      if output <> ttyoutput then
	close (output);
    end;
  end (* loop *);
end (* reldmp *).
Ga8