(*********     Initialization -- Machine-dependent Declarations

**********     VAX/VMS

*********)


module vaximd;

$SYSTEM pascal.inc
$SYSTEM pasist.inc
$SYSTEM ptmcon.inc
$SYSTEM pasini.inc
$SYSTEM ptmimd.typ

public const
    mword_size: integer = 32;
    max_real: real = 1.7e+38;
    max_char: integer = 177b;
    fnamsize: integer = 125;
    fnamextern: extname = 'PAX$FIO.FNAME';
    trnamextern: extname = 'TRACE';
    condnamextern: condnames = ('', '', '', '', '', '', '');
$PAGE sys_ppf

(*  SYS PPF will enter the system-dependent predefined procedures and functions
    into the initial symbol table.  *)

public procedure sys_ppf;

begin
  predef ( 'SPY', 'SPY', consts, prtyp0 ( nil ) ); (* prints trace of most recently executed stmts *)
end;
$PAGE tm_constants

(*  TM_CONSTANTS will initialize the PTMCON variables to values which
    characterize the target machine.  *)

public procedure tm_constants;

begin
  tmprefix := 'VAX';
  ttyiname := 'PAX$INPUT';
  ttyoname := 'PAX$OUTPUT';
  rel_extension := 'OBJ';
  have_checkout := true;
  have_optimizer := false;
  radix := hex_radix;
  adr_width := 8;
  srealprec := 6;
end;
$PAGE dcl_stt_constants
(* DCL SCONSTS will declare the elements of the scalar status types.  *)

public procedure dcl_sconsts;

begin
  stt_constant ('IO_OK'  , stat_io, 0);
  stt_constant ('IO_NOVF', stat_io, 1);
  stt_constant ('IO_POVF', stat_io, 2);
  stt_constant ('IO_DGIT', stat_io, 3);
  stt_constant ('IO_GOVF', stat_io, 4);
  stt_constant ('IO_INTR', stat_io, 5);
  stt_constant ('IO_REWR', stat_io, 6);
  stt_constant ('IO_EOF' , stat_io, 7);
  stt_constant ('IO_OUTF', stat_io, 8);
  stt_constant ('IO_INPF', stat_io, 9);
  stt_constant ('IO_SEEK', stat_io, 10);
  stt_constant ('IO_ILLC', stat_io, 11);
  stt_constant ('IO_NEMP', stat_io, 12);
  stt_constant ('IO_OPNF', stat_io, 13);

  stt_constant ('PROGRAM_OK',		stat_program, 0);
  stt_constant ('PROGRAM_ASSERTION',	stat_program, 1);
  stt_constant ('PROGRAM_CASE',		stat_program, 2);
  stt_constant ('PROGRAM_COMPATIBILITY',stat_program, 3);
  stt_constant ('PROGRAM_FILE',		stat_program, 4);
  stt_constant ('PROGRAM_POINTER',	stat_program, 5);
  stt_constant ('PROGRAM_SUBSTRING',	stat_program, 6);
  stt_constant ('PROGRAM_SUBSCRIPT',	stat_program, 7);
  stt_constant ('PROGRAM_VALUE',	stat_program, 8);

end.
