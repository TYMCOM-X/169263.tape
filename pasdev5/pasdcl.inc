



const

$SYSTEM PASCON.INC

type

$SYSTEM PASTYP.INC

external var

$SYSTEM PASVAR.INC

external var

  dateofcompilation: packed array[1..9] of char;    (* "COMPDATE"/VERSION *)
  pas, lst, rel: linenrtype;
  errmsgfile: alfa;
  alphanumeric, alphabetic, digits, lettersdigitsorleftarrow, hexadigits: set of char;
  constbegsys, simptypebegsys, typebegsys, blockbegsys, selsys,
  facbegsys, languagesys, statbegsys, typedels: setofsys;
  intptr, realptr, charptr, boolptr, nilptr, textptr, dateptr, alfaptr, arbptrtyp, strptr: stp;
  lastbtp: btp;
  fglobptr: gtp;
  fileptr, sfileptr: ftp;
  ttyfptr, toutfptr, inputfptr, outfptr: ctp;
  utypptr, ucstptr, uvarptr, ufldptr, uprcptr, ufctptr: ctp;
  library: array[pascalsy..cobolsy] of librec;
  rnts: record
	name: array[supports] of alfa;
	link: packed array[supports] of addrrange
  end;
  head_symbol: ctp;

external procedure set_ptr_type (idclass);
external procedure open_xref_file;
 