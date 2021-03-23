module mmspub;

(* public declarations for the ODMS system (all of it!) *)

$SYSTEM MMSSYM.TYP
$SYSTEM MMSMDL.TYP

public var
  curmdl: ^pnode;			(* the current MDL *)
  newmdl: ^pnode;			(* new one by MDLPRO *)
  currid: mdlid;			(* ID to lookup *)
  lastsym: symptr;			(* return values from name lookup *)
  lastmod: modptr;			(* most recently looked-up nodes *)
  lastarea: areaptr;			(* for symbols, modules, and areas *)
  res_str: string[255];			(* BUILD RESIDENT command line *)
  js: array [1..100] of integer;	(* ecch, but can't include file *)
  dbvers, dbnvers: integer;		(* versions as parsed and passed *)
  curmod: modptr;			(* for database *)
  dbfile: file of * := Nilf;		(* the file variable for DB *)
  curfptr, prevfptr: integer;		(* global DB pointers *)
  compfn: file_name;			(* command line to COMPILE *)
  mdlerror: boolean;			(* did it go? *)

end (* of datamodule MMSPUB *).
