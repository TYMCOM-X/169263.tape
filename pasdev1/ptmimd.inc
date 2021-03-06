(*********     Initialization -- Machine-dependent Declarations     **********)


(* The following constants are public in the module P10IMD (or VAXIMD, ... ) rather than among the 
   globals saved from pass to pass because they are only used EXPLICITLY in the initialization pass.  *)
  
external const

(*  MWORD SIZE is the number of bits that a variable of type MachineWord
    will occupy on the target machine.  *)

    mword_size: integer;


(*  MAX REAL is the largest real value to be representable in the standard
    type Real.  *)

    max_real: real_type;


(*  MAX CHAR is the numeric index of the last character in the character set.
    (This is 177b for Ascii, 377b for Ebcdic.)  *)

    max_char: integer;


(*  FNAMSIZE is the maximum number of characters which may be occupied
    by a file name string on the target system.  *)

    fnamsize: integer;


(*  FNAMEXTERN is the actual runtime symbol for the routine to be called
    for the Filename built-in function.  *)

    fnamextern: extname;


(*  TRNAMEXTERN is the actual runtime symbol for the routine to be called
    for the Trace built-in procedure.  *)

    trnamextern: extname;

(*  EXNAMEXTERN is the actual runtime symbol for the routine to be called
    for the  ExecptionMessage built-in function.  *)

    exnamextern: extname;

(*  CONDNAMEXTERN is an array of the actual runtime symbols for the standard
    conditions.  *)

    condnamextern: condnames;


(*  SYS PPF is a procedure which will add the system-dependent predefined
    procedures and functions to the initial symbol table.  *)

external procedure sys_ppf;


(*  TM CONSTANTS is a procedure which initializes the target machine constants
    for the rest of the compiler.  *)

external procedure tm_constants;


(*  DCL SCONSTS is a procedure which will declare the elements of all the
    scalar status types.  *)

external procedure dcl_sconsts;
    