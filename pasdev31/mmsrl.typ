(* This defines the types used by the PDP-10 code generator.  It defines all
   formats specific to the machine, the linker, etc. *)

type
  registers = 0..#o17;			(* PDP-10 registers *)
  set_of_registers = set of registers;
  reg_selector = 0..#o20;		(* registers + special values, see consts below *)
  opc_range = 0..#o777;                  (* binary range of opcode values *)
  code_address = 0..#o777777; (* PDP 10 address range *)

const
  noreg: reg_selector := 0;		(* no register assigned, no indexing, etc. *)
  anyreg: reg_selector := #o20;		(* invites any register assignment *)
  int_nil: code_address := #o77777;	(* nil defined with base type integer *)


  (* A pdp10word is an undiscrimated union of words of all useful formats.
     It is used in code records and rel files. *)

Type
  pdp10word =
      packed record
	case char of

	  'I': ( opcode: opc_range;       (* instruction *)
		 acc: registers;
		 indirect: boolean;
		 index: registers;
		 offset: code_address  );

	  'B': ( p: 0..#o64;               (* byte pointer *)
		 s: 0..#o64;
		 bp1: 0..#o1;              (* unused: mbz *)
		 bpindirect: boolean;     (* instruction field names used *)
		 bpindex: registers;
		 bpoffset: code_address  );

	  'F': ( value: machine_word );       (* full word integer *)

	  'S': ( bits: packed array[0..35] of boolean );  (* full word set *)

	  'X': ( lh: code_address;          (* halfwords *)
		 rh: code_address  );

	  'C': ( str: packed array[1..5] of char );       (* character string *)

	  '6': ( sixbit: packed array[1..6] of 0..#o77 );   (* six bit characters *)

	  '5': ( code50: 0..#o17; (* radix-50 code/symbol word *)
		 sym50: 0..#o37777777777 );

	  'R': ( rel_byte: packed array [1..18] of 0..3 ) (* Link-10 relocation bytes *)

      end;
$PAGE
(* This file contains type declarations for the part of ODMS that
   emits the .SRL file, in place or in addition to the .SYM file *)

Type SYM_ANCHOR = Record			(* Anchor for SYMBOL table *)
	FIRST : ^SYM_ENTRY;			(* Begining of table *)
	LAST  : ^SYM_ENTRY			(* Last entry in the table *)
     End;

     SYM_STRING = String [ 10 ];		(* String for holding symbols *)

     SYM_KIND = ( POLISH , LOCAL , PUBLIC_DEFINITION , EXT_DEF , DEFINED );

      SYM_ENTRY = Record			(* Entry for each symbol *)
	SYMBOL : SYM_STRING;			(* The symbol itself *)
	ADDR   : Machine_word;			(* Address for back-chaining *)
	LEFT_CHAIN : Boolean;
	KIND   : SYM_KIND;			(* LOC,REF,POLISH,EXT *)
	FIRST  : Boolean;			(* FIrst time referenced *)
	NEXT   : ^SYM_ENTRY			(* Pointer to next symbol *)
     End;

     DATA_ARY_IDX = 1..22;			(* Index for code record *)

     (* The code record, containing up to 18 pdp10 words *)

     DATA_ARY = Array [ DATA_ARY_IDX ] of PDP10WORD;

(* End of MMSRL.TYP *)
$PAGE Link-10 item types

type
    item_type = 0 .. #o37;

const
    no_rec = 0;
    code_rec = 1;
    symbol_rec = 2;
    hiseg_rec = 3;
    entry_rec = 4;
    end_rec = 5;
    name_rec = 6;
    start_rec = 7;
    int_request_rec = #o10;
    polish_rec = #o11;
    lib_request_rec = #o17;

    def_global_sym = 1;
    def_local_sym = 2;
    req_global_sym = #o14;

    polish_byte = 0;
    polish_global = 2;
    polish_add = 3;
    polish_sub = 4;
    polish_str = #o777777 ;
    polish_stl = #o777776 ;
 