(* DEBREF.INC - external declarations for public routines of the Pascal
   Debugger module DEBREF.PAS.  *)

external function  ext$scalar (descriptor; var status_code): machine_word;
external procedure sto$scalar (descriptor; machine_word);
external function  ext$address (descriptor; var status_code): unit_range;
external function  ext$setelem (descriptor; set_range): boolean;
external procedure sto$setelem (descriptor; set_range; boolean);
external function  ext$real (descriptor; var status_code): real_type;
external function  ext$lenstr (descriptor): char_range;
external procedure sto$lenstr (descriptor; char_range);
external function  a$$backwards (descriptor; descriptor): boolean;
external function  ext$strchar (descriptor; char_range): char;
external procedure sto$strchar (descriptor; char_range; char);
external procedure init$desc (var descriptor);
external function  set$field$addr (intsym; addr_record): addr_record;
external function  get$element$addr (descriptor; machine_word): addr_record;
external procedure set$kind (var descriptor; inttyp);
external procedure mak$scalar$desc (var descriptor; inttyp; machine_word);
external procedure set$bounds (var descriptor; intsym; ^stack_frame);
external procedure set$size (intsym; var descriptor);
external function  comp$types (descriptor; descriptor): boolean;
external procedure check$type (descriptor; inttyp; var status_code);
external procedure get$descriptor (var lex_scan_rec_type;
                                   scope_type; var descriptor; var status_code);
