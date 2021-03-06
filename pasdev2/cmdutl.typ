(* CMDUTL.typ - last modified 7/8/82 - type declarations for command 
   utility subroutines. *)


TYPE						(* system dependent file name types *)
  FILE_ID = FILE_NAME;				(* changed 7-8-82: max string length to 'file_name' *)
  EXTENSION = PACKED ARRAY [1..3] OF CHAR;


CONST CMDLINELEN = 254;				(* command line declaration *)
TYPE
  CMDLINE = STRING[254];			(* string itself *)
  CMDLINEIDX = 0..255;				(* index of above *)

    