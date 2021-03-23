(* CMDUTL.typ - last modified 7/8/82 - type declarations for command 
   utility subroutines. *)


TYPE						(* system dependent file name types *)
  file_id = string [75];
  EXTENSION = PACKED ARRAY [1..3] OF CHAR;


CONST CMDLINELEN = 254;				(* command line declaration *)
TYPE
  CMDLINE = STRING[254];			(* string itself *)
  CMDLINEIDX = 0..255;				(* index of above *)

