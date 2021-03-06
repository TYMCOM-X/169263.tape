(* DEBIO.INC - external declarations for public routines of the Pascal
   Debugger module DEBIO.  These routines provide tty I/O using TTCALLS
   (or the host machine's equivalent) to avoid run_time conflicts.  *)

external procedure read$line (var string[*];
                              var boolean   );
 
external procedure writ$str  (    packed array [1..*] of char);
external procedure writ$eol;
external procedure writ$nl   (    packed array [1..*] of char);
external procedure writ$int  (    machine_word;
                                  radix_type);
  