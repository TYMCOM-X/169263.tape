
(* IORESET flushes ongoing terminal I/O.  Used to flush buffered output, and
   to discard read-ahead after an escape is received. *)

external procedure ioreset;



(* IRESET flushes terminal input.  Used to discard read-ahead after an error
   is detected. *)

external procedure ireset;

