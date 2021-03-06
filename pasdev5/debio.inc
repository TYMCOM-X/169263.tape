$options nosource

(* Pascal debugger tty I/O routines (use TTCALLs to avoid run-time conflicts) *)

type message = string[128];
     radix = (octal,decimal);

const cr = chr(15b); (*returned/passed from/to writ$ch/read$ch for end of line*)

external var at$eoln: boolean; (*set when cr just returned by read$ch*)

external procedure writ$ch(ch: char);
external procedure writ$str(s: message);
external procedure writ$nl(s:message);
external procedure writ$int(int: integer; def_radix: radix);
external procedure read$nl (var string); (* reads a line, uppercased *)
$options source
 