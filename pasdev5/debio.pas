$LENGTH 43
$OPTIONS special,notrace,nocheck

module debio$;

(* Package of routines for Pascal Debugger Teletype I/O...Routines use
   TTCALLs to avoid potential conflicts with Pascal run-time I/O.

   Entry points include:

   writ$ch -- basic character writer routine.
   writ$str - write a string.
   writ$nl -- write a string and a carriage return (string may be null).
   writ$int -- write an integer value.
*)


const cr = chr(15b); (*returned/passed on/for end of line*)

(* read$nl reads an uppercased string from the terminal, but does NOT
   return the carriage return or line feed. *)
external procedure read$nl (var string);
external procedure ch$out (char);	(* writes a char to tty *)
type radix = (octal,decimal); (*for parameters indicating radix*)

public var at$eoln: boolean; (*set when cr read by read$ch for query by read$nl*)
$PAGE writ$ch

public procedure writ$ch (ch: char);

const
  lf = chr (12b);

begin
  ch$out (ch);
  if ch = cr then
    ch$out (lf);
end;
$PAGE numeric I/O routines

(* routine to write integer number in desired radix. *)

public procedure writ$int(int: integer; default_radix: radix);

  var split: packed record (*for splitting octal numbers into halfwords*)
	case boolean of
	  true: (left: 0..777777b; right: 0..777777b);
	  false: (int: integer)
	end;

  procedure writenonneg(int: integer; default_radix: radix);
    var base: 8..10;
  begin
    if default_radix=octal then base:= 8
    else base:= 10;
    if (int div base)<>0 then writenonneg(int div base,default_radix);
    writ$ch( chr( int mod base + ord('0') ) )
  end (*writenonneg*);

begin (*writ$int*)
  if default_radix=octal then begin
    split.int:= int; (*write each halfword separately*)
    if split.left<>0 then begin
      writenonneg(split.left,octal);
      writ$ch(','); writ$ch(',')
    end;
    writenonneg(split.right,octal)
  end
  else
    if int>=0 then writenonneg(int,decimal)
    else begin
      writ$ch('-');
      writenonneg(abs(int),decimal)
    end
end (*writ$int*);
$PAGE string I/O routines

(* routine to write a string (of maximum length 128) *)

type message = string[128];

public procedure writ$str(s: message);
  var i: 1..128;
begin
  for i:=1 to length(s) do writ$ch(s[i])
end (*writ$str*);


(*routine to write a string and a carriage return*)

public procedure writ$nl(s: message);
begin
  writ$str(s);
  writ$ch(cr)
end (*writ$nl*).
  