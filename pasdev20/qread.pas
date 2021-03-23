(*   +--------------------------------------------------------------+
     I                                                              I
     I                         Q R E A D                            I
     I                         - - - - -                            I
     I                                                              I
     +--------------------------------------------------------------+

     MDSI, COMPANY CONFIDENTIAL

     STARTED: 29-Jul-77

     PURPOSE: Reads an input line and flags control-G's.

     USAGE:
        external function qread : qstring;
	var newline: qstring;

	newline := qread ;

     INPUT: 

        none.

     OUTPUT:

        newline is the line from TTY input.

     REQUIREMENTS: The files TTY and  TTYOUTPUT  must  be  open  when
	qread is called.  A spurious end-of-file on TTY is cleared
	by this routine,  however.

     NOTES: If  the  input  line  is  too  long,  an  end-of-line  is
	simulated  and  TTY  is  left  in the middle of a line - i.e.
	EOLN  (TTY)  is  false.  A  subsequent  call  picks  up   the
	remainder as a separate line.

     RESPONSIBLE: Software Tools

     CHANGES: 7/2/79 P. LEE - Changed EDITREAD to QREAD and deleted
              intra-line editing. This header also changed.

     ---------------------------------------------------------------- *)
$PAGE includes
$INCLUDE QSTR.TYP

$PAGE qread
public function qread : qstring;
  label 100;
 var newline: packed array[1..qstringlen] of char;  (* PACKED FOR SPEED *)
     nll: qstringidx;

procedure readline;    (* to read from the terminal *)

 begin
  if eoln (tty) then readln (tty);		(* IF AT END OF LINE, GET NEW LINE *)
  if eof (tty) then begin			(* ERRONEOUS CONDITION *)
    open (tty, '');				(* MUST REOPEN IT *)
    readln (tty)
  end;
  while not eoln (tty) do begin			(* READ EACH CHARACTER *)
    if nll < length (newline) then begin	(* ROOM LEFT IN BUFFER *)
      nll := nll + 1;
      newline [nll] := tty^;
      get (tty)
    end
    else goto 100				(* HAVE OVERFLOWED BUFFER, RETURN WHAT WE HAVE *)
  end

end; (* of READLINE *)

begin   (* Body of QREAD *)
  nll := 0;					(* HAVE NO CHARS YET *)

loop
  readline;

exit if eoln(tty) andif (tty^ <> chr(7) (* CONTROL-G *));

  write(tty,'?');break; (* Diagnostic for ^G character *)
end;

100:
  qread := substr(newline,1,nll)     (* return varying string *)
end.
