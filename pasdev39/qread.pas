$PAGE qread
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

              10/12/81 djm  - Changed chr(15b) to cr.

              7/29/82 WNH - changed module to use QTERM procedures and
		     functions.

     ---------------------------------------------------------------- *)
module qread
  options special;
public function qread : qstring;
 var nll: qstringidx;
     newline: qstring;

procedure readline;    (* to read from the terminal *)

 begin
  if ttieol then ttrdln;		(* IF AT END OF LINE, GET NEW LINE *)
  ttread (newline);

end; (* of READLINE *)

begin   (* Body of QREAD *)
  qread:= '';

loop
  readline;

exit if ttieol andif (ttirefbuf <> chr(7) (* CONTROL-G *));

  ttwrite ('^G?'); ttbrk; (* Diagnostic for ^G character *)
  qread:= qread || newline
end;

  nll:= length(newline);
  if (nll > 0) andif (newline[nll] = cr) then nll:= nll-1;
  if length(qread) = 0 then qread:= substr(newline,1,nll)
  else qread:= qread || substr(newline,1,nll)
end.
