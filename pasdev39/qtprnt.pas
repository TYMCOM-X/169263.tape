(* QTPRNT.PAS - Begun 7/27/82 by WNH to help allow ANC to use either a
   terminal or a graphics terminal for QED output.  This module does the
   same things as QPRINT, but this one does it through the QTERM modules. *)
$PAGE termline

module qtprint
  options special;

  function numtochar (num: qlineno; fw : 0..maximum (integer)): qstring;
  var value: qlineno;
  begin
    numtochar := '';
    value := num;
    repeat
      numtochar := chr (ord ('0') + (value mod 10)) || numtochar;
      value := value div 10
    until value = 0;
    while length (numtochar) < fw do
      numtochar := ' ' || numtochar;
  end;						(* numtochar *)

public procedure termline
(	line: qstring;				(* text to write out *)
        tab_print: boolean;                     (* flag for &I printing *)
        var err: qerrcode);			(* checks for write errors *)

var
  i: qlineno;

begin
  for i := 1 to length (line) do
  begin
    if (ord(line[i]) > #o37) orif (tab_print and (ord(line[i]) = #o11))
      then ttwrite (line [i] )
      else
        begin
        ttwrite ('&'); ttwrite (chr (ord (line [i]) + #o100));
        end;
  exit if not ttoeof do err := qwrterr
  end;
  ttwtln;
  if not ttoeof then err := qwrterr
end;
$PAGE termlstlns
public procedure termlstlns
(	var buffer: qbuffer;			(* working buffer *)
	low,
	high: qlineno;				(* range of lines to print *)
	ctl_char,
	number,        			(* flags to modify printing *)
        tab_print: boolean;                     (* flag for &I printing *)
        var err: qerrcode);			(* set if write errors occur *)

var
  i: qlineno;
  line: qstring;

begin
  i := low;
  err := qok;
  while (err = qok) and (i <= high) do
  begin
    line := qgetline (buffer, i, err);
    if err = qok then
    begin
      if number then begin
        ttwrite (numtochar (i, 5)); ttwrite (tab);
        if not ttoeof then err := qwrterr
      end;
      if err = qok then begin
	if ctl_char then termline (line, tab_print, err)
        else begin
          ttwrite (line);
          ttwtln;
          if not ttoeof then err := qwrterr
        end;
	i := i + 1
      end
    end
  end
end.
