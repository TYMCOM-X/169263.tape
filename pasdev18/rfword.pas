$TITLE rfword
$LENGTH 42
$PAGE includes
$include rfdata.inc
$PAGE word table

type wtype = array [command_words] of packed array[1..10] of char;
const words: wtype :=
      (	'JUSTIFY   ',
	'CENTER    ',
	'VERBATIM  ',
	'PAGE      ',
	'SKIP      ',
	'PARAGRAPH ',
	'INDENT    ',
	'SPACING   ',
	'MARGIN    ',
	'NEED      ',
	'PICTURE   ',
	'TITLE     ',
	'FOOTNOTES ',
	'TABS      ',
        'NUMBER    ',
	'LEFT      ',
	'RIGHT     ',
	'TOP       ',
	'BOTTOM    ',
	'TERMINAL  ',
	'ENDOFFILE ',
	'*NONWORD* ',
	'*BADCMD*  '  );
$PAGE rfword

public function rfword
	     (	line: line_type;
		var idx: line_index   ): command_words;

 var name: packed array[1..10] of char;
 var l: line_index;
 var w: command_words;
 var ch: char;

 begin
  with line^ do begin
    name := '';		(* get token *)
    l := 0;			(* to record length of name *)
    while idx <= textlen do begin
      ch := uppercase (text[idx].ch);
    exit if not (uppercase (ch) in ['A'..'Z']);
      idx := idx + 1;
      l := l + 1;
      if l <= length (name)
        then name [l] := ch;
    end (* while *) ;

    if (3 <= l) and (l <= length (name)) then begin
      for w := minimum (command_words) to maximum (command_words) do
	if substr (words [w], 1, l) = name	(* must match to length given *)
	  then begin
	    rfword := w;
	    return
	  end;
    end;

    rfword := badcmd;	(* will have exited if word found *) 
  end (* with *) ;
 end.
 