$TITLE rfread
$LENGTH 42
$OPTIONS special
$PAGE includes and externals
$include rfdata.inc

external function rfword ( line_type; var line_index ): command_words;
$PAGE readline
public procedure readline
      ( var cmdkind: command_words;	(* indicates which command line is, if any *)
	var line: line_type	    );	(* line read on input *)

var
  inline: array[1..max_line_length] of char;
  inlength: line_index;
  inchar: char;
  inpos, outpos: line_index;
  fontkind: fonts;


begin
  (* Read a single input line, raw.  If an end of file is encountered, return
     immediately, setting cmdkind to indicate the condition. *)

  readln (input);			(* advance to next line *)
  if eof (input) then begin
    cmdkind := endoffile;		(* drop out at end of file *)
    line := nil;			(* no text *)
    return;
  end;

  cmdkind := nonword;			(* assume that it is not a command for now *)
  inlength := 0;

  while not eoln (input) do begin	(* accumulate line *)
    inlength := inlength + 1;
    inline [inlength] := input^;
    get (input);
  end;

  (* Perform decapitalization if enabled.  This is done in place in the input
     line. *)

  if dodecap then begin
    inpos := 1; outpos := 1;		(* init scanning cursors *)
    inline [inlength+1] := ' ';		(* permit reference off end of line *)
    while inpos <= inlength do begin	(* scan input line *)
      inchar := inline [inpos];
      inpos := inpos + 1;
      if not (inchar in ['@', '*']) then begin	(* normal text *)
	inline [outpos] := lowercase (inchar);
	outpos := outpos + 1;
      end
      else if inchar = inline[inpos] then begin	(* have "@@" or "**" *)
	inline [outpos] := inchar;		(* escape to insert single one *)
	outpos := outpos + 1;
	inpos := inpos + 1;
      end
      else if inchar = '*' then begin	(* uppercase the next char *)
	inline [outpos] := uppercase (inline[inpos]);
	outpos := outpos + 1;
	inpos := inpos + 1;
      end
      else (* if inchar = '@' then *) begin	(* capitalize a string *)
	loop
	exit if inpos > inlength;	(* no '@' before end of line *)
	  inchar := inline [inpos];
	  inpos := inpos + 1;
	exit if inchar = '@';	(* terminate string, skip the '@' *)
	  inline [outpos] := uppercase (inchar);
	  outpos := outpos + 1;
	end;
      end;
    end (* while *) ;
    inlength := outpos - 1;		(* update length *)
  end (* dodecap *) ;


  (* Create the output line.  The input line is scanned to for all font changes
     (for now only underlining), and the input characters are expanded into the
     extended characters.  Note that the line is allocated with space for as
     many characters as are on the input line; if there are font change indicators
     not all these positions will be used on output. *)

  new (line:inlength);				(* allocate the line *)
  with line^ do begin
    indentation := 0;				(* assume left aligned *)
    spacing := 0;				(* assume not double (etc.) spaced *)
    total_width := 0;				(* incremented for each character *)
    number := 0;				(* don't kept track of this now *)
    inpos := 1; outpos := 1;			(* init scanning cursors *)
    fontkind := standard;				(* begin each line in standard *)
    inline [inlength+1] := ' ';	(* permit references off end *)

    while inpos <= inlength do begin
      inchar := inline [inpos];
      inpos := inpos + 1;
      if inchar <> '&' then begin			(* normal character *)
	with text [outpos] do begin
	  ch := inchar;
	  font := fontkind;
	  width := 1;
	  if inchar = ' ' then begin		(* compress spaces *)
	    while (inpos <= inlength) andif (inline[inpos] = ' ') do begin
	      inpos := inpos + 1;
	      width := width + 1;
	    end;
	  end;
	  total_width := total_width + width;	(* add to width of line too *)
	end (* with *) ;
	outpos := outpos + 1;
      end
      else if inline[inpos] = '&' then begin	(* if "&&" insert one "&" *)
	inpos := inpos + 1;		(* skip the second *)
	with text [outpos] do begin
	  ch := '&';
	  font := fontkind;
	  width := 1;
	  total_width := total_width + 1;
        end;
	outpos := outpos + 1;
      end
      else begin				(* single ampersand, change under lining state *)
	if fontkind = standard
	  then fontkind := underlined
	  else fontkind := standard
      end
    end (* while *) ;
    textlen := outpos - 1;
  end (* with *) ;


  (* Finally, check if this line is command text, and if so look up the
     command name. *)

  if inline[1] = '$' then begin
    inpos := 2;				(* init cursor to point to char after "$" *)
    cmdkind := rfword (line, inpos);
  end;
end.
    