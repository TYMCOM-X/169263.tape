program lc;

var
  line: string [200];
  i: 1..200;
  state: (init, starting_comment, in_comment, ending_comment, in_string);

begin
  reset (input, 'lexgen.pas');
  rewrite (output, 'lexgen.new');

  state := init;
  while not eof (input) do begin
    readln (input, line);

    for i := 1 to length (line) do
      case state of
	init:
	  if line [i] in ['A'..'Z'] then
	    line [i] := lowercase (line [i])
	  else if ((line [i] = '(') and (length (line) > i)) andif (line [i+1] = '*') then
	    state := starting_comment
	  else if line [i] = '''' then
	    state := in_string;
	starting_comment:
	  state := in_comment;
	in_comment:
	  if ((line [i] = '*') and (length (line) > i)) andif (line [i+1] = ')') then
	    state := ending_comment;
	ending_comment:
	  state := init;
	in_string:
	  if line [i] = '''' then
	    state := init
      end;

    writeln (output, line)
  end;

  assert (state = init);
  close
end.
   