program casetest;

var a: char;

begin
  case a of
    'A'..'C':
       a := 'e';
    'G':
       a := 'f'
  end;
  case a of
    '0'..'9','A'..'Z':
       a := char (8);
    others:
       a := char (9)
  end;
end.
 