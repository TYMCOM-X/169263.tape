module test61;

type
  a = procedure (char);
  b = function (var char): char;
  c = string[10];

var
  aa: a;
  bb: b;
  cc: c;
  ch: char;

public procedure ech (var d: char);

function gonzo (var dd: char): char;
begin gonzo := dd || gonzo end;

procedure barf (q: char);
var qq: b;
qqq: procedure (char);
begin d := 'c'; qq := gonzo; qqq := barf end;
var qqq: a;
begin barf (gonzo (d) ); qqq := barf ;
qqq ('c') end.
