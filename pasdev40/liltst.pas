program test;

var
  counter : integer;
  dummy : integer;
$PAGE page_1
procedure proc_1;

begin
  for counter := 1 to 10 do
    dummy := counter;
end (* proc_1 *);
$PAGE page_2
procedure proc_2;

begin
  for counter := 1 to 20 do
    dummy := counter;
end (* proc_2 *);
$PAGE page_3
procedure proc_3;

begin
  for counter := 1 to 30 do
    dummy := counter;
end (* proc_3 *);
$PAGE page_4
procedure proc_4;

begin
  for counter := 1 to 40 do
    dummy := counter;
end (* proc_4 *);
$PAGE page_5
procedure proc_5;

begin
  for counter := 1 to 50 do
    dummy := counter;
end (* proc_5 *);
$PAGE page_6
procedure proc_6;

begin
  for counter := 1 to 60 do
    dummy := counter;
end (* proc_6 *);
$PAGE page_7
procedure proc_7;

begin
  for counter := 1 to 70 do
    dummy := counter;
end (* proc_7 *);
$PAGE page_8
procedure proc_8;

begin
  for counter := 1 to 80 do
    dummy := counter;
end (* proc_8 *);
$PAGE main
begin
  proc_1;
  proc_2;
  proc_3;
  proc_4;
  proc_5;
  proc_6;
  proc_7;
  proc_8;
end.
    