$TITLE EXE013 FORMATTED I/O

program exe013;
(* EXE013 - Formatted and unformatted I/O *)

  const test_name:file_name := 'exe013.tst';

  var
     fix_string   :packed array[1..20] of char;
     test_string  :string[20];
     test_integer1:integer;
     test_integer2:integer;
     test_integer3:integer;
     test_real1   :minimum(real)..maximum(real) prec 4;  
     test_real2   :minimum(real)..maximum(real) prec 11;  
     test_char    :char;
     test_bool    :boolean;

     test_file    :text;

     fname:file_name;

  procedure error (error_number: 0..1000);
    begin
    writeln(tty, 'Error ',error_number);
    break(tty);
    end;

$PAGE
begin
rewrite(tty);
writeln(tty,'Begin EXE013');

test_integer1 := -12345;
test_integer2 := 333333;
test_real1    := .00000001;
test_real2    := .00000002;

rewrite(test_file,test_name); 

fname := filename(test_file);

writeln(test_file,test_integer1);
writeln(test_file,test_integer2);
writeln(test_file,test_integer2);
writeln(test_file,test_real1);
writeln(test_file,test_real2);
writeln(test_file,-237.813905);
writeln(test_file,'abcde');
writeln(test_file,'vwxyz');
writeln(test_file,'-4,,030,,-3.26');
writeln(test_file);
writeln(test_file,'11');

close(test_file);

$PAGE

reset(test_file,fname);

readln(test_file,test_string);
if not(( test_string = '-12345') and (length(test_string) = 6))
   then error ( 1 );

readln(test_file,test_string);
if not(( test_string = ' 333333') and (length(test_string) = 7))
   then error ( 2 );
 
readln(test_file,test_real2);
if test_real2  <> 333333.00000  
   then error ( 3 );

readln(test_file,test_string);
if not( test_string = ' 1.000E-08' )
   then error ( 4 );

readln(test_file,test_string);
if not( test_string = ' 2.0000000000E-08' )
   then error ( 5 );

readln(test_file,test_string);
if not(( test_string = '-237.813905') and (length(test_string) = 11))
   then error ( 6 );

readln(test_file,fix_string);
if not( fix_string = 'abcde               ') 
   then error ( 7 );

readln(test_file,test_string);
if not(( test_string = 'vwxyz') and (length(test_string) = 5))
   then error ( 8 );

readln(test_file,test_integer1,test_integer2,test_integer3,
       test_real1,test_real2);
if test_integer1 <> -4
   then error ( 9 );
if test_integer2 <> 0
   then error ( 10 );
if test_integer3 <> 30
   then error ( 11 );
if test_real1 <> 0
   then error ( 12 );
if test_real2 <> -3.2600000000
   then error ( 13 );

read(test_file,test_integer1);
if test_integer1 <> 11
   then error ( 14 );

close(test_file);

$PAGE 'FORMATTED I/O'

rewrite(test_file,fname);

test_string := 'Format';
test_char   := 'X';
test_integer1 := 751;
test_integer2 := -1234;
test_bool   := false;
test_real1  := 234.567;
test_real2  := -219.431257;

writeln(test_file,test_string:20);
writeln(test_file,test_string:4);
writeln(test_file,test_string:20:L);

writeln(test_file,test_char:3);
writeln(test_file,test_char:2:L);

writeln(test_file,test_bool:4);
writeln(test_file,test_bool:6);

writeln(test_file,test_integer1:5);
writeln(test_file,test_integer2:5);
writeln(test_file,test_integer2:8);
writeln(test_file,test_integer1:0);
writeln(test_file,test_integer1:4:O);
writeln(test_file,test_integer1:8:O);
writeln(test_file,test_integer1:3:H);
writeln(test_file,test_integer1:7:H);

writeln(test_file,test_real2:14);
writeln(test_file,test_real2:4);
writeln(test_file,test_real1:12:5);
writeln(test_file,test_real1:4:3);
writeln(test_file,test_real2:7:2:E);
writeln(test_file,test_real1:0:5:E);

close(test_file);

$PAGE

reset(test_file,fname);

readln(test_file,test_string);
if test_string <> '              Format'
   then error ( 15 );

readln(test_file,test_string);
if test_string <> 'Form'
   then error ( 16 );

readln(test_file,test_string);
if test_string <> 'Format              '
   then error ( 17 );

readln(test_file,test_string);
if test_string <> '  X'
   then error ( 18 );

readln(test_file,test_string);
if test_string <> 'X '
   then error ( 19 );

readln(test_file,test_string);
if test_string <> 'FALS'
   then error ( 20 );

readln(test_file,test_string);
if test_string <> ' FALSE'
   then error ( 21 );

readln(test_file,test_string);
if test_string <> '  751'
   then error ( 22 );

readln(test_file,test_string);
if test_string <> '-1234'
   then error ( 23 );

readln(test_file,test_string);
if test_string <> '   -1234'
   then error ( 24 );

readln(test_file,test_string);
if test_string <> '751'
   then error ( 25 );

readln(test_file,test_string);
if test_string <> '1357'
   then error ( 26 );

readln(test_file,test_string);
if test_string <> '00001357'
   then error ( 27 );

readln(test_file,test_string);
if test_string <> '2EF'
   then error ( 28 );

readln(test_file,test_string);
if test_string <> '00002EF'
   then error ( 29 );

readln(test_file,test_string);
if test_string <> '-2.1943126E+02'
   then error ( 30 );

readln(test_file,test_string);
if test_string <> '-2.2E+02'
   then error ( 31 );

readln(test_file,test_string);
if test_string <> '   234.56700'
   then error ( 32 );

readln(test_file,test_string);
if test_string <> '2.34567E+02'
   then error ( 33 );

readln(test_file,test_string);
if test_string <> '-2.2E+02'
   then error ( 34 );

readln(test_file,test_string);
if test_string <> '2.3457E+02'
   then error ( 35 );
writeln(tty,'*',test_string,'*');

scratch(test_file);

writeln(tty,'End   EXE013');
end.
   