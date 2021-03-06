(* TST016 - general tests.  This module should be linked with
   TST015 in order to execute it.  *)

module tst016;

$DISABLE generics

external procedure oof ( integer );

public var
  pub_var: integer;

type
$IF GENERICS  gen_array = packed array [*] of 0..255;
$IFNOT GENERICS gen_array = packed array [-3..*] of 0..255;
  rgb = (red, blue, green);
  hab_son_proc = function ( var integer; integer; integer; integer; integer;
    integer; integer; set of char ): integer;
$PAGE hab_son
function hab_son ( var i: integer; p1,p2,p3,p4,p5,p6: integer; s: set of char ): integer;

  var
    t: set of char;
begin
  writeln ( ttyoutput, 'Begin function HAB_SON' );
  break ( ttyoutput );
  writeln ( ttyoutput, 'hab_son parameters: ', i, p1, p2, p3, p4, p5, p6 );
  if s = ['a'..'c', chr( 0 )..chr(10), '<', 'A', ' ', '^']
    then writeln ( ttyoutput, 'parameter s ok' )
    else writeln ( ttyoutput, 'parameter s NOT ok' );
  i := 2 * i;
  writeln ( ttyoutput, 'i: ', i );
  hab_son := i;
  i := 32;
  writeln ( ttyoutput, 'hab_son: ', hab_son, '  i: ', i );
  t := s + ['%'];
  if t = ['a'..'c', chr( 0 )..chr( 10 ), '<', 'A', ' ', '^', '%' ]
    then writeln ( ttyoutput, 't is ok' )
    else writeln ( ttyoutput, 't is NOT ok' );
  writeln ( ttyoutput, 'End function HAB_SON' );
  break ( ttyoutput );
end;
$PAGE hab

public procedure hab ( var a: integer; gen1: gen_array );

var 
  i, j: integer;
  hab_son_var: hab_son_proc;

begin
  writeln ( ttyoutput, 'Begin procedure HAB' );
  break ( ttyoutput );
  i := a + 3;
  writeln ( ttyoutput, 'parameter a: ', a );
  write ( ttyoutput, 'parameter gen1: ' );
  for j := lowerbound ( gen1 ) to upperbound ( gen1 ) do
    write ( ttyoutput, gen1[ j ] );
  writeln ( ttyoutput );
  hab_son_var := hab_son;
  pub_var := 3000007b;
  writeln ( ttyoutput, 'pub_var: ', pub_var );
  j := i + a;
  writeln ( ttyoutput, 'j: ', j );
  oof ( j );
  a := hab_son_var ( i, 1, 2, 4, 8, 16, 32, ['a'..'c',chr(0)..chr(10),'<','A',' ','^'] );
  writeln ( ttyoutput, 'a: ', a );
  j := 1;
  i := gen1[ j ];
  writeln ( ttyoutput, 'i: ', i );
$IF GENERICS  j := size ( gen1, lowerbound(gen1), upperbound(gen1) );
$IFNOT GENERICS j := size ( gen1, upperbound(gen1) );
  writeln ( ttyoutput, 'j: ', j );
  writeln ( ttyoutput, 'End of procedure HAB' );
end.
  