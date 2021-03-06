$IF PLOT1 module PLOT1;
$IF PLOT2 module PLOT2;

$IF VER1 Const VERSTR : String := ' Version 1';
$IF VER2 Const VERSTR : String := ' Version 2';

$IF PLOT1 Const PLOTID : String := 'PLOT1' || VERSTR;
$IF PLOT2 Const PLOTID : String := 'PLOT2' || VERSTR;

$IF PLOT1 const PLOTID_NO : Integer := 1;
$IF PLOT2 const PLOTID_NO : Integer := 2;

$IF PLOT2 Public Var PUB_PLOT2_VAR: String:= 'Public ' || PLOTID ||' Var String.';
$IF PLOT1 Public Var PUB_PLOT1_VAR: String:= 'Public ' || PLOTID ||' Var String.';
$IF PLOT1 Public Const PUB_PLOT1_CST : String := 'Public ' || PLOTID ||' Const string.';
$IF PLOT2 Public Const PUB_PLOT2_CST : String := 'Public ' || PLOTID ||' Const string.';

Type PROC_ARY_TYP = Array [ 0..4 ] of String [ 5 ];
Const PROC_ARY : PROC_ARY_TYP := ('MAIN', 'PLOT1', 'PLOT2', 'PLOT1', 'PLOT2' );

External Procedure MAIN1 ( integer );
$IF PLOT1 External Procedure PLOT2 ( integer );
$IF PLOT2 External Procedure PLOT1 ( integer );

External Var PUB_MAIN_VAR : String;
External Const PUB_MAIN_CST : String;

$IF PLOT1 External Var PUB_PLOT2_VAR : String;
$IF PLOT1 External Const PUB_PLOT2_CST : String;
$IF PLOT2 External Var PUB_PLOT1_VAR : String;
$IF PLOT2 External Const PUB_PLOT1_CST : String;
var I : integer;
$PAGE PLOT OVERLAY
$IF PLOT1 Public Procedure PLOT1 ( FROM : Integer );
$IF PLOT2 Public Procedure PLOT2 ( FROM : Integer );

Begin

  I := I - 3;		(* Makes PLOT different from LINK *)
$IF PLOT2 I := I + 1 + 2 + 3 - 3 - 2 - 1;
$IF VER2 I := I -3 -2 -1;

  writeln ( tty , PLOTID , ' called from ' , PROC_ARY [ FROM ] );

  loop

    write ( tty , PLOTID , '.  Enter test number: ' );
    break ( tty );
    readln ( tty );
    read ( tty , I );
  exit if I = 0;
    case I of
      1: main1 ( 1 );
$IF PLOT1 2: PLOT2 ( PLOTID_NO );
$IF PLOT2 2: PLOT1 ( PLOTID_NO );
      3: begin
	   writeln ( tty ,'Displaying MAINS public consts and vars.' );
	   writeln ( tty , 'MAINS public var: ' , PUB_MAIN_VAR );
	   writeln ( tty , 'MAINS public const: ' , PUB_MAIN_CST )
	 end;
      4: begin
	   writeln ( tty , 'Displaying ',PLOTID,' public consts and vars.' );
$IF PLOT1 writeln ( tty , PLOTID , ' public var: ' , PUB_PLOT2_VAR );
$IF PLOT2 writeln ( tty , PLOTID , ' public var: ' , PUB_PLOT1_VAR );
$IF PLOT1 writeln ( tty , PLOTID , ' public const: ' , PUB_PLOT2_CST )
$IF PLOT2 writeln ( tty , PLOTID , ' public const: ' , PUB_PLOT1_CST )
	 end;
      others: writeln ( tty , 'Try again. 0..4 only!' )
    end

  end;		(* loop *)

end.
   