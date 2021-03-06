
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 252*)
(*TEST 6.8.3.5-12, CLASS=DEVIANCE*)
(* Some processors allow subrange-like lists to be used as case-constant
  elements. This test checks to see if this is allowed. It is
  not standard Pascal. The compiler deviates if the program
  compiles and prints DEVIATES. *)
program t6p8p3p5d12;
var
   thing:(a,b,c,d,e,f);
begin
        rewrite(output,'suite.txt',[preserve]);       writeln('suite program #252');
   thing:=a;
   while thing<>f do begin
      case thing of
      a..d: thing := succ(thing);
      e:    thing:=f
      end
   end;
   writeln(' DEVIATES...6.8.3.5-12, CASE CONSTANTS')
end.
   