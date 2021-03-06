
(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

(* program number 148*)
(*TEST 6.6.2-7, CLASS=CONFORMANCE*)
(* This test checks that functions are not prohibited from altering
  their environment (ie. side effects). Though side effects are
  generally not to be encouraged, they are part of standard Pascal
  and do have genuine uses. Functions with side effect occur
  elsewhere in the validation suite. *)
program t6p6p2d7;
type
   ptrtochar = ^char;
var
   c1,c2,c3,dummy:char;
   p1,p2:ptrtochar;
function testa(ptr:ptrtochar):char;
   (*sneakiest, uses pointers*)
var
   pp:ptrtochar;
begin
   pp:=ptr;
   pp^ := 'P';

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   testa:='1'
end;
procedure assign;
   (*used by testb*)
begin
   c1:='A'
end;
function testb:char;
   (*sneaky, calls a procedure*)
begin
   assign;
   testb:='2'
end;
function testc:char;
   (*blatantly changes the environment via write*)
begin
   write(' ',p1^,c1,c2,c3,p2^);
   testc:='6'
end;
function testd:ptrtochar;

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

   (*blatantly sneaky: modifying the environment via new
      and then passing it out*)
var
   pp:ptrtochar;
begin
   new(pp);
   pp^:='.';
   testd:=pp
end;
function teste:char;
   (*the most used side effect:global access*)
begin
   c2:='S';
   teste:='3'
end;
function testf(var c:char):char;
   (*straightforward*)
begin
   c:='S';
   testf:='4'

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

end;
begin
        rewrite(output,'suite.txt',[preserve]); (*of main program*)       writeln('suite program #148');
   new(p1);
   p1^:='F'; c1:='A'; c2:='I'; c3:='L';
   p2:=nil;
      (*which defines all variables*)
   dummy:=testa(p1);
   dummy:=testb;
   dummy:=teste;
   dummy:=testf(c3);
   p2:=testd;
   dummy:=testc;
   writeln('..6.6.2-7, ENVIRONMENT')
end.

(****           (c) 1981 Strategic Information                  ****)
(****           division of Ziff Davis Publishing Co.           ****)

  