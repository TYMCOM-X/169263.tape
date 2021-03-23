program mathtest;

(* This program produces the run times for each math operation 
   provided by PASCAL.  This value is computed as follows:
     1) Perform the operation count div 4 times with each of four sets of
        operands.
     2) Subtract the time it takes to execute the FOR loops and
        assignment statements without performing the math.
     3) Divide the result by count.

   The test is performed on real operands, except for integer operations.
   (DIV and MOD).

   The default for this program is double precision.  Single precision 
   can be tested by enabling 'singleprec', when the program.
*)
$IFNOT singleprec type dreal = minimum(real)..maximum(real) prec 16;
$IF    singleprec type dreal = real;
var
   timein, timeout: integer;
   count: integer;
   str: string;
   overhead : real;
   a,b,c,d,e : dreal;
   i,w,x,y,z : integer;
 
function timer: integer;

begin
  timer :=
$IFNOT M68	runtime ();
$IF    M68	time ();
end;

begin

   a := .123456789;
   b := .987654321;
   c := .333333333;
   d := .777777777;

   w := 2;
   x := 987654321;
   y := 1234567890;
   z := 37;

   rewrite (tty); open (tty);
   loop
     write (tty,'Number of iterations: '); break; readln (tty);
     read (tty,str);
     getstring (str,count);
   exit if (iostatus = io_ok) andif (count > 0);
     writeln (tty,'What?');
   end;

   rewrite (output,'math.ls');

   count := ((count + 3) div 4) * 4;

   timein := timer();
   for i := 1 to count div 4 do e := a;
   for i := 1 to count div 4 do e := b;
   for i := 1 to count div 4 do e := c;
   for i := 1 to count div 4 do e := d;
   overhead := timer() - timein;
   writeln('Overhead took ', overhead/count, 'ms.');
 
   timein := timer();
   for i := 1 to count div 4 do e := a+b; 
   for i := 1 to count div 4 do e := b+c; 
   for i := 1 to count div 4 do e := c+d; 
   for i := 1 to count div 4 do e := a+d; 
   timeout := timer() - timein;
   writeln('Each addition took', (timeout - overhead) / count, ' ms');
   
   timein := timer();
   for i := 1 to count div 4 do e := a-b;
   for i := 1 to count div 4 do e := b-c;
   for i := 1 to count div 4 do e := c-d;
   for i := 1 to count div 4 do e := d-a;
   timeout := timer() - timein;
   writeln('Each subtraction took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := a*b;
   for i := 1 to count div 4 do e := b*c;
   for i := 1 to count div 4 do e := c*d;
   for i := 1 to count div 4 do e := a*d;
   timeout := timer() - timein;
   writeln('Each multiplication took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := a/b;
   for i := 1 to count div 4 do e := b/c;
   for i := 1 to count div 4 do e := c/d;
   for i := 1 to count div 4 do e := d/a;
   timeout := timer() - timein;
   writeln('Each division took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := w div x; 
   for i := 1 to count div 4 do e := x div y; 
   for i := 1 to count div 4 do e := y div z; 
   for i := 1 to count div 4 do e := w div z; 
   timeout := timer() - timein;
   writeln('Each integer division took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := w mod x; 
   for i := 1 to count div 4 do e := x mod y; 
   for i := 1 to count div 4 do e := y mod z; 
   for i := 1 to count div 4 do e := w mod z; 
   timeout := timer() - timein;
   writeln('Each mod calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := ln(a);
   for i := 1 to count div 4 do e := ln(b);
   for i := 1 to count div 4 do e := ln(c);
   for i := 1 to count div 4 do e := ln(d);
   timeout := timer() - timein;
   writeln('Each natural log calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := exp(a);
   for i := 1 to count div 4 do e := exp(b);
   for i := 1 to count div 4 do e := exp(c);
   for i := 1 to count div 4 do e := exp(d);
   timeout := timer() - timein;
   writeln('Each e**x calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := arctan(a);
   for i := 1 to count div 4 do e := arctan(b);
   for i := 1 to count div 4 do e := arctan(c);
   for i := 1 to count div 4 do e := arctan(d);
   timeout := timer() - timein;
   writeln('Each arctan calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := sin(a);
   for i := 1 to count div 4 do e := sin(b);
   for i := 1 to count div 4 do e := sin(c);
   for i := 1 to count div 4 do e := sin(d);
   timeout := timer() - timein;
   writeln('Each sine calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := sqrt(a);
   for i := 1 to count div 4 do e := sqrt(b);
   for i := 1 to count div 4 do e := sqrt(c);
   for i := 1 to count div 4 do e := sqrt(d);
   timeout := timer() - timein;
   writeln('Each sqrt calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := arccos(a);
   for i := 1 to count div 4 do e := arccos(b);
   for i := 1 to count div 4 do e := arccos(c);
   for i := 1 to count div 4 do e := arccos(d);
   timeout := timer() - timein;
   writeln('Each arccos calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := arcsin(a);
   for i := 1 to count div 4 do e := arcsin(b);
   for i := 1 to count div 4 do e := arcsin(c);
   for i := 1 to count div 4 do e := arcsin(d);
   timeout := timer() - timein;
   writeln('Each arcsin calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := cosh(a);
   for i := 1 to count div 4 do e := cosh(b);
   for i := 1 to count div 4 do e := cosh(c);
   for i := 1 to count div 4 do e := cosh(d);
   timeout := timer() - timein;
   writeln('Each cosh calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := tanh(a);
   for i := 1 to count div 4 do e := tanh(b);
   for i := 1 to count div 4 do e := tanh(c);
   for i := 1 to count div 4 do e := tanh(d);
   timeout := timer() - timein;
   writeln('Each tanh calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := log(a);
   for i := 1 to count div 4 do e := log(b);
   for i := 1 to count div 4 do e := log(c);
   for i := 1 to count div 4 do e := log(d);
   timeout := timer() - timein;
   writeln('Each base 10 logarithm calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := sinh(a);
   for i := 1 to count div 4 do e := sinh(b);
   for i := 1 to count div 4 do e := sinh(c);
   for i := 1 to count div 4 do e := sinh(d);
   timeout := timer() - timein;
   writeln('Each sinh calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := tan(a);
   for i := 1 to count div 4 do e := tan(b);
   for i := 1 to count div 4 do e := tan(c);
   for i := 1 to count div 4 do e := tan(d);
   timeout := timer() - timein;
   writeln('Each tan calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := cotan(a);
   for i := 1 to count div 4 do e := cotan(b);
   for i := 1 to count div 4 do e := cotan(c);
   for i := 1 to count div 4 do e := cotan(d);
   timeout := timer() - timein;
   writeln('Each cotan calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := cos(a);
   for i := 1 to count div 4 do e := cos(b);
   for i := 1 to count div 4 do e := cos(c);
   for i := 1 to count div 4 do e := cos(d);
   timeout := timer() - timein;
   writeln('Each cosine calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := abs(a);
   for i := 1 to count div 4 do e := abs(b);
   for i := 1 to count div 4 do e := abs(c);
   for i := 1 to count div 4 do e := abs(d);
   timeout := timer() - timein;
   writeln('Each absolute value calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := sqr(a);
   for i := 1 to count div 4 do e := sqr(b);
   for i := 1 to count div 4 do e := sqr(c);
   for i := 1 to count div 4 do e := sqr(d);
   timeout := timer() - timein;
   writeln('Each square calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := trunc(a);
   for i := 1 to count div 4 do e := trunc(b);
   for i := 1 to count div 4 do e := trunc(c);
   for i := 1 to count div 4 do e := trunc(d);
   timeout := timer() - timein;
   writeln('Each truncation calculation took', (timeout - overhead) / count, ' ms');

   timein := timer();
   for i := 1 to count div 4 do e := round(a);
   for i := 1 to count div 4 do e := round(b);
   for i := 1 to count div 4 do e := round(c);
   for i := 1 to count div 4 do e := round(d);
   timeout := timer() - timein;
   writeln('Each round calculation took', (timeout - overhead) / count, ' ms');

end.
    