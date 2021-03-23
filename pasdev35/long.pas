program mathtest;

(* This program produces the run times for each math operation 
   provided by PASCAL.  This value is computed as follows:
     1) Perform the operation 2500 times with each of four sets of
        operands.
     2) Subtract the time it takes to execute the FOR loops and
        assignment statements without performing the math.
     3) Divide the result by 10000.

   The test is performed on real operands, except for integer operations.
   (DIV and MOD).

   The default for this program is double precision.  Single precision 
   can be tested by enabling 'singleprec', when the program.
*)
$IFNOT singleprec type dreal = minimum(real)..maximum(real) prec 16;
$IF    singleprec type dreal = real;
var
   timein, timeout: integer;
   overhead : real;
   a,b,c,d,e : dreal;
   i,w,x,y,z : integer;
 
begin

   a := .123456789;
   b := .987654321;
   c := .333333333;
   d := .777777777;

   w := 2;
   x := 987654321;
   y := 1234567890;
   z := 37;

   rewrite(tty);

   timein := time();
   for i := 1 to 2500 do e := a;
   for i := 1 to 2500 do e := b;
   for i := 1 to 2500 do e := c;
   for i := 1 to 2500 do e := d;
   overhead := time() - timein;
   writeln(tty,'Overhead took ', overhead/10000, 'ms.');
 
   timein := time();
   for i := 1 to 2500 do e := a+b; 
   for i := 1 to 2500 do e := b+c; 
   for i := 1 to 2500 do e := c+d; 
   for i := 1 to 2500 do e := a+d; 
   timeout := time() - timein;
   writeln(tty,'Each addition took', (timeout - overhead) / 10000, ' ms');
   
   timein := time();
   for i := 1 to 2500 do e := a-b;
   for i := 1 to 2500 do e := b-c;
   for i := 1 to 2500 do e := c-d;
   for i := 1 to 2500 do e := d-a;
   timeout := time() - timein;
   writeln(tty,'Each subtraction took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := a*b;
   for i := 1 to 2500 do e := b*c;
   for i := 1 to 2500 do e := c*d;
   for i := 1 to 2500 do e := a*d;
   timeout := time() - timein;
   writeln(tty,'Each multiplication took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := a/b;
   for i := 1 to 2500 do e := b/c;
   for i := 1 to 2500 do e := c/d;
   for i := 1 to 2500 do e := d/a;
   timeout := time() - timein;
   writeln(tty,'Each division took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := w div x; 
   for i := 1 to 2500 do e := x div y; 
   for i := 1 to 2500 do e := y div z; 
   for i := 1 to 2500 do e := w div z; 
   timeout := time() - timein;
   writeln(tty,'Each integer division took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := w mod x; 
   for i := 1 to 2500 do e := x mod y; 
   for i := 1 to 2500 do e := y mod z; 
   for i := 1 to 2500 do e := w mod z; 
   timeout := time() - timein;
   writeln(tty,'Each mod calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := ln(a);
   for i := 1 to 2500 do e := ln(b);
   for i := 1 to 2500 do e := ln(c);
   for i := 1 to 2500 do e := ln(d);
   timeout := time() - timein;
   writeln(tty,'Each natural log calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := exp(a);
   for i := 1 to 2500 do e := exp(b);
   for i := 1 to 2500 do e := exp(c);
   for i := 1 to 2500 do e := exp(d);
   timeout := time() - timein;
   writeln(tty,'Each e**x calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := arctan(a);
   for i := 1 to 2500 do e := arctan(b);
   for i := 1 to 2500 do e := arctan(c);
   for i := 1 to 2500 do e := arctan(d);
   timeout := time() - timein;
   writeln(tty,'Each arctan calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := sin(a);
   for i := 1 to 2500 do e := sin(b);
   for i := 1 to 2500 do e := sin(c);
   for i := 1 to 2500 do e := sin(d);
   timeout := time() - timein;
   writeln(tty,'Each sine calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := sqrt(a);
   for i := 1 to 2500 do e := sqrt(b);
   for i := 1 to 2500 do e := sqrt(c);
   for i := 1 to 2500 do e := sqrt(d);
   timeout := time() - timein;
   writeln(tty,'Each sqrt calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := arccos(a);
   for i := 1 to 2500 do e := arccos(b);
   for i := 1 to 2500 do e := arccos(c);
   for i := 1 to 2500 do e := arccos(d);
   timeout := time() - timein;
   writeln(tty,'Each arccos calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := arcsin(a);
   for i := 1 to 2500 do e := arcsin(b);
   for i := 1 to 2500 do e := arcsin(c);
   for i := 1 to 2500 do e := arcsin(d);
   timeout := time() - timein;
   writeln(tty,'Each arcsin calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := cosh(a);
   for i := 1 to 2500 do e := cosh(b);
   for i := 1 to 2500 do e := cosh(c);
   for i := 1 to 2500 do e := cosh(d);
   timeout := time() - timein;
   writeln(tty,'Each cosh calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := tanh(a);
   for i := 1 to 2500 do e := tanh(b);
   for i := 1 to 2500 do e := tanh(c);
   for i := 1 to 2500 do e := tanh(d);
   timeout := time() - timein;
   writeln(tty,'Each tanh calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := log(a);
   for i := 1 to 2500 do e := log(b);
   for i := 1 to 2500 do e := log(c);
   for i := 1 to 2500 do e := log(d);
   timeout := time() - timein;
   writeln(tty,'Each base 10 logarithm calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := sinh(a);
   for i := 1 to 2500 do e := sinh(b);
   for i := 1 to 2500 do e := sinh(c);
   for i := 1 to 2500 do e := sinh(d);
   timeout := time() - timein;
   writeln(tty,'Each sinh calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := tan(a);
   for i := 1 to 2500 do e := tan(b);
   for i := 1 to 2500 do e := tan(c);
   for i := 1 to 2500 do e := tan(d);
   timeout := time() - timein;
   writeln(tty,'Each tan calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := cotan(a);
   for i := 1 to 2500 do e := cotan(b);
   for i := 1 to 2500 do e := cotan(c);
   for i := 1 to 2500 do e := cotan(d);
   timeout := time() - timein;
   writeln(tty,'Each cotan calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := cos(a);
   for i := 1 to 2500 do e := cos(b);
   for i := 1 to 2500 do e := cos(c);
   for i := 1 to 2500 do e := cos(d);
   timeout := time() - timein;
   writeln(tty,'Each cosine calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := abs(a);
   for i := 1 to 2500 do e := abs(b);
   for i := 1 to 2500 do e := abs(c);
   for i := 1 to 2500 do e := abs(d);
   timeout := time() - timein;
   writeln(tty,'Each absolute value calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := sqr(a);
   for i := 1 to 2500 do e := sqr(b);
   for i := 1 to 2500 do e := sqr(c);
   for i := 1 to 2500 do e := sqr(d);
   timeout := time() - timein;
   writeln(tty,'Each square calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := trunc(a);
   for i := 1 to 2500 do e := trunc(b);
   for i := 1 to 2500 do e := trunc(c);
   for i := 1 to 2500 do e := trunc(d);
   timeout := time() - timein;
   writeln(tty,'Each truncation calculation took', (timeout - overhead) / 10000, ' ms');

   timein := time();
   for i := 1 to 2500 do e := round(a);
   for i := 1 to 2500 do e := round(b);
   for i := 1 to 2500 do e := round(c);
   for i := 1 to 2500 do e := round(d);
   timeout := time() - timein;
   writeln(tty,'Each round calculation took', (timeout - overhead) / 10000, ' ms');

end.
   