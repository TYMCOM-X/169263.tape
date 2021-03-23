$TITLE MATH TEST ROUTINE

program mathtest;

(* MATHTEST - test addition, multiplication, subtraction, division,
power, mod, abs, sqr, min, max, sqrt, sin, cos, tan, cotan, arcsin,
arccos, cosh, sinh, tanh, ln, exp, and log for integer, real and
double precision values. *)

var
   i,j,k,l,m,n,count,ibeta,iexp,irnd,it,k1,k2,k3,machep,
      maxexp,minexp,negep,ngrd:integer;
   a,b,c,d,e,f,ait,albeta,beta,eps,epsneg,one,r6,r7,
      sqbeta,w,x,xmax,xmin,xn,x1,y,z,zero,half,tenth,eight,xl,
      zz,del,two,ten,v:real;
   aa,bb,cc,dd,ee,ff:minimum(real)..maximum(real) prec 15;
   rslt:math_status;
   report:text;

const
   sngl_e:real:=0.00001;
   dbl_e:minimum(real)..maximum(real):=0.00000000000001;

procedure error(error_number:0..1000);
begin
   if rslt <> math_ok then write(tty,'   RSLT = ',ord(rslt));
   rslt:=math_ok;
   writeln(tty,'   ERROR = ',error_number);
   break(tty);
end;

function randl(x:real):real;
(*
   Returns pseudo random numbers logarithmically distributed over
   (1,exp(x)).  Thus a*randl(alog(b/a)) is logarithmically 
   distributed in (a,b).

   Other subroutines required

      exp(x) - the exponential routine
      random(x) - a function program returning random real numbers
                  uniformly distributed over (0,1).  
                                                            *)

begin

   randl:=exp(x*random);

end;

procedure machar (var ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,
                  maxexp:integer;var eps,epsneg,xmin,xmax:real);

var
   i,iz,j,k,mx:integer;
   a,b,beta,betain,betam1,one,y,z,zero:real;
   ext:boolean;

(*
   This subroutine is intened to determine the characteristics of the 
   floating-point arithmetic system that are specified below.  The first
   three are determined according to an algorithm due to M. Malcolm,
   CACM 15 (1972), pp. 949-951, incorporating some, but not all, of the
   improvements suggested by M. Gentleman and S. Marovich, CACM 17 (1974),
   pp. 276-277.  The version given here is for single precision.  

   ibeta  - the radix of the floating point representation
   it     - the number of base ibeta digits in the floating-point
            significand
   irnd   - 0 if floating-point addition chops,
            1 if floating-point addition rounds
   ngrd   - the number of guard digits for multiplication.  It is 0
            if irnd=1, or if irnd=0 and only it base ibeta digits
            participate in  thee post normalization shift of the floating-
            point significand in multiplication.  It is 1 if irnd=0 and
            more than it base ibeta digits participate in the post
            normalization shift of the floating-point significand in
            multiplication.
   machep - the largest negative integer such that 1.0+float(ibeta)
            **machep <> 1.0, except that machep is bounded below by
            -(it+3)
   negeps - the largest negative integer such that 1.0-float(ibeta)**
            negeps <> 1.0, except that negeps is bounded below by
            -(it+3)
   iexp   - the number of bits (decimal places if ibeta = 10)
            reserved for the representation of the exponent (including
            the bias or sign) of a floating-point number
   minexp - the largest in magnitude negative integer such that 
            float(ibeta)**minexp is a positive floating-point number
   maxeexp - the largest positive integer exponent for a finite
            floating-point number
   eps    - the smallest positive floating-point number such that 1.0+eps 
            <> 1.0.  In particular, if either ibeta = 2 or irnd = 0,
            eps = float(ibeta)**machep.  Otherwise, eps = (float(ibeta)
            **machep)/2
   epsneg - a small positive floating-point number such that 1.0-
            epsneg <> 1.0.  In particular, if ibeta = 2 or irnd = 0,
            epsneg=float(ibeta)**negeps.  Otherwise, epsneg=(ibeta**
            negeps)/2.  Because negeps is bounded below by -(it+3),
            epsneg may not be the smallest number which can alter 1.0 
            by subtraction.
   xmin   - the smallest non-vanishing floating-point power of thee
            radix.  In particular, xmin = float(ibeta)**minexp
   xmax   - the largest finite floating-point number.  In particular
            xmax = (1.0-epsneg)*float(ibeta)**maxexp not - on some
            machines xmax will be only the second, or perhaps third
            largest number, being too small by 1 or 2 units in the last
            digit of the significand.

latest fortran revision - October 22, 1979

latest pascal revision - April 30, 1982

author - W. J. Cody
         Argonne National Laboratory

                                                                           *)

begin
   one:=1.0;
   zero:=0.0e0;
(*
   determine ibeta, beta ala Malcolm
                                                    *)
   a:=one;
   repeat begin
      a:=a+a;
   end until (((a+one)-a)-one <> 0);
   b:=one;
   repeat b:=b+b until ((a+b)-a <> 0);
   ibeta:=round((a+b)-a);
   beta:=ibeta;
(*
   determine it, irnd
                         *)
   it:=0;
   b:=one;
   repeat begin
      it:=it+1;
      b:=b*beta;
   end until (((b+one)-b)-one <> zero);
   irnd:=0;
   betam1:=beta-one;
   if ((a+betam1)-a <> zero) then irnd:=1;
(*
   determine negep, epsneg
                               *)
   negep:=it+3;
   betain:=one / beta;
   a:=one;

   for i:=1 to negep do
      a:=a*betain;

   b:=a;
   while ((one-a)-one = zero) do
   begin
      a:=a*beta;
      negep:=negep-1;
   end;
   negep:=-negep;
   epsneg:=a;
   if (ibeta <> 2) and (irnd <> 0) then
   begin
      a:=(a*(one+a)) / (one+one);
      if ((one-a)-one <> zero) then epsneg:=a;
   end;
 (*
   determine machep, eps
                            *)
   machep:=-it-3;
   a:=b;
   while ((one+a)-one = zero) do
   begin
      a:=a*beta;
      machep:=machep+1;
   end;
   eps:=a;
   if (ibeta <> 2) and (irnd <> 0) then
   begin
      a:=(a*(one+a)) / (one+one);
      if ((one+a)-one <> 0) then eps:=a;
   end;
(*
   determine ngrd
                    *)
   ngrd:=0;
   if (irnd = 0) and (((one+eps)*one-one) <> zero) then ngrd:=1;
(*
   determine iexp, minexp, xmin

   loop to determine largest i and k =2**i such that
      (1/beta) ** (2**(i))
   does not underflow
   exit from loop is signaled by an underflow.
                                                        *)
   i:=0;
   k:=1;
   z:=betain;
   ext:=false;
   repeat begin
      y:=z;
      begin
         z:=y*y;
         exception
            math_error:   rslt:=mathstatus;
      end;
(*
   check for underflow here
                               *)
      rslt:=math_ok;
      a:=z*one;
      if (a+a = zero) or (abs(z) >= y) then ext:=true
      else begin
         i:=i+1;
         k:=k+k;
      end;
   end until ext;
   if (ibeta <> 10) then
   begin
      iexp:=i+1;
      mx:=k+k;
   end
   else
   begin
      iexp:=2;
      iz:=ibeta;
      while (k >= iz) do
      begin
         iz:=iz*ibeta;
         iexp:=iexp+1;
      end;
      mx:=iz+iz-1;
   end;
(*
   loop to determine minexp, xmin exit from loop is signaled by
   an underflow
                                                               *)
   ext:=false;
   repeat begin
      xmin:=y;
      begin
         y:=y*betain;
         exception
            math_error:   rslt:=mathstatus;
      end;
 (*
   check for underflow here
                              *)
      rslt:=math_ok;
      a:=y*one;
      if ((a+a) = zero) or (abs(y) >= xmin) then ext:=true
      else
         k:=k+1;
   end until ext;
   minexp:=-k;
(*
   determine maxexp, xmax
                            *)
   if ((mx <= k+k-3) and (ibeta <> 10)) then
   begin
      mx:=mx+mx;
      iexp:=iexp+1;
   end;
   maxexp:=mx+minexp;
(*
   adjust for machines with implicit leading bit in binary
   significand and machines with radix point at extreme right of
   significand
                                                                *)
   i:=maxexp+minexp;
   if ((ibeta = 2) and (i = 0)) then maxexp:=maxexp-1;
   if (i > 20) then maxexp:=maxexp-1;
   if (a <> y) then maxexp:=maxexp-2;
   xmax:=one-epsneg;
   if (xmax*one <> xmax) then xmax:=one-beta*epsneg;
   xmax:=xmax/(beta*beta*beta*xmin);
   i:=maxexp+minexp+3;
   if (i > 0) then
      for j:=1 to i do
      begin
         if (ibeta = 2) then xmax:=xmax+xmax;
         if (ibeta <> 2) then xmax:=xmax*beta;
      end;

      writeln(report,'******     results from machar call    ******');
      writeln(report);  writeln(report);
      writeln(report,'ibeta = ',ibeta,'  it = ',it,'  irnd = ',irnd,
         '  ngrd = ',ngrd);
      writeln(report,'machep = ',machep,'  negep = ',negep,'  iexp = ',
         iexp);
      writeln(report,'minexp = ',minexp,'  maxexp = ',maxexp,'  eps = ',
          eps,'  epsneg = ',epsneg);
      writeln(report,'xmin = ',xmin,'  xmax = ',xmax);
      writeln(report);   writeln(report);

end;


begin

   rewrite(report);
   if iostatus(report) <> io_ok then error(0);
   rewrite(tty);
(* First just try some misc math stuff on integers *)

   i:=0+1+2;
   j:=i-2;
   k:=i div j;
   l:=i*j;
   if (rslt <> math_ok) or (i <> 3) or (j <> 1) or (k <> 3)
      or (l <> 3) then error(1);

   k:=i**3;
   l:=k mod 10;
   j:=l-k;
   if (rslt <> math_ok) or (j <> -20) or (k <> 27) or (l <> 7) 
      then error(2);


(* Now some basic math stuff on reals *)

   a:=0.0+1.1+2.2;
   b:=a-2.2;
   c:=a/b;
   d:=a*b;
   if (rslt <> math_ok) or (abs(3.3-a) >= sngl_e) or (abs(1.1-b) >= sngl_e)
      or (abs(3.0-c) >= sngl_e) then error(3);

   c:=a**3;
   b:=a-c;
   if (rslt <> math_ok) or (abs(35.937-c) >= sngl_e) or
      (abs(-32.637-b) >= sngl_e) then error(4);


(* And finally some basic math stuff on double precision *)

   aa:=0.0+1.1+2.2;
   bb:=aa-2.2;
   cc:=aa/bb;
   dd:=aa*bb;
   if (rslt <> math_ok) or (abs(1.1-bb) >= dbl_e) or
      (abs(3.3-cc) >= dbl_e) or (abs(3.63-dd) >= dbl_e) then error(5);
   writeln(tty,aa,bb,cc,dd);

   cc:=aa**3;
   bb:=aa-cc;
   if (rslt <> math_ok) or (abs(35.937-cc) >= dbl_e*10) or
      (abs(-32.637-bb) >= dbl_e*10) then error(6);
   writeln(tty,bb,cc);


(* Now that the simple stuff at least hasn't died, really test integer
math *)

   i:=maximum(integer);
   i:=i+0;
   if (rslt <> math_ok) or (i <> maximum(integer)) 
      then error(7);
 
   j:=-1;
   i:=i+j;
   if (rslt <> math_ok) or (i = maximum(integer)) then error(8);

$IF m68
   begin
      i:=i+2;
      exception
         math_error:   rslt:=mathstatus;
   end;
   if rslt = math_ok then error(9);
   rslt:=math_ok;
$ENDIF

   i:=4;
   j:=trunc(79.7/i);
   k:=-5;
   l:=1000;
   m:=min(i,j,k,l);
   n:=max(i,j,k,l);
   if (rslt <> math_ok) or (m <> -5) or (n <> 1000) 
      then error(10);

   i:=3;
   j:=-3;
   l:=3;
   m:=-3;
   for count:=2 to 13 do
   begin
      k:=i**count;
      l:=l*i;
      m:=m*j;
      if (rslt <> math_ok) or (k <> l) or ((odd(count)) and (l <> -m))
         or (not(odd(count)) and (l <> m)) then
      begin
         write(tty,'    count = ',count);
         error(11);
      end;
   end;

   if k <> 3**13 then error(12);

   j:=0**i;
   if (rslt <> math_ok) or (j <> 0) then error(13);

   j:=1**k;
   if (rslt <> math_ok) or (j <> 1) then error(14);

   j:=-1**i;
   if (j <> -1) or (rslt <> math_ok) then error(15);

   begin
      j:=j**-3;
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(16);
   rslt:=math_ok;

   i:=14;
   j:=0;
   begin
      k:=i div j;
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt <> math_zero_divide) then error(17);
   rslt:=math_ok;

   i:=9;
   j:=i+1;
   k:=j-1;
   if (rslt <> math_ok) or (j <> 10) or (k <> 9)
      then error(18);

   i:=5;
   j:=i*2;
   k:=j div 2;
   if (rslt <> math_ok) or (j <> 10) or (k <> 5) 
      then error(19);

   i:=-4;
   j:=12;
   k:=j+i;
   l:=j-i;
   m:=i-j;
   if (rslt <> math_ok) or (k <> 8) or (l <> 16) or (m <> -16)
      then error(20);

   j:=-2;
   k:=i+j;
   l:=i-j;
   m:=j-i;
   if (rslt <> math_ok) or (k <> -6) or (l <> -2) or (m <> 2)
      then error(21);
  
   i:=maximum(integer);
   i:=i-12;
   i:=i-12;
   i:=i-12;
   i:=i-12;
   if (rslt <> math_ok) or (i <> (maximum(integer)-48))
      then error(22);

   i:=maximum(integer);
   j:=minimum(integer);
   k:=i+j;
   if (rslt <> math_ok) then error(23);
   
$IF m68
   begin
      i:=i**3;
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt <> math_int_ovf) then error(24);
   rslt:=math_ok;
   
   i:=maximum(integer);
   begin
      i:=i*3;
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt <> math_int_ovf) then error(25);
   rslt:=math_ok;
$ENDIF


(* This is the SQRT test.  It is directly taken from Software
Manual for the Elementary Functions by William J. Cody, Jr. and
William Waite.  The following comment is the intro to the SQRT
test.

The tests are divided into four major parts.  First is a random 
argument to determine the accuracy of the basic Newton iteration,
i.e., the accuracy of the square root when the argument lies in the
primary range and no argument reduction is necessary.  Second is
a similar test using random arguments outside the primary range, and 
with odd exponent, to test the argument reduction and subsequent
multiplication by sqrt(1/B).  Third is a series of short tests with 
special arguments, including arguments close to the largest and 
smallest in magnitude representable in the machine.  Finally there
is a test with zero as an argument to verify that an error return is 
not triggered, and a test with a negative argument to verify that an
error return is triggered.

The random argument tests measure the relative differece between the
two sides of the identity
         sqrt(x**2) = x
See chapter 3 for a discussion of the choice of this identity.  Thus 
we measure
         E = [sqrt(x**2) - x] / x
for x drawn randomly (logarithmically ditributed) from an interval
(r,s) with r > 0.

For the first test r = sqrt(1/B), where B is the radix of the floating-
point number representation (see Glossary), and s = 1.  This
particular choice of interval will contaminate the error on some
machines, particularly those with B = 16.  to see this, assume that
B = 16 and that there are b bits (not base-16 digits) available
for the representation of the significand of a floating-point
number.  As described in Chapter 2, such numbers suffer from 
"wobbling precision."  Table 4.1 compares th number of possible
significant bits in the representation of X, X*X, and SQRT(X*X) 
for X drawn from subintervals of (r,s) = (1/4,1).  Because X*X
cannot generally be represented exactly, there is a rounding error
in the argument for the SQRT routine.  We can assume that this 
inherited error (see Glossary) is limited to one unit in the last 
bit position.  From the formula in Chapter 3, the corresponding 
transmitted error (see Glossary) is then a half unit in the last bit
position.  This means that the value returned by SQRT can have only one
more bit of precission than the argument.  Thus, the precision of function
values for arguments drawn from the first interval is not so great as the
precision of the original arguments, and the testing process is contam-
inated.  The contamination is unavoidable if we are to test in the primary 
range using this identity.  Nevertheless, we expect the test to report
accuracies only slightly less than machine precision for good 
implementations of SQRT.  The results given in Table 4.2 all show a loss of 
less than one base-B digit for the MRE (see Chapter 3), and a loss of less
than half a digit for the RMS.

For the second test we choose r = 1 and s = sqrt(B).  Then X*X is 
drawn from the interval (1,B) and the previous contamination due to
wobbling precision is eliminated.  Table 4.1 illustrates the situation
for B = 16.  Test results for this interval must be interpreted 
carefully, however.  In Chapter 3 we discussed how a last-digit error in
the form 1+eps is B times more significant than a last-digit error in a
number of the form 1-eps.  That is precisely the situation we have here;
all of the calculated square roots are less than 1 for the first interval, 
and all of them are greater than 1 for this second interval.  Thus a
last digit error in this test is B times more significant than a last digit 
error in the first test, and this test should report larger errors than the
first.  The MRE might report a loss of one base-b digit on non-binary
machines, and perhaps a little more than one bit on binary machines.  
The RMS should report te loss of a large fraction of a digit on non-binary
machines, and perhaps a little more than one bit on binary machines.  The
results given in Table 4.2 illustrate this.  The library program tested
on the IBM machine is superb; the identity was always satisfied
exactly.  The library routine on the CDC machine is also superb;
the identity was satisfied exactly for all except a handful of arguments,
and the MRE reported a loss of only one bit.  Our algorithm fared a 
little worse in each case.  The MRE for both the IBM and CDC versions was 
limited to the last bit of the result, however.  It occurred for
x=1.0006 on the IBM machine, and for x=1.0060 on the CDC machine.


program to test sqrt

data required

    none

subprograms required from this package

    machar - an environmental inquiry program providing information
             on the floatine-point arithmetic system.  Note that the
             call to machar can be deleted provided the following six
             parameters are assigned the values indicated

             ibeta - the radix of the floating-point system
             it    - the number of base-ibeta digits in the significand
                     of a floatiint number
             eps   - the smallest positive floating-point number such
                     that 1.0+eps <> 1.0
             epsneg- the smallest positive floating-point number such
                     that 1.0 - epsneg <> 1.0
             xmin  - the smallest non-vanishing floating-point power
                     of the radix
             xmax  - the largest finite floating-point no.

  randl(x) - a function subprogram returning logarithmically distributed 
             random real numbers.  In particular,
                  a * randl(log(b/a))
             is logarithmically distributed over (a,b)

    ran(k) - a function subprogram returning random real numbers 
             uniformly distributed over (0,1)


standard pascal subprograms required
    abs, alog, amax1, float, sqrt

latest revision from book - August 2, 1979

latest pascal revision - April 29, 1982

author - W. J. Cody
         Argonne National Laboratory

                                                                      *)


   writeln(report);   writeln(report);
   writeln(report,'******    sqrt tests    ******');
   writeln(report);   writeln(report);
   machar(ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp,eps,
      epsneg,xmin,xmax);
   beta:=ibeta;
   sqbeta:=sqrt(beta);
   albeta:=ln(beta);
   ait:=it;
   one:=1.0e0;
   zero:=0.0e0;
   a:=one / sqbeta;
   b:=one;
   n:=2000;
   xn:=n;
(*
    random argument accuracy tests
                                        *)
   a:=random(.2);           (* initialize random so that program will run the
                            same way every time *)
   for j:=1 to 2 do
   begin
      c:=ln(b/a);
      k1:=0;
      k3:=0;
      x1:=zero;
      r6:=zero;
      r7:=zero;

      for i:=1 to n do
      begin
         x:=a * randl(c);
         y:=x * x;
         z:=sqrt(y);
         w:=(z - x) / x;
         if (w > zero) then k1:=k1+1;
         if (w < zero) then k3:=k3+1;
         w:=abs(w);
         if (w > r6) then
         begin
            r6:=w;
            x1:=x;
         end;
         r7:=r7 + (w * w);
      end;

      k2:=n - k1 - k3;
      r7:=sqrt(r7/xn);
      writeln(report);   writeln(report);
      writeln(report,'test of sqrt(x*x) - x');
      writeln(report,'=====================');
      writeln(report);
      writeln(report,n:7,' random arguments were tested from the interval ',
         '      (',a,',',b,')');
      writeln(report);
      writeln(report);
      writeln(report,'sqrt(x) was larger ',k1:6,' times, agreed ',k2:6,
         ' times, and was smaller ',k3:6,' times.');
      writeln(report);
      writeln(report);
      writeln(report,'there are ',it:4,' base ',ibeta:4,' significant ',
         'digits in a floating-point number');
      w:=-999.0e0;
      if (r6 <> zero) then w:=ln(abs(r6))/albeta;
      writeln(report,'the maximum relative error of ',r6,' = ',
         ibeta:4,' **',w);
      writeln(report,'    occurred for x =',x1);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base ',ibeta:4,' significant ',
         'digits is ',w);
      w:=-999.0e0;
      if (r7 <> zero) then w:=ln(abs(r7))/albeta;
      writeln(report,'the root mean square relative error was ',r7,
         ' = ',ibeta:4,' **',w);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base ',ibeta:4,' significant ',
         'digits is ',w);
      a:=one;
      b:=sqbeta;
   end;

(* 
    special tests
                      *)
   writeln(report);   writeln(report);
   writeln(report,'test of special arguments');
   writeln(report,'=========================');   writeln(report);
   x:=xmin;
   y:=sqrt(x);
   writeln(report,'sqrt(xmin) = sqrt(',x,') = ',y);
   x:=one-epsneg;
   y:=sqrt(x);
   writeln(report,'sqrt(1-epsneg) = sqrt(1 - ',epsneg,') = ',y);
   x:=one;
   y:=sqrt(x);
   writeln(report,'sqrt(1.0) = sqrt( ',x,') = ',y);
   x:=one + eps;
   y:=sqrt(x);
   writeln(report,'sqrt(1+eps) = sqrt(1+',eps,') = ',y);
   x:=xmax;
   y:=sqrt(x);
   writeln(report,'sqrt(xmax) = sqrt(',x,') = ',y);
(*
    test of error returns
                              *)
   writeln(report);   writeln(report);
   writeln(report,'test of error returns');
   writeln(report,'=====================');
   writeln(report);
   x:=zero;
   y:=sqrt(x);
   if (rslt <> math_ok) or (y <> 0) then error(26);
   x:=-one;
   begin
      y:=sqrt(x);
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(27);
   rslt:=math_ok;
   writeln(report,'this concludes the tests for sqrt');

(*
    end of sqrt test
                        *)

(*
This is the ln/log test.  It is directly taken from Software Manual for
the Elementary Functions by William J. Cody, Jr. and William Waite.
The following comment is the intro to the LN/LOG test.

The tests are divided into five major parts.  First is a random 
argument test of ln(x) for x very close to 1, checking the accuracy of
the computation of s.  Seecond are tests of both ln(x) and log(x)
for x in the primary range, checking the accuracy of the evaluation of 
ln(g) or log(g), as appropriate, and the conversion between natural and
common logarithms.  Third is a test of ln(x) for x reasonable large,
checking the accuracy of the computations involving the exponents m and n.
Fourth is a series of short tests with special arguments, and a cursory
check of the property
      ln(x) = -ln(1/x)

Finally, the error returns are checked for x<=0.

The evaluation of the lagarithm is relatively insensitive to small errors
in the argument except for arguments close to 1, as we noted in the
general discussion above.  To see this analytically, let
      y = ln(w).
Then
      dy = dw/w,
and we see that thee absolute error in ln(w) is roughly equal to the
relativ error in th argument w.  This assures us that small errors in
the argument have little effect on the accuracy of the function value
except when ln(x) is small in magnitude, i.e., except for arguments
close to 1.

The first test compares ln(x) for x close to 1 against the truncated
Taylor series
      TS(x) = - sum from j=1 to 4 of ( ((1-x)**j)/j )
with the truncation error approximately bounded in magnitude by
((1-x)**5)/5.  Let X = f*B*m, 1/B<=f<1, where B is the radix of the
floating-point number system (see Glossary), and assume there are t
base-B digits in the representation of f.  Then, if X is restricted so
that abs(X-1)<=B**(-t/2), TS(X) can be used to computee ln(X) to
within rounding error on the machine, provided care is taken in 
evaluating 1-X and in combining the various terms of TS.  Because the
test procedure involves the direct comparison of two evaluations of
ln(X), the possibility of systematic error exists were the subroutine 
being tested to implement this same Taylor series, but there is no
problem for implementations of the algorithm advocated here.  We expect
the test program to report accuracies approaching machine precision
in this first test.  The results tabulated in Table 5.1 generally show
a loss of one base-B digit or less for the MRE (see Chapter 3), and a
loss of a small fraction of a digit for the RMS.  The larger errors 
for our program on the GP L3055 are probably due to the final multi-
plication by ln(10).  We do not know the reason for the larger than expected
MRE for the FTN 4.7 routine on the CDC 7600.  Arguments close to 1.0
are handled carefully, because the errors would be much larger if they 
were not.  In any case the errors are not large enough to be of concern.

Although tests for ln(x) and log(x) over the primary range of 
arguments should differ slightly for decimal and non-decimal machines,
we wish to use the same procedures for both classes of machines.  We
therefore assume that ln(x) is the primary computation and log(x) a
secondary one on non-decimal machines and the the roles are reversed on
decimal machines.  Then in places where the test procedures should 
differ slightly, our procedures favor non-decimal machines for ln(x)
and decimal machines for log(x).

The second test checks ln(x) for x in the primary range with the identity
      ln(x) = ln(17x/16) - ln(17/16).
Thus we measure
      E = (ln(x) - [ln(17x/16) - ln(17/16)]) / ln(x)
for 1/sqrt(2) <= x <= 15/16.  The restricted range assures that both x and
17x/16 have the same exponents m and n in the algorithm described
earlier and that neither argument gets too close to 1.  There are two
possible sources of error contamination in using this identity.  The
first is the roundoff error introduced in computing 17x/16 from the
assumed exact argument x.  This roundoff error is eliminated on
non-decimal machines by perturbing the machine argument X to a nearby
argument X' with the last four bits zeros.  The Fortran statement
      X = (X + 8.0E0) - 8.0E0
suitably modified to foil optimizing compilers, accomplishes the task on
most contemporary computers for X's drown from the primary interval.

The exceptions are those machines in which the active arithmetic
registers carry more significance than the storage registers.  It is
necessary to force the storage and retrieval of intermediate results in
this purification process on such machines (see Gentleman and Marovich
[1974]).  The four trailing zero bits in X' are enough to eensure that
X' + X'/16 = 17X'/16 is also an exact machine number.

The second source of contamination is in the subtraction of ln(17/16).
This subtraction is accomplished in a pseudo multiple precision by 
expressing ln(17/16) as 31/512 (which is exactly representable in
five bits or eight decimal places) plus a remainder term and performing 
the subtraction in two steps.  These constants should be adjusted 
appropriately on decimal machines carrying fewer than eight digits of
significance.  With these precautions, the test procedure is quite stable,
and a good logarithm routine will return an MRE = max(abs(E)) close
to machine precision.  On decimal machines the division of X by 16 is
not exact, so for them the MRE will probably be slightly larger.  We 
therefor expect our teest program to report MRE values for this test that 
are similar to those reported for the first test.  the RMS values will
probable_y be slightly greater.  Table 5.1 lists typical results.

The analogous test for log(x), and our third test, employs the identity
      log(x) = log(11x/10) - log(1.1)
over the range 1/sqrt(10) ,= x ,- .9.  The auxila_liary argument 11x/10
is obtained as x + x/10 after argument puification using the Fortran
statement
      X = (X + 8.0E0) - 8.0e0.
As before, the subtraction of log(1.1) is carried out in pseudo multiple
precision by expressing log(1.1) as 21/512 (expressible in eight decimal
places or five bits) plus a remainder term.  These constants should be
adjusted appropriately on decimal machines carrying fewer than eight
digits of significance.  A good implementation of LOG on decimal 
machines should return an MRE only slightly larger than the precision of
the machine.  For non-decimal machines larger errors may be found,
primarily because division by 10 is not exact even for purified arguments
on such machines.  Typical test results are again given in Table 5.1.
Note in particular the expected increase in error for the programs on
binary machines.

for x outside the primary range we measure the relative error
      E = [ln(x**2) - 2 ln(x)] / ln(x**2)
in the identity
      ln(x**2) = 2 ln(x).
In particular, we choose the interval 16 <= x <= 240.  In this argument
range the machine representations of x and x**2 have different exponents,
thus minimizing the possibility of undetected systematic error.  Because
the logarithm computation is insensitive to small argument perturbations
for larger x, no argument purification is necessary.  For the same reason,
only gross errors in the handling of the exponents m and n are 
detectable by any identity test, and an implementation of LOG that
looked bad in the previous teests may even look good in this test
(see Table 5.1).


program to test log

data required
    none

subprograms required from this package

    machar - an environmental inquiry program providing 
             information on the floating-point arithmetic
             system.  Note that the call to machar can
             be deleted provided the following four
             parameters are assigned the values indicated

             ibta - the radix of the floating-point system
             it    - the number of base-ibeta digits in the
                     significand of a floatin-point number
             xmin  - the smallest non-vanishing floating-point
                     power of the radix
             xmax  - the largest finite floating-point no.

    ran(k) - a function subprogra returning random real 
             numbers uniformly distributeed over (0,1)

standard fortran subprograms reqired
    abs, alog, alog10, amax1, float, sign, sqrt

latest revision - december 6, 1979

latest pascal revision - may 5, 1982

author - W. J. Cody
         Argonne National Laboratory
                                                            *)

   writeln(report);   writeln(report);
   writeln(report,'******    ln/log test    ******');
   writeln(report);   writeln(report);
   machar(ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,maxexp,
          eps,epsneg,xmin,xmax);
   beta:=ibeta;
   albeta:=ln(beta);
   ait:=it;
   j:=it div 3;
   zero:=0.0e0;
   half:=0.5e0;
   eight:=8.0e0;
   tenth:=0.1e0;
   one:=1.0e0;
   c:=one;

   for i:=1 to j do
      c:=c/beta;

   b:=one+c;
   a:=one-c;
   n:=2000;
   xn:=n;
   x:=random(0);     (* initialize random *)
(*
   random argument accuracy tests
                                   *)
   for j:=1 to 4 do
   begin
      k1:=0;
      k3:=0;
      x1:=zero;
      r6:=zero;
      r7:=zero;
      del:=(b-a)/xn;
      xl:=a;
   
      for i:=1 to n do
      begin
         x:=del*random+xl;
         if (j = 1) then
         begin
            y:=(x-half)-half;
            zz:=ln(x);
            z:=one/3.0e0;
            z:=y*(z-y/4.0e0);
            z:=(z-half)*y*y+y;
         end
         else
         if (j = 2) then
         begin
            x:=(x+eight)-eight;
            y:=x+x/16.0e0;
            z:=ln(x);
            zz:=ln(y)-7.7746816434842581e-5;
            zz:=zz-31.0e0/512.0e0;
         end
         else
         if (j = 3) then
         begin
            x:=(x+eight)-eight;
            y:=x+x*tenth;
            z:=log(x);
            zz:=log(y) - 3.7706015822504075e-4;
            zz:=zz - 21.0e0/512.0e0;
         end
         else
         begin
            z:=ln(x*x);
            zz:=ln(x);
            zz:=zz+zz;
         end;
         w:=one;
         if (z <> zero) then w:=(z-zz)/z;
         if z < 0 then z:=-abs(w)
            else z:=abs(w);
         if (z > 0) then k1:=k1+1;
         if (z < 0) then k3:=k3+1;
         w:=abs(w);
         if (w > r6) then
         begin
            r6:=w;
            x1:=x;
         end;
         r7:=r7+w*w;
         xl:=xl+del;
      end;
 
      k2:=n-k3-1;
      r7:=sqrt(r7/xn);
      writeln(report);   writeln(report);
      if (j = 1) then
         writeln(report,'test of ln(x) vs T.S. expansion of ln(1+y)')
      else if (j = 2) then
         writeln(report,'  test of ln(x) vs ln(17x/16) - ln(17/16)')
      else if (j = 3) then
         writeln(report,'          test of ln(x*x) vs 2*ln(x)')
      else if (j = 4) then
         writeln(report,'  test of log(x) vs log(11x/10)-log(11/10)');
      writeln(report,'==========================================');
      writeln(report);
      if (j = 1) then
         writeln(report,n:7,' random arguments were tested from the interval',
            ' (1-eps,1+eps), where eps = ',c)
      else
         writeln(report,n:7,' random arguments were tested from the interval',
            ' (',a,',',b,')');
      if (j <> 3) then
         writeln(report,'ln(x) was larger ',k1:6,' times, agreed ',
            k2:6,' times, and was smaller ',k3:6,' times.')
      else
         writeln(report,'log(x) was larger ',k1:6,' times, agreed ',
            k2:6,' times, and was smaller ',k3:6,' times.');
      writeln(report,'there are ',it:4,'base ',ibeta:4,' significant ',
         'digits in a floating-point number');
      w:=-999.0e0;
      if (r6 <> zero) then w:=ln(abs(r6))/albeta;
      writeln(report,'the maximum relative error of ',r6,
         ' = ',ibeta:4,' **',w);
      writeln(report,'    occurred for x =',x1);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base ',ibeta:4,
         ' significant digits is',w);
      w:=-999.0e0;
      if (r7 <> zero) then w:=ln(abs(r7))/albeta;
      writeln(report,'the root mean square relative error was',r7,
         ' = ',ibeta:4,' **',w);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base ',ibeta:4,
        ' significant digits is ',w);
      if (j = 1) then
      begin
         a:=sqrt(half);
         b:=15.0e0 / 16.0e0;
      end
      else if (j = 2) then
      begin
         a:=sqrt(tenth);
         b:=0.9e0;
      end
      else
      begin
         a:=16.0e0;
         b:=240.0e0;
      end;
   end;

(*
   special tests
                  *)
   writeln(report);   writeln(report);
   writeln(report,'special tests');
   writeln(report,'=============');
   writeln(report);
   writeln(report,'the identity ln(x) = -ln(1/x) will be tested.');
   writeln(report);
   writeln(report,'        x         f(x) + f(1/x)');

   for i:=1 to 5 do
   begin
      x:=random;
      x:=x+x+15.0e0;
      y:=one/x;
      z:=ln(x)+ln(y);
      writeln(report,'  ',x,'     ',z);
   end;

   writeln(report);   writeln(report);
   writeln(report,'test of special arguments');
   writeln(report,'=========================');
   writeln(report);
   x:=one;
   y:=ln(x);
   writeln(report,'ln(1.0) = ',y);
   x:=xmin;
   y:=ln(x);
   writeln(report,'ln(xmin) = ln(',x,') = ',y);
   x:=xmax;
   y:=ln(x);
   writeln(report,'ln(xmax) = ln(',x,') = ',y);

(*
   test of error returns
                         *)
   writeln(report);   writeln(report);
   writeln(report,'test of error returns');
   writeln(report,'=====================');
   writeln(report);
   x:=-2.0e0;
   begin
      y:=ln(x);
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(30);
   rslt:=math_ok;
   x:=zero;
   begin
      y:=ln(x);
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(31);
   rslt:=math_ok;
   
   writeln(report);   writeln(report);
   writeln(report,'this concludes the tests');



(*
This is the exp test.  It is directly taken from Software Manual for
the Elementary Functions by William J. Cody, Jr. and William Waite.
The following comment is the intro to the EXP test.

The tests are divided into four major parts.  First is a random 
argument test of exp(x) for arguments in the primary range, checking the
accuracy of the computation of exp(g) for the reduced argument g.
Second are similar tests using large positive and negative arguments to
check the accuracy of the argument reduction scheme.  Third is a cursory
check of the identity
    exp(x) * exp(-x) = 1
and several tests with special arguments, including some close to the 
theoretically largest and smallest acceptable arguments.  Finally, the
error returns are checked with arguments exceeding the theoretical 
limits.

The exponential function is extremely sensitive to small errors in
arguments greater than one in magnitude.  Recall from the general 
discussion earlier that the relative error in the exponential function
is roughly equal to the absolute error in the argument.  If there is a
rounding error in the machine number X, and if X is bounded in magnitude
between B**(n-1) and B**n, where B is the radix of the floating-point
system, then the last n B-ary digits in the machine representation of
exp(X) are probably incorrect.  Poor argument reduction in the
exponential function can introduce errors that behave as if they were
rounding errors in the argument.  It is therefore essential that
arguments used in the test procedure be error-free if we are to
distinguish between good and bad argument reduction.

The most obious identity to use in the random argument accuracy
tests,
    exp(x) = [exp(x/2)]**2,
is unsatisfactory because it tends to magnify errors.  Squaring is an
expansion mapping, carrying one machine interval into a second
containing roughly twice as many floating-point numbers.  This means
that about half of all machine numbers cannot be generated by squaring
other machine numbers.  If the machine representation of exp(x) happens
to be one of these excluded numbers, then the above identity cannot be
satisfied regardless of the quality of the exponential routine.  In
addition, because
    [x*(1+e)]**2 = x**2 * (1+2e)
to the first order terms in e, squaring exp(x/2) doubles the corresponding
relative error, further contaminating the error we want to measure.
 
A better possibility is an identity of the form
    exp(x-v) = exp(x) * exp(-v),
where v is an exact machine number and exp(-v) is known to more than 
machine precision.  An appropriate choice of v guarantees that the
arguments x and x-v used in the test will always lead either to the same
or to different values of N during argument reduction.  Further, the
extra error normally associated with multiplying by a rounded value of
exp(-v) can be minimized by representing exp(-v) as a sum of constants
and forming the product in two steps, i.e., by representing exp(-v) as
K1 + K2 and forming exp(x)*exp(-v) as exp(x)*K1 + exp(x)*K2.

The above identity with v = 1/16 forms the basis of thee accuracy 
tests for arguments in the primary interval (by slight adjustment of the
lower bound of the test interval, both x and x-v are guaranteed to lie
in the primary interval).  Thus we measure
    E = [exp(x-v) - exp(x)*exp(-v)]/exp8x-v).
Assume that relative errors of D and d are made in the evaluation of
exp(x-v9 and exp(x9, respectively, and that the argument x-v contains an
absolute error of e.  Then
    E = [exp(x-v)*(1+e)*(1+D) - exp(x)*exp(-v)*(1+d)] /
             [exp(x-v)*(1+e)*(1+D)]
Simplifying, and retaining only terms linear in d, D and e, we get
    E = D + e - d,
where e is likely to dominate when abs(x-v)>1, as explained abbove.
Although that situation does not happen for the primary interval, we
still prefer to use exact machine numbers for arguments.  We assume,
therefore, that X is already an exact machine number obtained from a
random number generator.  Then, because v > 0 in this case, X-v is also
exact if it is positive.  If it is negative, the sequence of Fortran
statements
    Y = X - V
    X = Y + V
where V is the machine representation of v, leads to a pair of exact
machine numbers differing by v; e - 0 in the above analysis, and
    E = D - d.
As with all argument purification schemes, the program must be modified
to force storage of intermediate results on machines that ordinarily
retain them in overlengh arithmetic registers.

Accuracy tests outside of the primary interval use v = 45/16 but
are otherwise analogous to the above procedure.  This choice of v
ensures that X and X-V lead to different values of N in the argument
reduction step.  The intervals used are carefully chosen to ensure that
arguments approach the largest and smallest that can b used in the
exponential, while at the same time protectiong against underflow in
performing the subtraction in E.

A good implementation of EXP should return an MRE (see Chapter 3)
only slightly greater than the precision of the machine for all argument 
ranges.  The RMS error should be at, or slightly less than, machine
precision.  When the reported MRE is significantly larger than the
machine precision, the distribution of the algebreaic signs of the errors
as reported on the output becomes important.  Our experience indicates 
that when the MRE is large, there is a tendency for the errors D and d
to be of the same sign and comparable in magnitude.  Because E = D - d,
the MRE value reported before conversion to units of base-B digits (see
Chapter 3) may be conservative by up to an order of magnitude when this
happens.

The test results reported in Table 6.1 are typical of good
implementations of EXP.  In all cases the MRE reports a loss of about 
one base-B digit (slightly larger for binary mahines), and the RMS
reports a loss of a small fraction of a digit.  Further, the errors are
comparable for a given program across all of the tests.  The results
quoted for our algorithm on the IBM equipment are for a fixed-point
implementation.  The MRE values for the corresponding floating-point
implementation are slightly larger (by at most .2 digits), but the RMS
values are almost identical to those reported here.  We believe the
slight increase in the MRE values is the result of wobbling precision
encountered during evaluation of the rational approximation.  This
problem is almost impossible to overcome in any reasonable way;
fortunately, it is not a serious problem.

The above tests will not detect a systematic error whereby the
calculated function value is too large by a constant factor.  The test
program checks for this situation by evaluating the identity
    exp(x) = [exp(x/2)]**2
for one argument.


program to test exp

data required
   none

subprograms required from this package
   
   machar - an environmental inquiry program providing 
            information on the floating-point arithmetic
            system.  Note that the call to machar can 
            be deleted provided the following four 
            parameters are assigned the values indicated

            ibeta - the radix of the floating-point system
            it    - the number of base-ibeta digits in the
                    significand of a floating-point number
            xmin  - the smallest non-vanishing floating-point
                    power of the radix
            xmax  - the largest finite floating-point no.

   ran(k) - a function subprogram returning random real
            numbers uniformly distributed over (0,1)


standard fortran suprograms required
   abs, aint, alog, amax1, exp, float, sqrt

latest revision - December 6, 1979

latest pascal revision - May 6, 1982

author - W. J. Cody
         Argonne National Laboratory

                                                            *)


   writeln(report);   writeln(report);
   writeln(report,'******    exp test    ******');
   writeln(report);   writeln(report);
   machar(ibeta,it,irnd,ngrd,machep,negep,iexp,minexp,
          maxexp,eps,epsneg,xmin,xmax);
   beta:=ibeta;
   albeta:=ln(beta);
   ait:=it;
   one:=1.0e0;
   two:=2.0e0;
   ten:=10.0e0;
   zero:=0.0e0;
   v:=0.0625e0;
   a:=two;
   b:=ln(a) * 0.5e0;
   a:=-b + v;
   d:=ln(0.9e0*xmax);
   n:=2000;
   xn:=n;
   x:=random(0);    (* initialize random *)
(*
   random argument accuracy tests
                                   *)
   for j:=1 to 3 do
   begin
      k1:=0;
      k3:=0;
      x1:=zero;
      r6:=zero;
      r7:=zero;
      del:=(b - a) / xn;
      xl:=a;

      for i:=1 to n do
      begin
         x:=del * random+xl;
(*
   purify arguments
                     *)
         y:=x-v;
         if (y < zero) then x:=y + v;
         z:=exp(x);
         zz:=exp(y);
         if (j = 1) then
            z:=z - z * 6.058693718652421388e-2
         else
         begin
            if (ibeta <> 10) then
               z:=z * 0.0625e0 - z * 2.4453321046920570389e-3
            else
               z:=z * 6.0e-2 + z * 5.466789530794296106e-5;
         end;
         w:=one;
         if (zz <> zero) then w:=(z-zz)/zz;
         if (w < 0) then k1:=k1+1
         else if (w > 0) then k3:=k3+1;
         w:=abs(w);
         if (w > r6) then
         begin
            r6:=w;
            x1:=x;
         end;
         r7:=r7 + w*w;
         xl:=xl + del;
      end;

      k2:=n - k3 - k1;
      r7:=sqrt(r7/xn);
      writeln(report);   writeln(report);
      writeln(report,'test of exp(x-',v,') vs exp(x)/exp(',v,')');
      writeln(report,'==================================================');
      writeln(report);
      writeln(report,n:7,' random arguments were tested from the interval (',
         a,',',b,')');
      writeln(report);
      writeln(report);
      writeln(report,'exp(x-v) was larger ',k1:6,'times, agreed ',k2:6,
         ' times, and was smaller ',k3:6,' times.');
      writeln(report);
      writeln(report);
      writeln(report,'there are ',it:4,' base ',ibeta:4,' significant ',
         'digits in a floating point number');
      writeln(report);
      writeln(report);
      w:=-999.0e0;
      if (r6 <> zero) then w:=ln(abs(r6))/albeta;
      writeln(report,'the maximum relative error of ',r6,' = ',
         ibeta:4,' **',w);
      writeln(report,'    occurred for x =',x1);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base ',ibeta:4,'significant ',
         'digits is ',w);
      writeln(report);
      writeln(report);
      w:=-999.0e0;
      if (r7 <> zero) then w:=ln(abs(r7))/albeta;
      writeln(report,'the root mean square relative error was',r7,
         ' = ',ibeta:4,' **',w);
      w:=max(ait+w,zero);
      writeln(report,'the estimated loss of base',ibeta:4,' significant ',
         'digits is',w);
      writeln(report);
      writeln(report);
      if (j <> 2) then
      begin
         v:=45.0e0 /16.0e0;
         a:=-ten * b;
         b:=4.0e0 * xmin * beta ** it;
         b:=ln(b);
      end
      else
      begin
         a:=-two * a;
         b:=ten * a;
         if (b < d) then b:=d;
      end;
   end;

(*
   special tests
                  *)
   writeln(report);   writeln(report);
   writeln(report,'special tests');
   writeln(report,'=============');
   writeln(report);
   writeln(report,'the identity exp(x)*exp(-x) = 1.0 will be tested');
   writeln(report);
   writeln(report);
   writeln(report,'        x         f(x)*f(-x) - 1');
   writeln(report);
   
   for i:=1 to 5 do
   begin
      x:=random * beta;
      y:=-x;
      z:=exp(x) * exp(y) - one;
      writeln(report,'  ',x,'     ',z);
   end;

   writeln(report);
   writeln(report);
   writeln(report,'test of special arguments');
   writeln(report,'=========================');
   writeln(report);
   x:=zero;
   y:=exp(x) - one;
   writeln(report,'exp(0.0) - 1.0e0 = ',y);
   writeln(report);
   x:=trunc(ln(xmin));
   y:=exp(x);
   writeln(report,'exp(',x,') = ',y);
   writeln(report);
   x:=trunc(ln(xmax));
   y:=exp(x);
   writeln(report,'exp(',x,') = ',y);
   writeln(report);
   x:=x / two;
   v:=x / two;
   y:=exp(x);
   z:=exp(v);
   z:=z*z;
   writeln(report,'if exp(',x,') = ',y,' is not about exp(',
      v,')**2 =',z,' there is an arg red error');
(*
   test of error returns
                          *)
   writeln(report);   writeln(report);
   writeln(report,'test of error returns');
   writeln(report,'=====================');
   writeln(report);
   x:=-one/sqrt(xmin);
   begin
      y:=exp(x);
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(32);
   rslt:=math_ok;
   x:=-x;
   begin
      y:=exp(x);
      exception
         math_error:   rslt:=mathstatus;
   end;
   if (rslt = math_ok) then error(33);
   rslt:=math_ok;

   writeln(report);   writeln(report);
   writeln(report,'this concludes the tests');

   writeln(tty,'end of mattst');

end.
    ~ \@