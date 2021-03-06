(* QTERM consolidates many tty I/O functions, so that other users of
   the QEDLIB subroutines may provide their own controllers for whatever
   hardware they want to use for the tty.

   Module begun 29 July 1982 by WNH.  *)

external procedure ttput;
external procedure ttwrite (string [*]);
external procedure ttwtln;
external procedure tticlr;
external procedure ttoclr;
external procedure ttbrk;
external procedure ttget;
external procedure ttread (var line: string [*]);
external procedure ttrdln;
external procedure ttpage;
external procedure ttisetbuf (letter : char);
external procedure ttosetbuf (letter : char);

external function ttieof : boolean;
external function ttoeof : boolean;
external function ttieol : boolean;
external function ttirefbuf : char;
external function ttorefbuf : char;
external function ttist : io_status;
external function ttost : io_status;
 