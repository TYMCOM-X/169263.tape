$options nosource

(* This $include file defines the data items which interface the Pascal
   Debugger to the Pascal Run-time environment.  They are all global
   (Pascal External Var) cells defined in and set by the run-time system,
   but accessible to the Debugger.  The debugger itself is a Pascal
   procedure DEBUG$ which gets all its arguments through these storage cells *)

type

  reason_code = (    (*describes reason debug called*)
    badreason, (*Debugger called without run-time mediating. Either is bug
		 or user has called it at the wrong time via DDT.*)
    init,  (*Call at beginning of execution.*)
    trap,  (*Debugger entered (via DDT) after trap, e.g., illegal memory
	     reference. Continuing execution after such errors impossible.*)
    rterr, (*Debugger entered (via DDT) after error detected and diagnosed
	     by run-time. Continuing execution afterwards impossible.*)
    step,  (*Step count set by debugger on last return to program exhausted.*)
    brkpt, (*Debugger breakpoint has been encountered.*)
    intrp);(*Debugger entered (via DDT) after break (control-c) hit.
	     Execution may be continued afterwards.*)

const maxbrkpt = 9;    (*Maximum number of breakpoints which may be set.*)

external var (*These cells are set by run-time and shared with debugger*)

  c$tmt: stmtblkptr;   (*Pointer to last statement executed.  On breakpoint
		         and step entries, is current statement.*)
  ba$i$: packed record (*Describes most recently called routine.*)
    mainflag: 0..777777b; (*zero if main routine, non-zero otherwise*)
    basis: stkframeptr    (*pointer to current stack frame*)
  end;
  rea$on: reason_code; (*Parameter indicating reason for debugger call.
			 Should always be set to bad_reason when debugger exits.*)
  step$: -1..100000;   (*Number of statements to step.  Should always be set
			 to -1 when debugger exits, unless exit is due to
			 a step command.*)
  p$kip: boolean;      (*If true, then routine call counted as single step, i.e.,
			 statements in called routines not counted.*)
  br$kip: -1..100000;  (*Number of breakpoints to bypass before taking one.
			 Should always be set to -1 when debugger exits, unless
			 exit is due to a proceed command.*)
  break$: 0..maxbrkpt; (*On breakpoint call, the index of breakpoint (in brk$table)
			 encountered.*)

  (* Breakpoint addresses (addresses of statement blocks) are stored in
     following table. The table should be initialized to NIL on the init call.
     When a breakpoint is set, the table should be searched for the first NIL
     entry, and its stmtblkptr stored there.  When a breakpoint is cleared,
     the corresponding entry should be NILed. The  STMT. run-time routine
     always searches the entire table for a match.*)

  brk$table: array[0..maxbrkpt] of stmtblkptr;

(*Note that all symbols shared with the debugger contain at least one $
  to avoid conflicts with program symbols.*)

$options source
   