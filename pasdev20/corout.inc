(* COROUT.INC Definitions for JMB's coroutine implementation *)

type
  ENVIRONMENT = 0..777777B;
  COROUTINE = procedure ();
  STACKSIZE = 0..377777B;

external function CREATE (P: COROUTINE; S: STACKSIZE): ENVIRONMENT;
external procedure CALL (E: ENVIRONMENT);
external procedure DETACH;
external procedure RESUME (E: ENVIRONMENT);

(* End of COROUT.INC *)
   