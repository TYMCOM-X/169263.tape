$page cl1
$title cl/1 main routine
$length 45

(*
 *  cl/1 main routine.
 *)

program cl1;

$include cl1.inc

const
   prompt = '>';    (* user prompt *)

public var
   token: token_rec;   (* current token *)

var
   err_code: err_types;   (* error code from parser routines *)
   exit_cmd: boolean := false; (* flag - 'exit cl/1' *)

external function scanner: token_rec; (*  scanner *)
external procedure assign_stmt(var err_types); (* parses assignment stmts *)
external procedure expr_stmt(var err_types); (* parses expression stmts *)

$page main
(*
 *  begin cl/1 main routine code.
 *)

begin
   open(tty);
   rewrite(tty);
   err_code := success;

   repeat   (* process one statement each iteration *)
      write(tty,prompt);   (* print user prompt *)
      break(tty);

      if (err_code <> success) andif (token.t_type <> eol) then
         repeat   (* if error return, scan to end of line *)
            token := scanner
         until token.t_type = eol;
      err_code := success;
      token := scanner;   (* get first token of line *)

      case token.t_type of

         id:   (* assignment statement *)
            assign_stmt(err_code);
         assign_op:   (* expr to evaluate and print *)
            expr_stmt(err_code);
         eol:   (* exit command *)
            exit_cmd := true;
         error:   (* lexical error *)
            err_code := lex_err;
         others:   (* else syntax error *)
            err_code := syn_err
      
      end (* case *);
      
      if err_code = syn_err then
      begin writeln(tty, ' syntax error');
         break(tty);
      end

   until exit_cmd

end.
    