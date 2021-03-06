$TITLE prtstk -- stack manipulation routines
$LENGTH 43
$INCLUDE prt.typ

const
    stk_size = 100;
    ind_quantum = 2;

var
    stack: array [1..stk_size] of packed record
	     flag: stack_flags;
	     ind: text_index
	   end;
    stack_ptr: 0 .. stk_size;

public var
    indentation: text_index;
    stack_empty: boolean;


public procedure stkinit;
begin
  indentation := 0;
  stack_ptr := 0;
  stack_empty := true;
end;
$PAGE push & mark
(*  MARK will push a specified flag on top of the stack.  *)

public procedure mark ( f: stack_flags );
begin
  if stack_ptr = stk_size then
    error ('STACK OVERFLOW');
  stack_ptr := stack_ptr + 1;
  with stack [stack_ptr] do begin
    flag := f;
    ind := indentation;
  end;
  stack_empty := false;
end (* mark *);



(*  PUSH is the same as MARK, except that it also increments the
    current indentation level.  *)

public procedure push ( f: stack_flags );
begin
  mark (f);
  indentation := indentation + ind_quantum;
end (* push *);
$PAGE pop_while & pop_until
(*  ^p^o^p_^w^h^i^l^e will pop the stack as long as a specified flag is at the
    top of the stack.  *)

public procedure pop_while ( f: flag_set );
begin
  while (stack_ptr <> 0) andif (stack[stack_ptr].flag in f) do begin
    indentation := stack[stack_ptr].ind;
    stack_ptr := stack_ptr - 1;
  end;
  stack_empty := (stack_ptr = 0);
end (* pop_while *);



(*  ^p^o^p_^u^n^t^i^l will pop the stack until a specified symbol is popped
    off the top of the stack.  *)

public procedure pop_until ( f: flag_set );
begin
  while (stack_ptr <> 0) andif not (stack[stack_ptr].flag in f) do
    stack_ptr := stack_ptr - 1;
  if stack_ptr = 0 then
    error ('STACK UNDERFLOW: BAD PROGRAM SYNTAX');
  indentation := stack[stack_ptr].ind;
  stack_ptr := stack_ptr - 1;
  stack_empty := (stack_ptr = 0);
end (* pop_until *).
 