
external procedure stkinit;
external procedure push ( f: stack_flags );
external procedure mark ( f: stack_flags );
external procedure pop_while ( f: flag_set );
external procedure pop_until ( f: flag_set );

external var
    indentation: text_index;
    stack_empty: boolean;
  