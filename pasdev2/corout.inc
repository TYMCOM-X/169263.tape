type
  environment = ^ environment_record;

external function create (p: procedure (); stacksize: integer): environment;
external procedure call (e: environment);
external procedure detach;
external procedure resume (e: environment);
    