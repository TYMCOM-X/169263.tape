(*********   S C N L E X - - SCANNR Lexical Scanner   *********)

external var
    insymbol: token_type;
    invalue: sym_value;

external procedure lexinit;
external procedure getsymbol;
external function tokcolumn: number;
 