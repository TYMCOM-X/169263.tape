$INCLUDE pasopt.typ

external var
    smod: svector;
    suse: svector;

external procedure do_summary_analysis;
external procedure effects ( node: tuple );
external procedure optimize;
external procedure low_reduce;
   