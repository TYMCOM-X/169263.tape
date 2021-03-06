$INCLUDE pasxrf.typ

external procedure xrf_init;
external procedure xrf_freeze;
external procedure xrf_unfreeze ( xrf_class );
external procedure xrf_scratch;
external procedure xrf_block ( blk; source_id );
external procedure xrf_write ( xrf_class );
external procedure xrf_use ( sym; source_id; xrf_class );
external procedure xrf_with ( expr; sym; source_id; xrf_class );
external procedure xrf_close;
   