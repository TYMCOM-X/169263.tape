program rftest;

$include rfdata.inc

external procedure rf_data_init;
external procedure page_fmt_init;
external procedure justify (var command_words; var line_type);
external procedure wrline (line_type);
external procedure rfmain;

begin
 rewrite (tty);
 open (input, 'test.doc');
 rewrite (output, 'tty:');
 rf_data_init;
 page_fmt_init;
 rfmain;
end.
 