$INCLUDE fio.typ[31024,320156]
external procedure fio_open (var file_block; packed array [1..*] of char);
external procedure fio_attach (var file_block; text);
external procedure fio_close (var file_block);
external procedure fio_page (var file_block);
external procedure fio_skip (var file_block);
external procedure fio_nskip ( var file_block; integer; integer );
external procedure fio_tab (var file_block; fio_width);
external procedure fio_write (var file_block; packed array [1..*] of char);
external procedure fio_nop (var file_block);
external procedure fio_eject (var file_block);
external procedure fio_reopen (var file_block);
external procedure fio_line (var file_block; packed array [1..*] of char);
external procedure fio_space (var file_block; fio_width);
