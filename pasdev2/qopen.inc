TYPE
  QIOMODE = (QINPUT_MODE, QOUTPUT_MODE);
  QIOOPTIONS = (QIO_APPEND, QIO_CONFIRM, QIO_ASCII);
  QIOOPTION_SET = SET OF QIOOPTIONS;

EXTERNAL PROCEDURE QOPENFILE ( VAR F: TEXT;	(* file to open *)
			     FID: FILE_NAME;	(* name of file *)
			     EXT: STRING[3];	(* default extension *)
			     MODE: QIOMODE;	(* I/O mode *)
			     OPTS: QIOOPTION_SET;   (* options *)
			     VAR ERR: QERRCODE );   (* error code *)
  