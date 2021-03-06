$PAGE	QED -- General Environment Module
(* QEDENV.PAS - modified 9/17/81 by djm to change QUERY to QQUERY *)
(*            - modified 9/25/81 by djm to add conditional compile code
                around infpac.inc and imgnam.inc *)
(*            - modified 10/01/81 by djm to put wio.inc into a 
                conditional compile bracket *)
(*            - modified 04/30/82 by djm to delete lookup.typ, add
                cmdutl.inc, add qedtyp.typ, and add qsubst.typ. 
		These are to allow calls to cmd_lookup. *)

ENVMODULE QEDENV;
$SYSTEM cmdutl
$SYSTEM qerr.typ
$SYSTEM qstr.typ
$SYSTEM qspat.typ
$SYSTEM qspred.typ
$SYSTEM cmdutl.typ
$SYSTEM qedln.typ
$SYSTEM qline.typ
$SYSTEM qld.typ
$SYSTEM qed.typ
$SYSTEM qsplit.typ
$SYSTEM qsubst.typ
$SYSTEM wio.typ
$SYSTEM qedtyp.typ
$SYSTEM filutl
$SYSTEM qquery
$SYSTEM qspat
$SYSTEM qspred
$SYSTEM qederr
$SYSTEM qld
$SYSTEM qread
$SYSTEM qedln
$SYSTEM qmark
$SYSTEM qprint
$SYSTEM qsubst
$SYSTEM qjoin
$SYSTEM qsplit
$SYSTEM qopen
$SYSTEM qed
$IF P10
$SYSTEM infpac
$SYSTEM wio
$END
$IF VAX
$SYSTEM imgnam
$END
$SYSTEM qlabel
END.
    