$PAGE	QED -- General Environment Module
(* QEDENV.PAS - created 10/07/81 by djm *)
(*            - modified 04/29/82 by djm to delete lookup.typ, add
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
$SYSTEM qedtyp.typ
$SYSTEM filutl
$SYSTEM query
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
$END
$IF VAX
$SYSTEM imgnam
$END
END.
   