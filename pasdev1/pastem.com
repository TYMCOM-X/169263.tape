do qed
l pasdat.pas
1,$ s /public/external/
b/PASCAL.INC/
2,$ w pascal.inc
y
b/PASIST.INC/
2,$ w pasist.inc
y
b/PASFIL.INC/
2,$ w pasfil.inc
y
b/PASLOG.INC/
2,$ w paslog.inc
y
b/PTMCON.INC/
2,$ w ptmcon.inc
y
q
y
  