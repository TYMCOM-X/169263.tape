do (pasdev10)pascal
:target m68
/sea ((pasdev16),(pasdev2))
qedenv
.p
:env qedenv
/stat/map/quick/dump(map,peep)
qedcl
.p
qld
.p
qjoin
.p
qsplit
.p
qedtyp
.p
qsubst
.p
qederr
.p
qmark
.p
qprint
.p
qedln
.p
qread
.p
qspred
.p
qspat
.p
/exit
 