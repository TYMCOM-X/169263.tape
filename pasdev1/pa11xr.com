del ???bld.cmd
do build
c11;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
m68ini.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 11:  MC68000 Initialization
del 000bld.cmd
    