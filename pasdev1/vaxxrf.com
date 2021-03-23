del ???bld.cmd
do build
c0,1,3,6,8,9;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
vax.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Composite xref of all Vax passes
delete 000bld.cmd
