del ???bld.cmd
do build
c0,1,3,5,6,7;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
q10.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Composite xref of all PDP10 Checkout passes
delete 000bld.cmd
    