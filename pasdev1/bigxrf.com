del ???bld.cmd
do build
c*;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
big.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Composite cross-reference of all passes
delete 000bld.cmd
   