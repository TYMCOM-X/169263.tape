del ???bld.cmd
do build
c3;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
paslst.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 3: Listing Pass
del 000bld.cmd
r (upl)com;(pasdev1)pa4xrf

  