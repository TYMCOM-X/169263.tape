del ???bld.cmd
do build
c2;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
p10shp.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 2:  PDP-10 Shaper and Optimizer
del 000bld.cmd
r (upl)com;(pasdev1)pa3xrf

 