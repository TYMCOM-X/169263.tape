del ???bld.cmd
do build
c6;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
pascmd.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 6:  Command Line Processor
del 000bld.cmd
r(upl)com;(pasdev1)pa7xrf

  