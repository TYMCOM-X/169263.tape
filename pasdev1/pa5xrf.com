del ???bld.cmd
do build
c5;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
p10ccg.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 5:  PDP-10 Checkout Code Generator
del 000bld.cmd
r(upl)com;(pasdev1)pa6xrf

    