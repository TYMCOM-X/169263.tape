del ???bld.cmd
do build
c10;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
m68ccg.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 10:  MC68000 Code Generator
del 000bld.cmd
r(upl)com;(pasdev1)pa11xr
  
   