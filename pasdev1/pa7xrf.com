del ???bld.cmd
do build
c7;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
p10ini.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 7:  PDP-10 Initialization
del 000bld.cmd
r(upl)com;(pasdev1)pa8xrf
  
 