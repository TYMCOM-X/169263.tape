del ???bld.cmd
do build
c0;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
pascal.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 0:  Startup Program
delete 000bld.cmd
r (upl)com;(pasdev1)pa1xrf

del ???bld.cmd
do build
c1;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
pasanl.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 1:  Analysis Pass
del 000bld.cmd
r (upl)com;(pasdev1)pa2xrf

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

del ???bld.cmd
do build
c4;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
p10ocg.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 4:  PDP-10 Optimizing Code Generator
del 000bld.cmd
r (upl)com;(pasdev1)pa5xrf

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
  
del ???bld.cmd
do build
c8;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
vaxccg.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 8:  VAX Code Generator
del 000bld.cmd
r(upl)com;(pasdev1)pa9xrf
  
del ???bld.cmd
do build
c9;qf
rename ???bld.cmd as 000bld.cmd
do qed
l 000bld.cmd
1,2d
w
y
q
do xref
vaxini.xrf=@000bld.cmd,@(pasdev1)librry.cmd
Pascal Version 2, Pass 9:  VAX Initialization
del 000bld.cmd
r(upl)com;(pasdev1)pa10xr

 