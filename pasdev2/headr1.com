do qed
l header.lis
set wild on
f not"/"s@@/P@
n
f@/F*@or@/f*@s//.hdr;1d;$d;1,$s::C     :;w%y/;s::l :
n
f@/M*@or@/m*@s//.hdr;1d;$d;1,$s::;    :;w%y/;s::l :
n
f@/*@s//.hdr;1d;$d;1,$s::     :;1s"  "(*";$s@@ *)@;w%y/;s::l :
n
1,$spl/%/del
1i
do qed
.
$a
q
del headr1.tmp,headr2.tmp,headr3.tmp
.
w headr3.tmp
y
q
r(upl)com;headr3.tmp

