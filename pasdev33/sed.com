; SMK's QED .COM file for schedule editing
do qed
l smk.pmf
set mark :#com2(:
f :#com: li
n
  