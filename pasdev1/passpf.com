do trnslt
passpf.tmp=passpf.typ/a-z=A-Z

do qed
l passpf.tmp
1,/(	/-1c
type stdpfarray = array [std_pr_fun] of string [20];

const
  std_pf_id: stdpfarray =
.
/);/+1,$ d
1,$ s /	FN/	'/a
1,$ s /	PR/	'/a
1,$ s /,/',/a
$ s /XFER/'/
w passpf.nam

q
 