ru (pasdev13)odms
.b mmdbpr@chainonend/25 'page_size:=27;.p'
.p
use mtest
upd mtinit
upd mt11
upd mt11 ver=1 o=mtalt1
upd mt12
upd mt13
upd mt21
upd mt22
upd mt23
pack
q
 