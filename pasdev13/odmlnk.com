do decmac
mmovlm=mmovlm

copy mmovlm.rel+(pasdev21)infpac.rel to mmovlm.rel
do link
@odms
do link
@mdlpro
r filex
mdlpro.exe=mdlpro.shr


run odms
.p
compile odms
verify mdlpro o=mdlpro
exit
