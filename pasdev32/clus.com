del clustr.rel
del build.rel
del print.rel
del cost.rel
del optim.rel
del dispos.rel
del clustr.shr
do pascal
clustr,clustr=clustr/stat/deb/enable:trace
build,build=build/stat/deb/enable:trace
print,print=print/stat/deb/enable:trace
cost,cost=cost/stat/deb/enable:trace
optim,optim=optim/stat/deb/enable:trace
dispos,dispos=dispos/stat/deb/enable:trace

do link
clustr
build
print
cost
optim
dispos
/g
ssave clustr
 