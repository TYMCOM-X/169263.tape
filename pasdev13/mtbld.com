ru (pasdev13)odms
.p
compile mtest
build main chan=1 using mt01a,rdlib[,320156]/s&
paslib[,320155]/s,sys:lnkddt,sys:jobdat,forlib[,320155]/s
build mt02 using mt02a,paslib[,320155]/s,forlib[,320155]/s
build mt03 using mt03a,paslib[,320155]/s,forlib[,320155]/s
build mtinit using mtinit,paslib[,320155]/s,forlib[,320155]/s
build mt11 using mt11a,mt11b,paslib[,320155]/s,forlib[,320155]/s
q
r (upl)com;mtbld2.com

  