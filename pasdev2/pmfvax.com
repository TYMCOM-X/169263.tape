; This COM file compiles PMF for VAX
do pascal
:tar VAX
/search:(pasdev2)
/enable(VAX)
pmf
pmfcmd
pmfinp
pmfput
pmfdef
pmfexp
pmfscn
pmferr

