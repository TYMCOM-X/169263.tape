; This COM file compiles PMF for ADP
do pascal
/search:(pasdev2)
/enable(ADP)
/optimize
pmf
pmfcmd
pmfinp
pmfput
pmfdef
pmfexp
pmfscn
pmferr

    