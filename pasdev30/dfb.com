rename dfb.lst as dfb.lpt
print cdc:=dfb.lpt/fullch/size 102
PASCAL10
delete dfb.lpt
fil *.lst/size
rename *.lst as *.lpt
print cdc:=*.lpt/fullch/size 102
pascal10
delete *.lpt
    