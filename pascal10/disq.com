l disq.log
f not ## d
n
1,$s"FIL(""
1,$s")/TOT""
1,$s"TOTAL PAGES ""
1,$s"FILES""
f :: . mov a .1;.-1,.join/ /
n
w disq.lst

q
 