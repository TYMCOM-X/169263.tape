:EXAMP5.LST
do link
@reader
ru odms
use scribe
verify reader overlay=reader.exe
q
ru odms
compile scribe
build reader using reader
build main using scribe,doscri,rdlib[,320156]/s
q
do link
@reader
do link
@scribe
 