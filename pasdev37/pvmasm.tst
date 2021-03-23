      .TITLE wozmo
      .psect P0,5,EXE,RD,WRT
lab1: ftype1     next+12 ;first comment
lab2: ltype2     7,0 ;kjoiuoijiui
lab3: ftype3     0,absym2+5
lab4: ftype4     1,15*5(0)
lab4a: ltype4    2,absym3(12)[0]
lab5: ltype5     3,lab1+other1
lab5a: ftype5    4,p1\orig[15]
lab6: ftype6     5,100,absym3-absym2(11) ;commentkkfijijdmkweijskmsdkm
lab7: ftype7     6,101(0),<<12+76-8>*2>/<8*5>(7) ; = 4
lab7a: ltype7    9,absym1(15)[1],absym2(2);plus some comment
next: ftype1     skip
;blank line
block: .BLKB     absym1*2
byte:  .BYTE     block - 10;
word:  .WORD     word
long:  .LONGWORD lab1 * absym2
chars: .ASCII '''''you don''t say''''' ;comment
      absym1 = <17*2> + <12*1> - <0 + 0> + 1 - 1 * 1 ; = 46
      absym2 = 4 + absym1 + -48 ;check unary minus
      absym3 = lab5a - lab5 ; special abs symbol
absym4=197;pack on margin
.global lab1,lab2,absym1,lab3,absym3
  .external other1,other2,other3
skip:   ftype8  1,2,3,4
     .PSECT p1,4,noexe,nord,nowrt
   orig = next2 - next1
  next1: ltype8 orig,99;iejd eiei
next2: ftype1 next1 
     .PSECT P0,5,exe,rd,wrt
absym5 = 198 ;to be obnoxious
 ltype2 1,1 ;no label
more: ltype2 2,2
 .END next
