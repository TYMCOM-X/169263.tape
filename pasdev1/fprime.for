	SUBROUTINE PRIME(IP,M)
	INTEGER IP(1),M,I,IJ,INC,IQI,J,JJ,JQI,K,N
	INTEGER IQ(0:100),JQ(0:100)
	IP(1)=2
	IP(2)=3
	IP(3)=5
	J=2
	K=3
	N=5
	JJ=25
	IQI=25
	JQI=-10
	IQ(2)=49
	JQ(2)=-14
	INC=4
	GOTO 3
1	CONTINUE
	IF(JQI.LE.0) GOTO 10
	  IQI=IQI+JQI+JQI
	GOTO 20
10	CONTINUE
	  IQI=IQI-JQI
20	CONTINUE
	I=1
	IJ=2
30	IF(IJ.GE.J) GOTO 40
	  IF(IQ(IJ).GT.IQ(IJ+1)) IJ=IJ+1
	  IF(IQ(IJ).GE.IQI) GOTO 2
	  IQ(I)=IQ(IJ)
	  JQ(I)=JQ(IJ)
	  I=IJ
	  IJ=I+I
	GOTO 30
40	CONTINUE
	IF(IQI.LT.JJ) GOTO 2
	JJ=IQ(J)
	J=J+1
	IJ=IP(J+2)
	IQ(J)=IJ*IJ
	JQ(J)=IJ+IJ
	IF((IJ-(IJ/3)*3).EQ.1) JQ(J)=-JQ(J)
2	CONTINUE
	IQ(I)=IQI
	IQI=IQ(1)
	JQ(I)=JQI
	JQI=-JQ(1)
	IF(N.EQ.IQI) GOTO 1
3	CONTINUE
	INC=6-INC
	N=N+INC
	IF(N.EQ.IQI) GOTO 1
	K=K+1
	IP(K)=N
	IF(K.NE.M) GOTO 3
	RETURN
	END
   