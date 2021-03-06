(*  Standard Procedures and Functions  *)

type
    std_pr_fun =

     (	propen,		prreset,	prrewrite,	prupdate,
	prget,		prput,		prpage,		prclear,
	prbreak,	prempty,	prclose,	prscratch,
	prread8,	prwrite8,	prseek,	
	prnew,		prallocate,	prdispose,	prassert,
	prsignal,	prmask,		prunmask,

	fnabs,		fnsqr,		fnsqrt,
	fnln,		fnlog,		fnexp,
	fnsin,		fnarcsin,	fnsinh,
	fncos,		fnarccos,	fncosh,
	fntan,		fnarctan,	fntanh,		fncotan,
	fnrandom,	fnmin,		fnmax,		fnodd,
	fnround,	fntrunc,	fnord,		fnchr,
	fnsucc,		fnpred,		fnminimum,	fnmaximum,
	fneoln,		fneopage,	fneof,		fncursor,
	fniostatus,	fnextstatus,
	fnmathstatus,	fnprogramstatus,
	fnspecialstatus,		fnexiostatus,
	fndate,		fntime,		fnruntime,
	fnmasked,	fnpending,
	fnlength,	fnlowercase,	fnuppercase,
	fnsubstr,	fnindex,	fnverify,	fnsearch,
	fnlowerbound,	fnupperbound,	fndimension,
	fnsize,		fnextent,	fnaddress,
	fnfieldoffset,	fnfieldwidth,	fnxfer		);

	(*  Note:  XFER is not a user function.  It is the function code
	    representing a type name used as a transfer function.  *)

    std_proc = propen .. prunmask;
    std_func = fnabs .. fnxfer;
  