type
  rwordtype = packed array [1..13] of char;
  rwtype  = array [1..60] of rwordtype;
  rsytype = array [1..60] of symbols;
  roptype = array [1..60] of operators;
  frwtype = array [1..14] of 1..61;
  ssytype = array [' '..'~'] of symbols;
  soptype = array [' '..'~'] of operators;

const rw: rwtype :=
     (  'IF',            'DO',            'OF',            'TO',            
	'IN',            'OR',            'END',           'FOR',           
	'VAR',           'DIV',           'MOD',           'SET',           
	'AND',           'NOT',           'THEN',          'ELSE',          
	'WITH',          'GOTO',          'LOOP',          'CASE',          
	'TYPE',          'FILE',          'EXIT',          'PREC',          
	'ORIF',          'STOP',          'READ',          'ANDIF',         
	'BEGIN',         'UNTIL',         'WHILE',         'ARRAY',         
	'CONST',         'LABEL',         'WRITE',         'RECORD',        
	'DOWNTO',        'PACKED',        'OTHERS',        'REPEAT',        
	'PUBLIC',        'STATIC',        'STRING',        'RETURN',        
	'MODULE',        'READLN',        'READRN',        'FORWARD',       
	'PROGRAM',       'OPTIONS',       'WRITELN',       'WRITERN',       
	'FUNCTION',      'EXTERNAL',      'PROCEDURE',     'EXCEPTION',     
	'GETSTRING',     'PUTSTRING',     'ENVMODULE',     'ALLCONDITIONS'  );

const rsy: rsytype :=
     (  ifsy,            dosy,            ofsy,            tosy,            
	relop,           addop,           endsy,           forsy,           
	varsy,           mulop,           mulop,           setsy,           
	mulop,           notsy,           thensy,          elsesy,          
	withsy,          gotosy,          loopsy,          casesy,          
	typesy,          filesy,          exitsy,          precsy,          
	addop,           stopsy,          iosy,            mulop,           
	beginsy,         untilsy,         whilesy,         arraysy,         
	constsy,         labelsy,         iosy,            recordsy,        
	downtosy,        packedsy,        otherssy,        repeatsy,        
	publicsy,        staticsy,        stringsy,        returnsy,        
	modulesy,        iosy,            iosy,            forwardsy,       
	programsy,       optionssy,       iosy,            iosy,            
	functionsy,      externalsy,      proceduresy,     exceptionsy,     
	iosy,            iosy,            envmodsy,        allcondsy        );

const rop: roptype :=
     (  noop,            noop,            noop,            noop,            
	inop,            orop,            noop,            noop,            
	noop,            idiv,            imod,            noop,            
	andop,           noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	orifop,          noop,            readsy,          andifop,         
	noop,            noop,            noop,            noop,            
	noop,            noop,            writesy,         noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            readlnsy,        readrnsy,        noop,            
	noop,            noop,            writelnsy,       writernsy,       
	noop,            noop,            noop,            noop,            
	getstrsy,        putstrsy,        noop,            noop             );

const frw: frwtype :=
     (  1,               1,               7,               15,              
	28,              36,              48,              53,              
	55,              60,              60,              60,              
	60,              61               );

const ssy: ssytype :=
     (  badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	lparent,         rparent,         mulop,           addop,           
	comma,           addop,           period,          mulop,           
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       colon,           semicolon,       
	relop,           relop,           relop,           badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       lbracket,        
	badsymbol,       rbracket,        arrow,           badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol,       badsymbol,       
	badsymbol,       badsymbol,       badsymbol        );

const sop: soptype :=
     (  noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            mul,             plus,            
	noop,            minus,           noop,            rdiv,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	ltop,            eqop,            gtop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop,            noop,            
	noop,            noop,            noop             );
    