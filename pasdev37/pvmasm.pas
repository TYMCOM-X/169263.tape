program pvmasm options debug;

$system (pasdev14)objfil.typ
(* general types *)
const
exprends : set of char :=  [';',  ',',  '(',  '['];
delims : set of char := [' ',  ',',  ':',  ';',  '''',  '=',  '[',  ']',  '(',  ')'];

undefined : integer := -99999;
type
str15 = string[15];
str32 = string[32];
str8 = string[8];
str120 = string[120];
opfield = packed array [1..*] of char;

opnd_array = array [1..9] of ^opfield;
var
linenum : integer := 0;

(* op code table types *)
type
opco_rec = record
   mnemonic : str15;  (* op code mnemonic *)
   opco : integer     (* hex op code *)
   end;
const
max_num_opnds = 28;

opco_table : array [1..max_num_opnds] of opco_rec :=
(
    ('=',0),
   ('.TITLE',0),
    ('.PSECT',0),
    ('.GLOBAL',0),
     ('.EXTERNAL',0),
    ('.END',0),
    ('.BLKB',0),
    ('.BYTE',0),
    ('.WORD',0),
    ('.LONGWORD',0),
    ('.ASCII',0),
   ('FTYPE1',1),
    ('LTYPE1',02),
    ('FTYPE2',03),
    ('LTYPE2',04),
    ('FTYPE3',05),
   ('LTYPE3',06),
    ('FTYPE4',07),
    ('LTYPE4',08),
    ('FTYPE5',09),
    ('LTYPE5',10),
    ('FTYPE6',11),
    ('LTYPE6',12),
    ('FTYPE7',13),
    ('LTYPE7',14),
    ('FTYPE8',15),
    ('LTYPE8',16),
    ('',17)
);

(*   symbol table types  *)
type
   expression_type = (abso,relo,extr,psect_orig) ;
   relocation_type = (abs,rel,extrnl);
   sym_type = (glo,lcl,ext);

   stack = record
      entry : str15;
      next  :  ^stack
      end;
 
   eval_stack = record
      entry : integer;
      next : ^eval_stack
   end;

   symtab_rec = record
      sname : str15;      (* symbol name *)
      val : integer;      (* symbol value *)
      absrelext : relocation_type;   (* absolute or relative *)
      glolocext : sym_type;   (* global, local, or external *)
      psect_num : integer;    (* number of psect associated with symbol *)
      nxtptr : ^symtab_rec        (* pointer to next symtab rec on list *)
      end;

var
   symtab_head : ^symtab_rec := nil;  (* pointer to first symtab rec on list *)
   symtab_tail : ^symtab_rec := nil;  (* pointer to last symtab rec on list *)


(* psect table types  *)
type
   psect_rec = record
      pname : str15;               (* psect name *)
      pnum : integer;              (* psect number *)
      loc : integer;               (* current value of location cntr in psect *)
      align : integer;        (* psect alignment *)
      params : set of psect_attributes;   (* params on psect directive *)
      nxtptr : ^psect_rec        (* pointer to next psect rec on list *)
      end;

var
   psect_head : ^psect_rec := nil; (* pointer to first psect rec on list *)
   psect_tail : ^psect_rec := nil; (* pointer to last psect rec on list *)
   nxt_psect_num : integer := 0; (* psects are numbered sequentially from 0 up. This is next num to use *)

const
psect_params : array[1..6] of str15 :=
('EXE', 'NOEXE', 'RD', 'NORD', 'WRT', 'NOWRT');

$PAGE dump_sym_psect
procedure dump_sym_psect;

(* dump symbol and psect tables *)

begin  (* dump_sym_psect *)
end;    (* dump_sym_psect *)
$PAGE err
procedure err(msg:str120; var error:boolean; var err_count:integer);

(*
*)

begin  (* err *)
error := true;
err_count := err_count + 1;
writeln(tty,msg,' line:',linenum)
end;   (* err *)
$PAGE squeeze
function squeeze(str:str120):str120;

(*
*)

var
qstr:str120;
i : integer;

begin  (* squeeze *)
qstr := '';
for i := 1 to length(str) do 
   if str[i] <> ' ' then qstr := qstr || str[i];
squeeze := qstr
end;  (* squeeze *)
$PAGE store_op
procedure store_op(str:str120; var op_array:opnd_array; var ind:integer);

(*
*)

begin (* store_op *)
new(op_array[ind],length(str));
op_array[ind]^ := str;
ind := ind + 1
end;   (* store_op *)
$PAGE legal_sym
function legal_sym(str:str120;var error:boolean; var err_count:integer):boolean;

(*
return true if str is a legal symbol
*)

const
legal_sym_set : set of char := ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '$'];

var
len,i : integer;

begin  (* legal_sym *)
error := false;
len := length (str);
if len > 15 then err('symbol too long',error,err_count)
else if len <1 then err('illegal syntax',error,err_count)
   else if not (str[1] in ['A'..'Z','a'..'z']) then err('sym must begin with char',error,err_count)
      else for i := 2 to len do
           if not(str[i] in legal_sym_set) then err('illegal char in symbol',error,err_count);
legal_sym := not error
end;   (* legal_sym *)
$PAGE find_sym
function find_sym(name:str15):^symtab_rec;

(*
search the symbol table for the the entry that matches 'name'.
return a pointer to the symbol table entry, or nil if not found.
*)

var
sptr : ^symtab_rec;
found : boolean;

begin  (* find_sym *)
sptr := symtab_head;
found := false;
while (sptr<>nil) and (not found) do 
   if (sptr^.sname = name) then found := true
   else sptr := sptr^.nxtptr;
find_sym := sptr
end;   (* find_sym *)
$PAGE put_sym
procedure put_sym(name:str15; psect_num:integer; value:integer; 
                  a_r_e:relocation_type; g_l_e:sym_type;
                  var error:boolean; var err_count:integer);

(*
put new symbol and its associated data into the symbol table
*)

var
sptr : ^symtab_rec;

begin  (* put_sym *)
if not legal_sym(name,error,err_count) then err('illegal symbol',error,err_count)
else begin
sptr := find_sym(name);
if sptr <> nil then
  if (sptr^.glolocext = ext) and (sptr^.val = undefined) then
    sptr^.val := value
  else
    err('multiply defined symbol',error,err_count)
else begin
   new(sptr);
   with sptr^ do begin
   sname := name;
   val := value;
   absrelext := a_r_e;
   glolocext := g_l_e;
   nxtptr := nil;
   if symtab_head = nil then 
       symtab_head := sptr;
   if symtab_tail <> nil then symtab_tail^.nxtptr := sptr;
   symtab_tail := sptr
   end
end
end
end;   (* put_sym *)
$PAGE find_psect_by_name
function find_psect_by_name(name:str15):^psect_rec;

(*
search the psect table for an entry matching 'name'.  return a pointer to
the record or nil if not found.
*)

var
psect_ptr : ^psect_rec;
found : boolean;

begin  (* find_psect_by_name *)
psect_ptr := psect_head;
found := false;
while (psect_ptr<>nil) and (not found) do 
   if psect_ptr^.pname = name then found := true
   else psect_ptr := psect_ptr^.nxtptr;
find_psect_by_name := psect_ptr
end;   (* find_psect_by_name *)
$PAGE find_psect_by_num
function find_psect_by_num(num:integer):^psect_rec;

(*
search the psect table for an entry with a number matching 'num'.
return a pointer to the record , or nil if not found.
*)

var
psect_ptr : ^psect_rec;
found : boolean;

begin (* find_psect_by_num *)
psect_ptr := psect_head;
found := false;
while (psect_ptr<>nil) and not found do
   if psect_ptr^.pnum = num then found := true
   else psect_ptr := psect_ptr^.nxtptr;
find_psect_by_num := psect_ptr
end;  (* find_psect_by_num *)
$PAGE num_of_opnds
function num_of_opnds(aray:opnd_array):integer;

(*
aray is an array [1..9] of pointers.  return the number of non nil pointers
in the array.  (starting at the beginning of the array, once a nil pointer
is found, all following pointers are nil)
*)

var
i : integer;

begin  (* num_of_opnds *)
i := 1;
while (aray[i]<>nil) and (i<=9) do
  i := i+1;
num_of_opnds := i-1
end;   (* num_of_opnds *)
$PAGE do_new_psect
procedure do_new_psect(parameters:opnd_array;
                      var curr_psect:integer; var loc:integer;
                      var error:boolean; var err_count:integer);

(*
given a new psect name (in parameters[1]), if is does not already exist,  
add it to the psect table with its associated parameters.
store the current location counter in the old psect (if there was one)
and then set the current location counter to that found in the new psect.
*)

var
pptr, psect_ptr : ^psect_rec;
i,j : integer;
value : integer;
found : boolean;
numops : integer;

begin (* do_new_psect *)
if not legal_sym(parameters[1]^,error,err_count) then err('illegal psect symbol',error,err_count)
else begin
  psect_ptr := find_psect_by_name(parameters[1]^);
  if psect_ptr = nil then begin
    new (psect_ptr);
    with psect_ptr^ do begin
      pname := parameters[1]^;
      pnum := nxt_psect_num;
      nxt_psect_num := nxt_psect_num + 1;
      loc := 0;
      numops := num_of_opnds(parameters);
      if numops = 1 then err('no opnds on .psect dir',error,err_count)
      else if numops <> 5 then 
	err('too many params',error,err_count)
      else begin
        getstring(parameters[2]^,value);
        if (value < 0) or (value > 9) then
         err('first param must be in [0..9]',error,err_count)
        else begin
          getstring(parameters[2]^,align);
          params := [];
          for i := 3 to 5 do begin
            found := false;
            j := 0;
            while (j < 7 ) and not found do  begin
              j := j+1;
              if psect_params[j] =  parameters[i]^ then found := true
              end;
            if not found then
	        err('illegal param',error,err_count)
            else case j of
            1 : params := params + [psect_executable];
            3 : params := params + [psect_readable];
            5 : params := params + [psect_writable];
            others :
            end  (* case *)
            end;
          if psect_head = nil then psect_head := psect_ptr;
          if psect_tail <> nil then psect_tail^.nxtptr := psect_ptr;
          psect_ptr^.nxtptr := nil;
          psect_tail := psect_ptr
          end
          end
         end
         end;  (* with *)
     if curr_psect <> -1 then begin
       pptr := find_psect_by_num(curr_psect);
       if pptr = nil then err('psect error',error,err_count)
       else pptr^.loc := loc
       end;
   curr_psect := psect_ptr^.pnum;
    loc := psect_ptr^.loc
  end
end;  (* do_new_psect *)
$PAGE isp/icp
(*
FUNCTIONS = ISP ICP -- in stack priority/incoming priority of operator or delimiter.
Calling sequence:
  priority = isp/icp(character)
Where:
  character is char operator or delimiter (one of : "<", "+", "-", "*", "/", "^")
Return:
  priority is integer in-stack-priority / incoming-priority of character.
*)

function isp(character:char):integer;
begin
case character of 
  '<' : isp := 0;
  '+' : isp := 2;
  '-' : isp := 2;
  '*' : isp := 2;
  '/' : isp := 2;
  '^' : isp := 3;
  '@' : isp := -1;  (* bottom of stack *)
  others : writeln(tty,' OOPS: isp given ',character);
end
end ; (* isp *)



function icp(character:char):integer;
begin
case character of
  '<' : icp := 4;
  '+' : icp := 2;
  '-' : icp := 2;
  '*' : icp := 2;
  '/' : icp := 2;
  '^' : icp := 3;
  others : writeln(tty,' WHOA: icp given ',character);
end
end ; (* icp *)
$PAGE stack_em
(*
PROCEDURE = STACK_EM -- push operators/delimiter onto the postfix stack.
Calling sequence:
  stack_em(expr,buf,stack_ptr,unary_flag)
Where:
  expr is str120 rest of infix expression to be parsed; expr[1] is operator/delimiter.
Return:
  buf is str120 postfix expression buffer.
  stack_ptr is ^stack of current stack top.
  unary_flag is boolean flag set to true if the next operator is allowed to be unary minus.
Note:
  if the incoming operator has lower priority than operators on top of the
  stack, the stack is popped until it has lower priority.
*)

procedure stack_em(var expr:str120; var buf:str120;
          var stack_ptr:^stack; var unary_flag:boolean);
var temp : ^stack;
begin
if (expr[1] = '-') and (unary_flag) then expr[1] := '^'; (* change minus to unary minus *)
while isp(stack_ptr^.entry[1]) >= icp(expr[1]) do
  begin
  buf := buf || stack_ptr^.entry;
  temp := stack_ptr^.next;
  dispose(stack_ptr);
  stack_ptr := temp
  end;
new(temp);
temp^.entry := expr[1];
temp^.next := stack_ptr;
stack_ptr := temp;
unary_flag := true
end ;  (* stack_em *)
$PAGE unstack
(*
  PROCEDURE = UNSTACK -- pop the postfix stack until balancing  bracket '<' is found.
Calling sequence:
  unstack(stack_ptr,stack_head,buf,error,err_count)
Return:
  stack_ptr is ^stack to current top of postfix stack.
  stack_head is ^stack to bottom of stack.
  buf is str120 buffer to append more of postfix expression to.
  error is boolean flag; set to true if error found.
  err_count is integer total number of errors in module.
*)

procedure unstack(var stack_ptr:^stack; stack_head:^stack; var buf:str120;
          var error:boolean; var err_count:integer);
var
  temp : ^stack;
begin
while (stack_ptr^.entry <> '<' ) and (not error) do
  begin
  buf := buf || stack_ptr^.entry;
  temp := stack_ptr^.next;
  dispose(stack_ptr);
  stack_ptr := temp;
  if stack_ptr = stack_head then
    err('unbalanced brackets',error,err_count)
  end;
if (not error) then
  begin
  temp := stack_ptr^.next;
  dispose(stack_ptr);
  stack_ptr := temp
  end
end; (* unstack *)
$PAGE process_operands
(*
PROCEDURE = PROCESS_OPERANDS -- append next operand or its value to postfix notation.
Calling sequence:
  process_operands(expr,idx,buf,expr_type,rel_flag,error,err_count)
Where:
  expr is str120 infix representation of rest of expression to be parsed.
  idx is integer index into expr of next operator or delimiter.
Return:
  buf is str120 appended postfix representation --
    if operand is constant, append '#' constant '#'
    if operand is symbol,   append '#' value '#' or '!' symbol '!'
    if operand is psect_orig_expr, append '\' psectname '\'
  expr_type is expression_type kind of expression found.
  rel_flag is boolean relocatable symbol flag.
  error is boolean flag; set to true if error found.
  err_count is integer total number of errors in module.
Note:
  expr_type is only conclusively set to 'extrnl' or 'psect_orig'.
  rel_flag has been punted for the time being.
*)

procedure process_operands(expr:str120;var idx:integer;var buf: str120;
          var expr_type:expression_type;var rel_flag:boolean;
          var error:boolean;var err_count:integer);
var
  opnd,num : str32;
  value : integer;
  sym_ptr : ^symtab_rec;
  local : io_status;
begin
opnd := substr(expr,1,idx-1);
getstring(opnd,value);
local := iostatus;
if local <> io_dgit then (* found constant *)
  buf := buf || '#' || opnd || '#'
else if (idx < length(expr)) andif (expr[idx] = '\') then (* try psect origin expression *)
  if buf <> '' then
    err('psection origin expression must appear alone',error,err_count)
  else
    begin
    buf := '\' || opnd || '\';
    idx := idx +1;
    expr_type := psect_orig
    end
else (* must be a symbol *)
  begin
  sym_ptr := find_sym(substr(opnd,1,min(length(opnd),15)));
  if sym_ptr = nil then
    err('undefined symbol',error,err_count)
  else
    case sym_ptr^.absrelext of
      abs: begin
        putstring(num,sym_ptr^.val);
	buf := buf || '#' || num || '#' ;
        end;
      rel: begin
        rel_flag := true;
        expr_type := relo;
        putstring(num,sym_ptr^.val);
        buf := buf || '#' || num || '#' 
        end;
      extrnl: begin
        expr_type := extr;
        buf := buf || '!' || opnd || '!'
        end;
    end
  end
end; (* process_operands *)
$PAGE postfix
(*
FUNCTION = POSTFIX -- return the postfix notation for a given expression.
Calling sequence:
  post_exp = postfix(expr,type,error,err_count)
Where:
  expr is str120 infix representation of expression to be evaluated --
    expr contains no embedded blanks; symbols must be checked for legality.
  type is expression_type kind of expression found.
  error is boolean flag; set to true if error found.
  err_count is integer total number of errors in module.
Return:
  post_exp is str120 postfix representation of expr.  Constants are 
    enclosed in '#'; external symbols are enclosed in '!'
Notes:
  1. expr can contain: legal_symbol, +, -, /, *, <, or > .
  2. A special form of absolute expr is:  relocatable_expr - relocatable_expr.
  3. All other absolute expressions must contain absolute symbols.
*)

function postfix(var expr:str120;var expr_type:expression_type;var error:boolean;var err_count:integer):str120;
var
  tok_idx,num_err : integer;
  opnd : str32;
  rel_flag,unary_flag : boolean;
  sym_ptr : ^symtab_rec;
  stack_ptr,stack_head,temp : ^stack;
  buf : str120;
begin
buf := '';
expr_type := abso;
rel_flag := false;
unary_flag := true;
new(stack_ptr);
stack_ptr^.entry := '@'; (* bottom of stack *)
stack_head := stack_ptr;
num_err := err_count;
while (expr <> '') and (num_err = err_count) do
  begin
  tok_idx := search(expr,['<','>','+','-','*','/','\'],length(expr)+1);
  if tok_idx <> 1 then
    begin
    unary_flag := false;
    process_operands(expr,tok_idx,buf,expr_type,rel_flag,error,err_count)
    end
  else if expr[1] = '>' then (* unstack to balancing bracket *)
    begin
    tok_idx := tok_idx + 1;
    unary_flag := false;
    unstack(stack_ptr,stack_head,buf,error,err_count)
    end
  else (* stack operator or delimiter *)
    if expr[1] = '\' then
      err('null psection specification',error,err_count)
    else
      begin
      tok_idx := tok_idx + 1;
      stack_em(expr,buf,stack_ptr,unary_flag)
      end;
  expr := substr(expr,tok_idx)
  end; (* while *)
(* now unstack and dump to buf *)
while stack_ptr <> stack_head do
  begin
  if stack_ptr^.entry = '<' then
    err('unbalanced brackets',error,err_count)
  else
    buf := buf || stack_ptr^.entry;
  temp := stack_ptr^.next;
  dispose(stack_ptr);
  stack_ptr := temp
  end;
dispose(stack_head);
postfix := buf
end; (* postfix *)
$PAGE eval_expr
(*
  FUNCTION = EVAL_EXPR -- return the value of a given expression.
  Calling sequence:
   value = eval_expr(expr,error,err_count)
  Where:
   expr is str120 postfix notation of expression to be evaluated, as returned
    from function postfix.
   error is boolean flag; set to true if error found.
   err_count is integer total number of errors in module.
  Return:
   value is integer value of evaluated expression.
*)

function eval_expr(var expr:str120; var error : boolean; var err_count : integer):integer;
var
  idx,op1,ol,num_err : integer;
  opnd : str15;
  stack_head,stack_ptr,temp : ^eval_stack;
begin
new(stack_ptr);
stack_head := stack_ptr ;
num_err := err_count;
while (expr <> '') and (num_err = err_count) do
  begin
  idx := 1;
  case expr[idx] of
    '#' : begin
      expr := substr(expr,2);
      idx := search(expr,['#']);
      if idx = 0 then
        err('postfix operand without closing "#"',error,err_count)
      else
        begin
        opnd := substr(expr,1,idx-1);
        new(temp);
        getstring(opnd,temp^.entry);
        temp^.next := stack_ptr;
        stack_ptr:= temp
        end
      end;
    '+','-','*','/' : begin  (* pop twice and operate *)
      if (stack_ptr = stack_head) orif (stack_ptr^.next = stack_head) then
        err('illegal expression',error,err_count)
      else
        begin
        op2 := stack_ptr^.entry;
        temp := stack_ptr^.next;
        dispose(stack_ptr);
        stack_ptr := temp;
        op1 := stack_ptr^.entry;
        temp := stack_ptr^.next;
        dispose(stack_ptr);
        stack_ptr := temp;
        case expr[idx] of
          '+' : val := op1 + op2;
          '-' : val := op1 - op2;
          '*' : val := op1 * op2;
          '/' : val := op1 div op2;
        end;
        (* now push onto stack *)
        new(temp);
        temp^.entry := val;
        temp^.next := stack_ptr;
        stack_ptr := temp;
        end
      end;
    '^' : begin
      if (stack_ptr = stack_head) then
        err('illegal expression!',error,err_count)
      else
        stack_ptr^.entry := -stack_ptr^.entry;
      end;
    end;
  expr := substr(expr,idx+1)
  end; (* while *)
if stack_ptr^.next <> stack_head then
  err('invalid expresion!!',error,err_count);
eval_expr := stack_ptr^.entry;
dispose(stack_ptr);
if stack_head <> stack_ptr then dispose(stack_head)
end ;  (* eval_expr *)
$PAGE readline
procedure readline(var lab:str15; var inst:str15; 
               var op_array:opnd_array; line:str120;
               var error:boolean; var err_count:integer);

(*
*)

const
legal_ends:set of char := ['A'..'Z', 'a'..'z', '0'..'9', ')', ']', '>', '.', '_', '$'];
var
i,cmdidx,array_index,ind : integer;
str,cmdline:str120;
done,string_done : boolean;

begin  (* readline *)
linenum := linenum + 1;
error := false;
done := false;
cmdline := line;
lab := '';
inst := '';
array_index := 1;
if cmdline = '' then begin
   writeln(tty,'blank line ignored');
   break
   end
else begin
   cmdline := substr(cmdline,verify(cmdline,[' ']));  (* remove leading blanks *)
   cmdidx := search (cmdline,delims);
   if cmdidx = 0 then begin
      if cmdline = '.END' then 
        inst := cmdline
      else err('illegal syntax',error,err_count);
       done := true
      end
   else
   case cmdline[cmdidx] of
      '=' : if cmdidx=1 then err('syntax error',error,err_count)
            else begin
            lab := substr(cmdline,1,cmdidx-1);
            inst := '='
            end;

     ' ' : begin
          ind := verify(substr(cmdline,cmdidx+1),[' ']);
          if cmdline[cmdidx+ind] = '=' then begin
            lab := substr(cmdline,1,cmdidx-1);
            inst := '=';
            cmdidx := cmdidx+ind
            end
          else inst := substr(cmdline,1,cmdidx-1)
          end;

     ';' : begin
	   if cmdidx <> 1 then inst := substr(cmdline,1,cmdidx);
           done := true
           end;

     ':' : if cmdidx = 1 then err('illegal syntax',error,err_count)
	   else lab := substr(cmdline,1,cmdidx-1);  (* get label from cmdline *)

     others : err('illegal character - line ignored',error,err_count)
     end;  (* case *)


   cmdline := substr(cmdline,cmdidx+1);  (* remove token from cmdline *)
   while (cmdline<>'') and (not done) and ( not error)  do begin
     if array_index > 9 then err('too many arguments',error,err_count)
     else begin
       cmdline := substr(cmdline,verify(cmdline,[' ']));  (* leading blanks *)
       cmdidx := search(cmdline,delims);
       if cmdidx = 0 then
         begin
         if cmdline[length(cmdline)] in legal_ends then
            begin
            if inst = '' then
              if not ( cmdline[length(cmdline)] in ['1'..'8'] ) then
                err('illegal syntax',error,err_count)
              else inst := cmdline
              else store_op(cmdline,op_array,array_index);
           end
         else 
            err('illegal syntax',error,err_count);
         done := true
      end
       else
       case cmdline[cmdidx] of
         ':' : err('illegal symbol',error,err_count);

         '''' : if inst <> '.ASCII' then err ('illegal symbol',error,err_count)
                else begin
                  if length(cmdline) = 1 then err('invalid ascii string',error,err_count);
                  if length(cmdline) > 1 then cmdline := substr(cmdline,2);  (* remove 1st ' *)
                  str := '';
                  string_done := false;
                  while not string_done and not error do
                    begin
		    ind := search(cmdline,['''']) ;
		    if ind = 0 then
		      err('ascii string with unbalanced delimiter',error,err_count)
		    else
		      begin
		      if ind > 1 then str:= str || substr(cmdline,1,ind-1);
		      if (ind=length(cmdline)) orif (cmdline[ind+1]<>'''') then
			begin
			if ind = length(cmdline) then cmdline:=''
			  else cmdline := substr(cmdline,ind+1);
			ind := 0;
			string_done := true
			end
		      else
			begin
			str := str || '''' ;
			cmdline := substr(cmdline,ind+2)
			end
		      end
		    end;  (* while *)
                  store_op(str,op_array,array_index);
                  if length(cmdline) > 0 then ind := verify(substr(cmdline,1),[' ']);
                  if (ind<>0) andif (cmdline[ind]<>';') then
                     err('illegal character',error,err_count);
                  done := true
                  end;  (* '''' *)

               ' ' : if inst = '' then
                       inst := substr(cmdline,1,cmdidx-1)
                    else begin  (* get opnd or expr and remove blanks *)
                      ind := search(cmdline,exprends);
                      if ind = 0 then begin
                        store_op(squeeze(cmdline),op_array,array_index);
                        done := true
                        end
                      else begin
                        store_op(squeeze(substr(cmdline,1,ind-1)),op_array,array_index);
                        if cmdline[ind] = ';' then done := true
                        else if cmdline[ind] = ',' then cmdidx := ind
                        else cmdidx := ind-1
                        end
                      end;

	      '(','[' : if inst = '' then err('illegal syntax',error,err_count)
			else begin
                        if cmdidx <> 1 then 
			  store_op(substr(cmdline,1,cmdidx-1),op_array,array_index);
			  if cmdline[cmdidx] = '[' then ind := search(cmdline,[']'])
			  else ind := search(cmdline,[')']);
			if ind = 0 then err('missing ) or ]',error,err_count)
			else begin
			  store_op(squeeze(substr(cmdline,cmdidx,ind-cmdidx+1)),op_array,array_index);
			  i := verify(substr(cmdline,ind+1),[' ']);
			  if (i<>0) andif (cmdline[ind+i] = ',') then cmdidx := ind+i
			  else cmdidx := ind
			  end
		       end;

	      ',' : if inst = '' then err('illegal character',error,err_count)
		    else if cmdidx=1 then err('illegal syntax',error,err_count)
		      else store_op(substr(cmdline,1,cmdidx-1),op_array,array_index);

	      ';' : begin
		    if cmdidx <> 1 then 
		       store_op(substr(cmdline,1,cmdidx-1),op_array,array_index);;
		    done := true
		    end;

	      others : err('illegal character',error,err_count)
	      end (* case *)
	 end;
       if cmdline <> '' then cmdline := substr(cmdline,cmdidx+1)
       end;  (* while *)
end
end;   (* readline *)
$PAGE pvmasm

var
i : integer;
lab:str15;
inst:str15;
aray:opnd_array := (nil,nil,nil,nil,nil,nil,nil,nil,nil);
inf:text;
fname,post_expr,cmdline,in_expr:str120;
expr_type : expression_type;
error:boolean := false;
done : boolean := false;
loc : integer := 0;
title : str15 := '';
psect : integer := -1;
err_count : integer := 0;
ptr : ^symtab_rec;
found : boolean;
j : integer;

begin (* pvmasm *)
open(tty);
rewrite(tty);
write(tty,'filename: ');
break(tty);
readln(tty);
read(tty,fname);
if (fname='*') or (fname='tty') or (fname='tty:') or (uppercase(fname)='TERMINAL') then inf := tty 
  else open(inf,fname);
readln(inf);
read(inf,cmdline);
while not eof(inf) and not error and (title='') do begin
  readline(lab,inst,aray,uppercase(cmdline),error,err_count);
   if (inst = '.TITLE') andif legal_sym(aray[1]^,error,err_count) then
        title := aray[1]^
   else if (inst<>'') or (lab <> '') then
     err('.TITLE must be first instruction',error,err_count);
   if aray[1] <> nil then dispose(aray[1]);
   aray[1] := nil;
   readln(inf);
   read(inf,cmdline)
   end;

if eof(inf) then err('no .TITLE directive',error,err_count);
if title <> ''  then
   while not eof(inf) and (psect=-1) and not error do begin
     readline(lab,inst,aray,uppercase(cmdline),error,err_count);
     if inst='.PSECT' then 
       begin do_new_psect(aray,psect,loc,error,err_count);
       for i := 1 to 9 do begin
          if aray[i] <> nil then begin
           dispose(aray[i]);
           aray[i] := nil
           end
         end
       end
     else if (inst<>'')or (lab<>'') then
       err('code appears before PSECT def',error,err_count);
     readln(inf);
     read(inf,cmdline)
    end;

if (psect<>-1) and (title<>'') then
  while  not eof(inf) do begin
    if done then err('text appears after .END directive',error,err_count)
   else begin
      readline(lab,inst,aray,uppercase(cmdline),error,err_count);
    j := 0;
    found := false;
    while (j<max_num_opnds) and not found do begin
      j := j+1;
      if opco_table[j].mnemonic = inst then found := true
      end;
   if not found then err('Illegal Instruction',error,err_count)
   else
   case j of
   (*   =   *)
     1 : if num_of_opnds(aray) <> 1 then err('too many operands',error,err_count)
           else if lab = '' then err('syntax error in = directive',error,err_count)
           else
             begin
             in_expr := aray[1]^;
             post_expr := postfix(in_expr,expr_type,error,err_count);
             if expr_type <> abso then
               err('absolute expression required',error,err_count)
             else
	       put_sym(lab,psect,eval_expr(post_expr,error,err_count),abs,lcl,error,err_count);
             end;

   (*   .BLKB   *)
     7 : begin
		if num_of_opnds(aray) <> 1 then err('too many operands in .blkb',error,err_count)
               else if lab <>'' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
               in_expr := aray[1]^;
               post_expr := postfix(in_expr,expr_type,error,err_count);
               if expr_type <> abso then
                 err('expression must be absolute',error,err_count)
               else
		 loc := loc + eval_expr(post_expr,error,err_count)
               end;

   (*   .PSECT   *)
     3 : if lab <> '' then err('no label allowed on .PSECT',error,err_count)
                else if num_of_opnds(aray) > 5 then err('too many operands',error,err_count)
               else do_new_psect(aray,psect,loc,error,err_count);

   (*   .GLOBAL   *)
     4 : if lab <> '' then err('label not allowed on .GLOBAL dir',error,err_count)
                 else begin
                   i := 1;
                   while (aray[i]<>nil) and not error do begin
                     ptr := find_sym(aray[i]^);
                     if ptr = nil then err('symbol must be defined before declared global',error,err_count)
                     else ptr^.glolocext := glo;
                     i := i + 1
                     end
                   end;

   (*   .EXTERNAL   *)
     5 : if lab <> '' then err('label not allowed on .EXTERNAL dir',error,err_count)
                   else begin
                     i  :=  1;
                     while (aray[i]<>nil) and not error do begin
                       put_sym(aray[i]^,psect,undefined,extrnl,ext,error,err_count);
                       i := i + 1
                        end
                       end;

   (*   .END   *)
     6 : if lab <> '' then err('label not allowed on .END statement',error,err_count)
              else if num_of_opnds(aray) > 1 then err('no opnds allowed on .END',error,err_count)
              else done := true;

   (* .BYTE  *)
     8 : begin
               if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                loc := loc + 1
              end;

   (*   .WORD   *)
    9 : begin
               if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
               loc := loc + 2
               end;

   (*  .LONGWORD   *)
     10 : begin
                   if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                   loc := loc + 4
                    end;

   (*   .ASCII   *)
    11 : begin
               if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
               loc := loc + length(aray[1]^)
               end;

         (* type 1 or 2   *)
                    12..15 : begin
                           if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                                  loc := loc+2
                                  end;

                 (*   type 3 or 4   *)
                    16..19 : begin
                                 if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                                 loc := loc + 4
                                 end;

                (* type 5,6, or 7  *)
                   20..25 : begin
                          if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                                 loc := loc + 6
                                 end;

        (*   type 8    *)
                         26,27 : begin
                                 if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);
                                 loc := loc + num_of_opnds(aray)
                                 end;
          (*    (blank line or comment)   *)
                   28 : if lab <> '' then put_sym(lab,psect,loc,rel,lcl,error,err_count);

     others : err('illegal instruction',error,err_count)
     end;   (* case *)
  for i := 1 to 9 do begin
     if aray[i] <> nil then begin
      dispose(aray[i]);
      aray[i] := nil
      end
    end

end;
readln(inf);
read(inf,cmdline)
end;
if not done then err('no .END encountered',error,err_count);
dump_sym_psect;
if inf <> tty then close(inf)
end.    (* pvmasm *)
   	bSÂ