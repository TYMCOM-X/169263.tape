$page
PROGRAM RGB;
(* included herein, are the type declarations for the rgb code
   generator. Each of the various fields is enumerated with the
   mnemonics used by the Data General MICROASM assembler. *)
  
$page d bus source
(* D BUS SOURCE is a 3 bit field in the micro-instruction which selects one
   of 8 possible sources to drive the D data bus. *)
 
type
  d_source_type = 
  
                    (* OCT  BIN                   source          *)
  
        (con,       (*  0   000     "immediate data" field of current micro-instruction. *)
         rsp,       (*  1   001     scratchpad RAM/character ROM  *)
         sts,       (*  2   010     status register  *)
         cmd,       (*  3   011     NOVA command register  *)
         arg,       (*  4   100     NOVA parameter register  *)
         pxld,      (*  5   101     video memory read  *)
         rdar,      (*  6   110     UART data input  *)
         fifo);     (*  7   111     prefetcher FIFO data  *)
$page a_register-b_register
(* A REGISTER and B REGISTER are each 4 bit fields which designate which of the
   16 general purpose registers will be available on the inputs to the ALU.  *)
  
  a_register_type = (ar0,ar1,ar2,ar3,ar4,ar5,ar6,ar7,ar8,ar9,ar10,ar11,ar12,ar13,ar14,ar15);
  
  
  b_register_type = (br0,br1,br2,br3,br4,br5,br6,br7,br8,br9,br10,br11,br12,br13,br14,br15);
$page alu_function
(* ALU FUNCTION is an 8 bit field made up of 4 micro-field in the following
   format:
  
           ALU_FUNCTION = ALU_OPERATION+ALU_SOURCE+CSYB+CSYA
   
   in each of the followics mnemonics, each letter represents a certain ALU
   SOURCE or ALU OPERATION. i.e.:
  
                              A- A REGISTER
                                 (A' if followed by a B)
                              B- B REGISTER
                                 (B' if followed by a B)
                              D- DIRECT DATA INPUT
                                 (D' if followed by a b)
                              Q- Q REGISTER
                                 (Q' if followed by a B)
                              C- CARRY OF PREVIOUS OPERATION
                                 (C' if followed by a B)
                              P- PLUS
                              M- MINUS
                              1- LOGICAL 1
                              0- LOGICAL 0
                             OR- LOGICAL OR OPERATION
                            AND- LOGICAL AND OPERATION
                            XOR- LOGICAL EXCLUSIVE-OR OPERATION
                            XNR- LOGICAL EXCLUSIVE-NOR OPERATION
 
                                                                *)
  
  alu_function_type = (apq,   apqpcb,   apqpc,   apqp1,   
                       apb,   apbpcb,   apbpc,   apbp1,   
                         q,     qpcb,     qpc,     qp1,   
                         b,     bpcb,     bpc,     bp1,   
                         a,     apcb,     apc,     ap1,   
                       dpa,   dpapcb,   dpapc,   dpap1,   
                       dpq,   dpqpcb,   dpqpc,   dpqp1,   
                         d,     dpcb,     dpc,     dp1,   
  
                       qma,   qmamcb,   qmamc,   qmam1,   
                       bma,   bmamcb,   bmamc,   bmam1,   
                       qmz,     qmcb,     qmc,     qm1,   
                       bmz,     bmcb,     bmc,     bm1,   
                       amz,     amcb,     amc,     am1,   
                       amd,   amdmcb,   amdmc,   amdm1,   
                       qmd,   qmdmcb,   qmdmc,   qmdm1,   
                        md,    mdmcb,    mdmc,    mdm1,   
 
                       amq,   amqmcb,   amqmc,   amqm1,   
                       amb,   ambmcb,   ambmc,   ambm1,   
                        mq,    mqmcb,    mqmc,    mqm1,   
                        mb,    mbmcb,    mbmc,    mbm1,   
                        ma,    mamcb,    mamc,    mam1,   
                       dma,   dmamcb,   dmamc,   dmam1,   
                       dmq,   dmqmcb,   dmqmc,   dmqm1,   
                       dmz,     dmcb,     dmc,     dm1,   
 
 
(* Following, are the ALU logic mode mnemonics. There are numerous
   "don't care" bit patterns in the ALU_FUNCTION field. In these cases,
   the mnemonic "FILLn" is used to take up that spot in the enumeration.  *)
  
                    aorq,      aorb,     fill3,     fill4,   
                   fill5,      dora,      dorq,     fill6,   
             
                   fill7,     fill8,     fill9,    fill10,   
                  fill11,    fill12,    fill13,    fill14,   
                  fill15,    fill16,    fill17,    fill18,   
                  fill19,    fill20,    fill21,    fill00,   
                  fill22,    fill23,    fill24,    fill25,   
                  fill26,    fill27,    fill28,    fill29,   
  
                    andq,      andb,      zero,    fill30,   
                  fill31,      dnda,      dndq,    fill32,   
 
                  fill33,    fill34,    fill35,    fill36,   
                  fill37,    fill38,    fill39,    fill40,   
                  fill41,    fill42,    fill43,    fill44,   
                  fill45,    fill46,    fill47,    fill48,   
                  fill49,    fill50,    fill51,    fill52,   
                  fill53,    fill54,    fill55,    fill56,   
  
                   abndq,     abndb,    fill57,    fill58,   
                  fill59,     dbnda,     dbndq,    fill60,   
  
                  fill61,    fill62,    fill63,    fill64,   
                  fill65,    fill66,    fill67,    fill68,   
                  fill69,    fill70,    fill71,    fill72,   
                  fill73,    fill74,    fill75,    fill76,   
                  fill77,    fill78,    fill79,    fill80,   
                  fill81,    fill82,    fill83,    fill84,   
 
                    axrq,      axrb,    fill85,    fill86,   
                  fill87,      dxra,      dxrq,    fill88,   
  
                  fill89,    fill90,    fill91,    fill92,   
                  fill93,    fill94,    fill95,    fill96,   
                  fill97,    fill98,    fill99,   fill100,   
                 fill101,   fill102,   fill103,   fill104,   
                 fill105,   fill106,   fill107,   fill108,   
                 fill109,   fill110,   fill111,   fill112,   
  
                   axnrq,     axnrb,        qb,        bb,   
                      ab,     dxnra,     dxnrq,        db);
  
  
(* NOTE- if D_SOURCE is "PXLD" and MSTRT is "S", then pixel data is
   available on the carry line. The ALU operations "APC", "BPC", and
   "QPC" can be used to perform operations on this pixel data.   *)
$page alu_destination
(* The ALU_DESTINATION is a 3-bit field which specifies what will be done with
   the result of the ALU operation being performed in the current micro-
   instruction and what will be placed on the y-data bus.  *)
  
  alu_destination_type =
      
    (nold,                (* ALU --> Y; No other loads *)
     ldqr,                (* ALU --> Y; ALU --> Q      *)
     load,                (* ALU --> Y; ALU --> B      *)
     aout,                (*   A --> Y; ALU --> B      *)
  
     rsbr,                (* ALU (shifted right 1 bit) --> B ( CRY --> MSB of b)
                             ALU --> Y                                         *)
 
     rsqr,                (* ALU (shifted right 1 bit) --> B (CRY --> MSB of B  
                             Q (shifted right 1 bit) --> Q (MSB of ALU --> MSB of Q)
                             ALU --> Y                                         *)
    
     lsbr,                (* ALU (shifted left 1 bit) --> B (CRY --> LSB of B)
                             ALU --> Y                                        *)
     lsqr);               (* Q (shifted left 1 bit --> Q (CRY --> LSB of Q
                             ALU (shifted left 1 bit) --> B (MSB of Q --> LSB of B)
                             ALU --> Y.                                       *)
  
  
(* NOTE- The D.G. convention for assigning mnemonics differs from the A.M.D.
         convention in that the LSB is complemented. The reason for this is
         as follows: When a field in the D.G. assembly code is missing, 0's
         are placed in the corresponding field in the micro-word. With this
         implementation of bit patterns, if a null operation is desired, the
         D.G. assembly code user can leave this field blank, which results
         in a '000' (the D.G. no-op) being assigned to the field, which is
         then changed to '001' (the A.M.D. no-op) by the graphics display
         hardware.                                                         *)
  
$page y_destination
(* The Y_DESTINATION is a 4-bit field which specifies where the data on the
   Y data bus will be routed.                                              *)
  
  Y_DEST_TYPE =
  
     (noop,             (* no-op  *)
      ldy,              (* Load video memory vertical address  *)
      ldx,              (* Load video memory horizontal address  *)
      wct,              (* Prefetcher word count  *)
      pfld,             (* prefetcher start address  *)
      lkup,             (* video lookup table  *)
      ldfn,             (* video memory access bits  *)
      lmd,              (* multiplier/divider  *)
      wsp,              (* write scratchpad RAM *)
      host,             (* load status register to be read by NOVA *)
      data,             (* load data register to be read by NOVA  *)
      ldcy,             (*  load cursor horizontal coordinate  *)
      ldcx,             (* load cursor horizontal coordinate  *)
      lsa,              (* scratchpad/char. ROM address  *)
      tds,              (* UART data input  *)
      misc);            (* miscellaneous control bits.  *)
 
$page multiplier/divider
(* The MULTIPLIER/DIVIDER field is a 3-bit field which specifies what
   operation is to be performed by the multiplier/divider chip.   *)
  
  muldiv_type = (m0,m1,m2,m3,m4,m5,m6,rmd);
  
(* NOTE- rmd forces the results of a multiply/divide ont the D data bus.  *)
  
  
  
$page constant-branch_addr field
(* The lowest 16 bits of the micro-instruction is used for immediate-type
   data or for branch address specification. if the contents is meant to 
   be an address, than only the 12 lsb's of the field are recognized.  **)
 
  const_or_branch_addr_type = 0..32768;
  
  
  
$page step field
(* The STEP field is a 2-bit field which indicates which pixel address
    will be incremented/decremented at the end of the current micro-instruction.  *)
 
  step_type = (no,x,y,xy);
  
  
  
  
$page mstrt field
(* The MSTRT field starts a video memory operation.  *)
  
  mstrt_type = (n,s);
$page test field
(* The TEST field is a 5 bit field which selects 1 of 16 conditions (or their
   complements) to drive the micro-program sequencer.  *)
  
  test_type =
  
     (false,         (* always false  *)
      true,          (* always true   *)
      vr,            (* True during vertical retrace  *)
      evnt,          (* True if (BUSY or FLG0 or CLIP or UART DATA AVAILABLE  *)
      flg3,          (* True if FLG3 is set  *)
      flg2,          (* True if FLG@ is set  *)
      flg1,          (* True if FLG1 is set  *)
      flg0,          (* True if FLG0 is set  *)
      clip,          (* True if X or Y addresses are "off" screen  *)
      movf,          (* True on multiply/divide overflow  *)
      aluz,          (* True if ALU result is 0  *)
      msb,           (* True if MSB of ALU is set  *)
      cout,          (* True if carry out of current ALU operation is high  *)
      aovr,          (* True if ALU operation causes overflow in arith. oper. *)
      y15,           (* True if the LSB of ALU is high  *)
  
  
                     (* NOTE- The following mnemonics represent the reversal
                              of the sense of the above test. In the case of
                              "true" and "false", "fill1" and "fill2" are used,
                              since "trueb" and "falseb" are not allowed by 
                              the assembler.  *)
  
  
      fill1,fill2,vrb,frb,evntb,flg3b,flg2b,flg1b,flg0b,
      clipb,movfb,aluzb,msbb,coutb,aovrb,f15b);
  
$page control field
(* The CONTROL field specifies which way the sequencing of the micro-program
   will be done and from which source, the branch address will be selected.  *)
  
  
  control_type =
  
       (jz,         (* jump zero *)
        cjsr,       (* conditional jump to subroutine *)
        cjmp,       (* conditional jump to branch address  *)
        push,       (* Ppush/conditional load of counter from constant field *)
        jsrp,       (* cond. jump to subr. via reg./counter or branch addr  *)
        jmpy,       (* unconditional jump to Y bus *)
        cldy,       (* cond. load of reg./counter from Y bus *)
        jrp,        (* cond. jump via reg./counter or branch address *)
        rfct,       (* repeat from top of stack if counter not = 0 *)
        rpct,       (* repeat from branch address if counter not = 0 *)
        crtn,       (* conditional return  *)
        cjpp,       (* cond. jump to branch addr and pop stack *)
        ldct,       (* load reg./counter and continue *)
        lup,        (* test end loop *)
        cont,       (* continue to next micro-instruction *)
        twb);       (* three way branch *)
$page rgb_word
(* RGB_WORD is the template of the micro-instruction.  *)
  
  rgb_word = record
 
         d_source: d_source_type;
         a_register: a_register_type;
         b_register: b_register_type;
         alu_function: alu_function_type;
         alu_destination: alu_destination_type;
         y_destination: y_dest_type;
         muldiv: muldiv_type;
         mstrt: mstrt_type;
         step: step_type;
         control: control_type;
         test: test_type;
         immediate: const_or_branch_addr_type
  
             end;
 
  
$page body of main
begin
end.
 