PMF VERSION 3.0 LIB FILE
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
E
M 10 685 #BEGINITEM
),#literal(#after_list,"#skip_after_last_item))$INDENT LEFT +#item_indent
E
M 10 303 #ENDMARGIN
#if("#gen("#num("#left_indenture),0),$INDENT LEFT  -#left_indenture  ,$INDENT LEFT  +#substr("#left_indenture,2)  )#if("#gen("#num("#right_indenture),0),$INDENT RIGHT  -#right_indenture ,$INDENT RIGHT  +#substr("#right_indenture,2) )$SKIP #lines_after_mar #undef(#left_indenture)#undef(#right_indenture)
E
M 8 311 #TAB_POP
#if("#gtn("#tab_default_level,0),#assign(#tab_default_level,"#num("#tab_default_level - 1))#undef(#tab_1)#undef(#tab_2)#undef(#tab_3)#undef(#tab_4)#undef(#tab_5)#undef(#tab_6)#undef(#tab_7)#undef(#tab_8)$TABS #tab_1<:,:>#tab_2<:,:>#tab_3<:,:>#tab_4<:,:>#tab_5<:,:>#tab_6<:,:>#tab_7<:,:>#tab_8 ,#pop_error(TAB))&
E
E
E
M 10 56 #ENDFIGURE
$JUS
E
E
M 11 224 #MARGIN_POP
#if("#gtn("#margin_default_level,0),#assign(#margin_default_level,"#num("#margin_default_level - 1))#undef(#left_indent)#undef(#right_indent)#undef(#lines_before_margin)#undef(#lines_after_margin),#pop_error(MARGIN))&$SKIP 0
E
E
E
M 4 206 #INC
)
M 4 118 #ARG

E
M 11 203 #FIGURE_POP
#if("#gtn("#figure_default_level,0),#assign(#figure_default_level,"#num("#figure_default_level - 1))#undef(#figure_indent)#undef(#skip_before_figure)#undef(#skip_after_figure),#pop_error(FIGURE))&$SKIP 0
E
E
E
M 8 167 #PAR_POP
#if("#gtn("#par_default_level,0),#assign(#par_default_level,"#num("#par_default_level - 1))#undef(#lines_before_par)#undef(#first_line_indent),#pop_error(PAR))&$SKIP 0
E
M 4 0 #COM

E
E
M 8 269 #TOC_POP
#if("#gtn("#toc_default_level,0),#assign(#toc_default_level,"#num("#toc_default_level - 1))#undef(#toc_more_pad)#undef(#toc_indent)#if("#gen("#level,3),#assign(#tocpad,"#substr("#blanks,1,"#num("#toc_indent * (#level - 2)))),#assign(#tocpad,)),&#pop_error(TOC))&$SKIP 0
E
M 7 10 #ENDUSE
#endfigure
E
M 4 102 #PAR
,$PARAGRAPH #first_line_indent)~A",$PARAGRAPH  ~A",$SKIP #lines_before_par)
E
L 10 0 #ITEM_FLAG

E
E
L 4 181 #TOC

E
E
M 13 418 #TITLE_PAGE_1
,\)
E
E
E
M 4 36 #RET

E
E
M 4 34 #USE
$skip 2
M 13 525 #ITEM_DEFAULT
),#literal(#skip_after_last_item,"#skip_after_last_item))#assign(#item_default_level,"#num("#item_default_level + 1))$SKIP 0~A%,#literal(#skip_after_last_item,~A%),#literal(#skip_before_first_item,"#skip_before_first_item))#if(~A$,#literal(#skip_before_first_item,~A$),#literal(#start_number,"#start_number))#if(~A#,#literal(#start_number,~A#),#literal(#skip_before_each_item,"#skip_before_each_item))#if(~A",#literal(#skip_before_each_item,~A"),#literal(#item_indentation,"#item_indentation))#if(~A!,#literal(#item_indentation,~A!#if(
M 13 532 #SECT_DEFAULT
),#literal(#skip_after_l3,"#skip_after_l3))#assign(#sect_default_level,"#num("#sect_default_level + 1))$SKIP 0~A&,#literal(#skip_after_l3,~A&),#literal(#skip_before_l3,"#skip_before_l3))#if(~A%,#literal(#skip_before_l3,~A%),#literal(#skip_after_l2,"#skip_after_l2))#if(~A$,#literal(#skip_after_l2,~A$),#literal(#skip_before_l2,"#skip_before_l2))#if(~A#,#literal(#skip_before_l2,~A#),#literal(#skip_after_l1,"#skip_after_l1))#if(~A",#literal(#skip_after_l1,~A"),#literal(#skip_before_l1,"#skip_before_l1))#if(~A!,#literal(#skip_before_l1,~A!#if(
E
M 12 637 #BEGINMARGIN
),#assign(#lines_after_mar,"#lines_after_margin)))~A#,#assign(#lines_after_mar,~A#),#if(~A$,#assign(#lines_after_mar,~A$),#literal(#right_indenture,"#right_indent)#if("#gen("#num("#right_indent),0),$INDENT RIGHT  +#right_indent,&$INDENT RIGHT   #right_indent)&)#if(~A",&$INDENT RIGHT  ~A"),0),$INDENT RIGHT +~A")#if("#gen("#num(~A",#literal(#right_indenture,~A"   ),#literal(#left_indenture,"#left_indent)#if("#gen("#num("#left_indent),0),$INDENT LEFT  +#left_indent  ,$INDENT LEFT   #left_indent  ))#if(~A!  ,$INDENT LEFT   ~A!),0),$INDENT LEFT  +~A!)#if("#gen("#num(~A!,#literal(#left_indenture,~A!  ,$SKIP #lines_before_margin  )#if(~A#,$SKIP ~A##if(
E
E
E
E
M 12 310 #BEGINFIGURE
),#assign(#lines_after_figure,"#skip_after_figure)))~A#,#assign(#lines_after_figure,~A#),#if(~A$,#assign(#lines_after_figure,~A$), #assign(#figure_indentation,#figure_indent))$IND +#figure_indentation
E
E
E
E
E
E
E
E
E
E
M 9 138 #ENDLEVEL
#assign( #level,"#num("#level - 1 ))#if("#gen("#level,3),#assign(#tocpad,"#substr("#tocpad,"#toc_indent + 1)),#assign(#tocpad,))&$LEVEL -1
E
M 10 259 #FOOTNOTES
),off),#assign(#footnote_type,OFF)$FOOTNOTES OFF))))~A!),right),#assign(#footnote_type,RIGHT),#if("#eqc("#lwc(~A!),left),#assign(#footnote_type,LEFT),#if("#eqc("#lwc(~A!),center)),#assign(#footnote_type,"#CENTER),#if("#eqc("#lwc(~A!),"#eqc("#lwc(~A!#if("#or("#not(
E
E
E
E
E
L 12 0 #MARGIN_FLAG

E
E
M 10 295 #FIX_TITLE
))#literal(#index_apos,"#search("#old_title,'))#if("#index_apos,#literal(#after_apos,"#substr("#old_title,"#num("#index_apos + 1)))#substr("#old_title,1,"#num("#index_apos))'#fix_title(#after_apos)#undef(#after_apos),#old_title)&#undef(#old_title)#undef(#index_apos)~A!#literal(#old_title,"#eval(
E
E
M 12 635 #TAB_DEFAULT
),#literal(#tab_8,"#num("#tab_7 + 1)))$TABS #tab_1<:,:>#tab_2<:,:>#tab_3<:,:>#tab_4<:,:>#tab_5<:,:>#tab_6<:,:>#tab_7<:,:>#tab_8&#assign(#tab_default_level,"#num("#tab_default_level + 1))~A(,#literal(#tab_8,~A(),#literal(#tab_7,"#num("#tab_6 + 1)))#if(~A',#literal(#tab_7,~A'),#literal(#tab_6,"#num("#tab_5 + 1)))#if(~A&,#literal(#tab_6,~A&),#literal(#tab_5,"#num("#tab_4 + 1)))#if(~A%,#literal(#tab_5,~A%),#literal(#tab_4,"#num("#tab_3 + 1)))#if(~A$,#literal(#tab_4,~A$),#literal(#tab_3,"#num("#tab_2 + 1)))#if(~A#,#literal(#tab_3,~A#),#literal(#tab_2,"#num("#tab_1 + 1)))#if(~A",#literal(#tab_2,~A"),#literal(#tab_1,1))#if(~A!,#literal(#tab_1,~A!#if(
M 9 186 #APPENDIX
&|U
E
E
E
E
M 10 55 #ITEM_TEST
#if("#item_flag,#undef(#item_flag)#enditem
E
M 15 402 #MARGIN_DEFAULT
),#literal(#lines_after_margin,"#lines_after_margin))&#assign(#margin_default_level,"#num("#margin_default_level + 1))$SKIP 0 ~A$,#literal(#lines_after_margin,~A$),#literal(#lines_before_margin,"#lines_before_margin))&#if(~A#,#literal(#lines_before_margin,~A#),#literal(#right_indent,"#right_indent))#if(~A",#literal(#right_indent,~A"),#literal(#left_indent,"#left_indent))#if(~A!,#literal(#left_indent,~A!#if(
E
M 9 3893 #DOCUMENT
),#literal(#page_width,75))$WIDTH #page_width
E
E
E
M 7 99 #OUTPUT
#item_test
M 15 329 #FIGURE_DEFAULT
),#literal(#skip_after_figure,"#skip_after_figure))&#assign(#figure_default_level,"#num("#figure_default_level + 1))$SKIP 0~A#,#literal(#skip_after_figure,~A#),#literal(#skip_before_figure,"#skip_before_figure))&#if(~A",#literal(#skip_before_figure,~A"),#literal(#figure_indent,"#figure_indent))&#if(~A!,#literal(#figure_indent,~A!#if(
E
E
M 6 84 #OTHER
&:
E
M 12 238 #PAR_DEFAULT
),#literal(#first_line_indent,"#first_line_indent))#assign(#par_default_level,"#num("#par_default_level + 1))$SKIP 0~A",#literal(#first_line_indent,~A"),#literal(#lines_before_par,"#lines_before_par))#if(~A!,#literal(#lines_before_par,~A!#if(
E
E
M 10 315 #POP_ERROR
&_DEFAULT ****
E
L 8 15 #PADDING
\\\\\\\\\\\\\\\
M 12 257 #TOC_DEFAULT
)#literal(#toc_more_pad,"#substr("#blanks,1,"#num("#toc_indent)))#if("#gen("#level,3),#assign(#tocpad,"#substr("#blanks,1,"#num("#toc_indent * (#level - 2))))))&#assign(#toc_default_level,"#num("#toc_default_level + 1))$SKIP 0~A!,
E
E
M 5 28 #TOC1
#item_test
E
E
M 11 141 #BEGINLEVEL
#assign( #level,"#num("#level + 1 ))#if("#gtn("#level,2),#literal(#temp,#tocpad)#assign(#tocpad,"#temp&#toc_more_pad)#undef(#temp))&$LEVEL +1
E
E
E
E
E
E
M 6 88 #INPUT

E
E
M 6 83 #ENTRY
)
E
E
E
M 8 186 #ENDITEM
$INDENT -&#item_indent
E
E
E
E
M 12 50 #MARGIN_TEST
#if("#margin_flag,#undef(#margin_flag)#endmargin)
E
E
M 9 255 #ITEM_POP
#if("#gtn("#item_default_level,0),#assign(#item_default_level,"#num("#item_default_level - 1))#undef(#item_indentation)#undef(#skip_before_each_item)#undef(#start_number)#undef(#skip_before_first_item)#undef(#skip_after_last_item),#pop_error(ITEM))$SKIP 0
M 9 255 #SECT_POP
#if("#gtn("#sect_default_level,0),#assign(#sect_default_level,"#num("#sect_default_level - 1))#undef(#skip_before_l1)#undef(#skip_after_l1)#undef(#skip_before_l2)#undef(#skip_after_l2)#undef(#skip_before_l3)#undef(#skip_after_l3),#pop_error(SECT))&$SKIP 0
E
E
E
E
M 5 245 #MEMO

E
M 5 453 #ITEM
) + 1))#assign(#count,"#num("#item_indent - #count))#if("#gtn("#count,0),#substr("#backslashes,1,"#count)))&~A!&\#assign(#count,"#num("#count + #length(~A!,~A!#assign(#count,0)$SKIP #before_each_item   $PARAGRAPH -&#item_indent
M 5 848 #SECT

E
    