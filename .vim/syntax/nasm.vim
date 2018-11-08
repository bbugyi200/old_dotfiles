" Vim syntax file
" Language:	MCS-51 intel Mnemonics (MCS51A)
" Maintainer:	Thomas Langewouters <thomas.langewouters@skynet.be>
" Last Change:	
" Vim URL:	http://www.vim.org/lang.html
" mcs51 Home:	---	
" mcs51 Version: 2.00

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

setlocal iskeyword=a-z,A-Z,48-57,.,_
setlocal isident=a-z,A-Z,48-57,.,_
syn case ignore

syn keyword cTodo		contained TODO FIXME
" a, b r0-7 done with matches
syn keyword mcs51Register	A B R0 R1 R2 R3 R4 R5 R6 R7
syn keyword mcs51Register	P0 SP DPL DPH TCON TMOD TL0 TL1 TH0 TH1 P1 SCON SBUF
syn keyword mcs51Register	P2 IE P3 IP PSW ACC IT0 IE0 IT1 IE1 TR0 TF0 TR1 TF1
syn keyword mcs51Register	RI TI RB8 TB8 REN SM2 SM1 SM0 EX0 ET0 EX1 ET1 ES ET2
syn keyword mcs51Register	EADC EA RXD TXD INT0 INT1 T0 T1 WR RD PX0 PT0 PX1 PT1 PS 
syn keyword mcs51Register	P F1 OV RS0 RS1 F0 AC CY

syn keyword perRegister	DPP PCON I2CDAT I2CADD T3CON T3FD TIMECON HTHSEC SEC MIN HOUR INTVAL DPCON
syn keyword perRegister	IEIP2 PWMCON CFG832 PWM0L PWM0H PWM1L PWM1H SPH ECON EDATA1 EDATA2 EDATA3
syn keyword perRegister	EDATA4 WDCON CHIPID EADRL EADRH T2CON RCAP2L RCAP2H TL2 TH2 DMAL DMAH DMAP
syn keyword perRegister	PLLCON ADCCON2 ADCDATAL ADCDATAH PSMCON DCON I2CCON ADCCON1 ADCOFSL ADCOFSH
syn keyword perRegister	ADCGAINL ADCGAINH ADCCON3 SPIDAT SPICON DAC0L DAC0H DAC1L DAC1H DACCON T2
syn keyword perRegister	T2EX PT2 PADC PSI WDWR WDE WDS WDIR PRE0 PRE1 PRE2 PRE3 CAP2 CNT2 TR2 EXEN2 TCLK
syn keyword perRegister	RCLK EXF2 TF2 CS0 CS1 CS2 CS3 SCONV CCONV DMA ADCI D0EN D0 D1EN D1 I2CI I2CTX
syn keyword perRegister	I2CRS I2CM MDI MCO MDE MDO SPR0 SPR1 CPHA CPOL SPIM SPE WCOL ISPI 

syn keyword mcs51Instr		ADD ADDC SUBB INC DEV MUL DIV MOV MOVC MOVX PUSH POP XCH XCHD
syn keyword mcs51Instr		ACALL LCALL RET RETI AJMP LJMP SJMP JMP JZ JNZ CJNE DJNZ NOP ANL
syn keyword mcs51Instr		ORL XRL CLR CPL RL RLC RR RRC SWAP SETB CPL JC JNC JB JNB JBC 

syn keyword mcs51Directive	DB EQU DATA IDATA XDATA BIT CODE DS DBIT DW ORG END CSEG XSEG DSEG ISEG

syn match	mcs51NumericOperator	"[+-/*]"
" numbers
syn match	mcs51Binary8Number	"\(#[01]\{8\}b\)\|\(\D[01]\{8\}b\)"
syn match	mcs51Binary16Number	"\(#[01]\{16\}b\)\|\(\D[01]\{16\}b\)"
" 00dh should not be recognised as a decimal number!!!
syn match	mcs51DecimalNumber	"\(#0\d*d\)\|\(\D0\d*d[^h]\)"
syn match	mcs51HexNumber		"\(#0\x*h\)\|\(\D0\x*h\)"

syn region	mcs51Comment		start=";" end="$" contains=cTodo
syn region	mcs51String		start="\"" end="\"\|$"
syn region	mcs51String		start="'" end="'\|$"
syn match	mcs51Label		"^[^$]\s*[^; \t]\+:"
syn match	mcs51Preprocess		"^\$\w\+\((.*)\)\?"		
syn match	mcs51Relative		"@R[0-7]\|@a\s*+\s*dptr\|@[ab]"

"syn match	mcs51wreg		"\([\s,]\+[ab][\s,]\+\)\|\([\s,]\+[Rr][0-7][\s,]\+\)"

hi def link	mcs51AddressSizes	type
hi def link	mcs51NumericOperator	mcs51Operator
hi def link	mcs51LogicalOperator	mcs51Operator
hi def link	mcs51Relative		mcs51Operator
hi def link	mcs51Relative1		mcs51Operator

hi def link	mcs51Binary8Number	mcs51Number
hi def link	mcs51Binary16Number	mcs51Number
hi def link	mcs51HexNumber		mcs51Number
hi def link	mcs51DecimalNumber	mcs51Number

hi def link	mcs51Register	type
hi def link	perRegister	type

hi def link	mcs51Preprocess		mcs51Directive
hi def link	mcs51Include		mcs51Directive

"  link to standard syn groups so the 'colorschemes' work:
hi def link	mcs51Operator operator
hi def link	mcs51Comment  comment
hi def link	mcs51Directive	preproc
hi def link	mcs51Number   number
"constant
hi def link	mcs51Symbol structure
hi def link	mcs51String  String
hi def link	mcs51Special 	comment
"special
hi def link	mcs51Instr keyword
hi def link	mcs51Label label
hi def link	mcs51Prefix preproc
hi def link	cTodo todo
hi def link	mcs51wreg identifier
hi def link	mcs51wreg1 identifier
hi def link	mcs51wreg2 identifier
hi def link	mcs51wreg3 identifier
hi def link	mcs51wreg4 identifier

let b:current_syntax = "mcs51a"
" vim: ts=8 sw=8 :
