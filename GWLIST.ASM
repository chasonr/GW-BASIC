; [ This translation created 10-Feb-83 by Version 4.3 ]

	.RADIX  8		; To be safe

CSEG	SEGMENT PUBLIC 'CODESG' 
	ASSUME  CS:CSEG

INCLUDE	BINTRP.H 

	TITLE   GWLIST Copied from BINTRP.MAC

	.RADIX	10

	.XLIST

FETOK=0				;For FE extended tokens
FDTOK=0				;For FD tokens too.  (Must have
				;FETOK==1.)
				;tokens.
INTDEX=0			;For Intelledex version.
NMPAGE=1			;Number of text pages (for GW
				;Multi-page)
				;KPOS, etc.
LNREAD=0			;For LINE READ statement
MELCO=0				;Mitsubishi Electronics Co.
SIRIUS=0
MCI=0
ZENITH=0			;ZENITH 8086
TETRA=0
CPM86=0
HAL=0
GENFLS=0
PANDBL=0
TSHIBA=0
SGS=0
ALPS=0
ALPCPM=0
GENWID=0
NNECBS=0
CAN8=0
PC8A=0

	FN2SW=0			;IBMTOK versions dispatch from IBMRES.MAC
	LABEL_=PC8A
	HLPEDT=PC8A

OKI=0
BUBL=0
	NORNF=0
IEESLV=0
TRSHHC=0
OLVPPC=0
NECPPC=0
USA=0				;For HHC-USA version
EUROPE=0			;For HHC-EUROPE version

	.LIST

;Local Switches
;
	LTRACE=ALPCPM		;trace output selectable
	LABEL_=PC8A
	HLPEDT=PC8A
	UCEMSG=NNECBS		;Upper case error messages.
	OLD86=MELCO AND CPM86	;For "old" 8086 error messages (prior
				;to alignment for IBM compatibility).

	OLDBLD=ALPCPM OR ALPS OR OKI OR HAL OR PC8A OR BUBL OR GW OR TSHIBA

	INCLUDE	GIO86U
	INCLUDE	MSDOSU		;MSDOS constants




	EXTRN	INIT:NEAR



	EXTRN	SETGSB:NEAR
DSEG	SEGMENT PUBLIC 'DATASG'
	ASSUME DS:DSEG
	EXTRN	ONGSBF:WORD
DSEG	ENDS


	BUFOFS=0

	BUFOFS=2		;MUST CRUNCH INTO ERALIER PLACE FOR
				; SINGLE QUOTE

	KBFLEN=BUFLEN+(BUFLEN/4)	;MAKE KRUNCH BUFFER SOMEWHAT
				; LARGER THAN SOURCE BUFFER (BUF)




	EXTRN	NAME_:NEAR
	EXTRN	INLIN:NEAR,CRDO:NEAR,CRDONZ:NEAR,STRCMP:NEAR,PPSWRT:NEAR

	EXTRN	OUTDO:NEAR

	EXTRN	BLTU:NEAR,CLEARC:NEAR,GTMPRT:NEAR,ISLET:NEAR,ISLET2:NEAR
	EXTRN	PTRGET:NEAR
	EXTRN	QINLIN:NEAR,SCRTCH:NEAR,STKINI:NEAR,RUNC:NEAR,RESFIN:NEAR
	EXTRN	PTRGT2:NEAR,STPEND:NEAR

	EXTRN	SYNCHR:NEAR

	EXTRN	SIGN:NEAR

	EXTRN	PRGFIN:NEAR,FILIND:NEAR

	EXTRN	FILINP:NEAR,INDSKC:NEAR


	EXTRN	LRUN:NEAR


	EXTRN	INXHRT:NEAR
	EXTRN	ZERO:NEAR,MOVE:NEAR,FOUT:NEAR,FIN:NEAR,PUSHF_:NEAR
	EXTRN	MOVFR:NEAR,MOVRF:NEAR,MOVRM:NEAR,INPRT:NEAR,LINPRT:NEAR
	EXTRN	MOVFM:NEAR,MOVMF:NEAR
	EXTRN	INRART:NEAR,NEG_:NEAR
	EXTRN	FREFAC:NEAR,FRETMS:NEAR,STRCPY:NEAR,GETSTK:NEAR
	EXTRN	STRLIT:NEAR,STRLT2:NEAR,STRLT3:NEAR,STRLTI:NEAR,STROUT:NEAR
	EXTRN	STRPRT:NEAR,STROUI:NEAR
	EXTRN	GETSPA:NEAR,PUTNEW:NEAR,STOP:NEAR,OMERR:NEAR,REASON:NEAR


	EXTRN	INSTR_:NEAR

	EXTRN	PRINUS:NEAR,PUTTMP:NEAR

	EXTRN	FOUTH:NEAR,FOUTO:NEAR,STRO$:NEAR,STRH$:NEAR


	EXTRN	STRNG$:NEAR
	EXTRN	TON:NEAR,TOFF:NEAR
	EXTRN	SPACE$:NEAR

	EXTRN	SIGNS:NEAR




	EXTRN	UMULT:NEAR

	EXTRN	SIGNC:NEAR,POPHRT:NEAR

	EXTRN	FINLPT:NEAR

	EXTRN	VMOVFA:NEAR,VMOVAF:NEAR,ISIGN:NEAR,VSIGN:NEAR,VDFACS:NEAR
	EXTRN	VMOVMF:NEAR,VMOVFM:NEAR,FRCINT:NEAR,FRCDBL:NEAR,FRCSNG:NEAR
	EXTRN	VNEG:NEAR,PUFOUT:NEAR,DCXBRT:NEAR,IADD:NEAR
	EXTRN	FINDBL:NEAR
	EXTRN	VMOVE:NEAR,VALINT:NEAR,VALSNG:NEAR,FRCSTR:NEAR,CHKSTR:NEAR
	EXTRN	MAKINT:NEAR
	EXTRN	MOVE1:NEAR

	EXTRN	SCNSEM:NEAR
	EXTRN	WHILE_:NEAR,WEND:NEAR
	EXTRN	CALLS:NEAR
	EXTRN	PROCHK:NEAR
	EXTRN	WRITE:NEAR
;The following block of externals was added on Dec 19, 1982 when BINTRP was
; Split up after the freeze of GW-BASIC Version 1.0
; This Split-up was not reflected in the PS1:<BASIC>BINTRP.MAC source.
; See Tom Corbett if you have any questions.
;
DSEG	SEGMENT PUBLIC 'DATASG'		
	EXTRN	MEMSIZ:WORD,FRETOP:WORD,VARTAB:WORD,STREND:WORD,TXTTAB:WORD
	EXTRN	ARYTAB:WORD
	EXTRN	CURLIN:WORD,DOT:WORD,DATLIN:WORD,NLONLY:WORD,ERRLIN:WORD
	EXTRN	ERRTXT:WORD
	EXTRN	MRGFLG:WORD,CHNFLG:WORD
	EXTRN	SAVSTK:WORD,SAVTXT:WORD,OLDLIN:WORD,NXTLIN:WORD,OLDTXT:WORD
	EXTRN	ONELIN:WORD,ONEFLG:WORD
	EXTRN	CNTOFL:WORD,TRCFLG:WORD,CONSAV:WORD,CONTXT:WORD,CONTYP:WORD
	EXTRN	NUMCON:WORD,CONLO:WORD
	EXTRN	AUTFLG:WORD,AUTLIN:WORD,AUTINC:WORD
	EXTRN	KBUF:WORD,BUFMIN:WORD,BUF:WORD
	EXTRN	PTRFIL:WORD,PTRFLG:WORD,DORES:WORD,VALTYP:WORD,FACLO:WORD
	EXTRN	DFACLO:WORD,FAC:WORD
	EXTRN	TEMP:WORD,TEMP2:WORD,TEMP3:WORD,TEMPA:WORD
	EXTRN	DSCTMP:WORD,TEMPST:WORD
	EXTRN	OPRTYP:WORD
	EXTRN	SUBFLG:WORD,FVALSV:WORD,DEFTBL:WORD,FLGINP:WORD,FLGSCN:WORD
	EXTRN	OVCSTR:WORD,INPPAS:WORD
	EXTRN	USRTAB:WORD,DONUM:WORD,ENDPRG:WORD,ENDFOR:WORD,DATPTR:WORD
	EXTRN	FLGOVC:WORD
	EXTRN	ERRFLG:WORD,SAVSEG:WORD,PRMLN2:WORD,PRMSIZ:WORD,PARM2:WORD
	EXTRN	PRMLEN:WORD,PRMSTK:WORD,PARM1:WORD
	EXTRN	FUNACT:WORD,NOFUNS:WORD,OPTVAL:WORD,OPTFLG:WORD,RNDX:WORD
DSEG	ENDS

	EXTRN	INEG2:NEAR,FADD_:NEAR
	EXTRN	$OVMSG:NEAR,ERRTAB:NEAR,LSTERR:NEAR,DSKERR:NEAR,NONDSK:NEAR
	EXTRN	REDDY:NEAR
	EXTRN	ERRSN:NEAR,ERRDV0:NEAR,ERRRE:NEAR,ERROV:NEAR,ERRMO:NEAR
	EXTRN	ERRTM:NEAR,ERRNF:NEAR
	EXTRN	ERRNR:NEAR,ERRLBO:NEAR,ERRDD:NEAR,ERRUF:NEAR,ERRFC:NEAR
	EXTRN	ERRIFN:NEAR,ERRFNO:NEAR,ERRDNA:NEAR,ERRFDR:NEAR,ERRRAD:NEAR
	EXTRN	ERRDFL:NEAR
	EXTRN	ERRIOE:NEAR,ERRBFM:NEAR,ERRFNF:NEAR,ERRBFN:NEAR,ERRIER:NEAR
	EXTRN	ERRRPE:NEAR
	EXTRN	ERRFAO:NEAR,ERRNMF:NEAR,ERRWH:NEAR,ERRBRN:NEAR,ERRFOV:NEAR
	EXTRN	ERRTMF:NEAR
	EXTRN	ERRFAE:NEAR,ERRUS:NEAR,ERRRG:NEAR,ERROD:NEAR,ERRFN:NEAR
	EXTRN	ERRUE1:NEAR	;ERRUE+DSKERR-NONDSK
	EXTRN	DSKER1:NEAR	;DSKERR-NONDSK

;The following externs are defined in GWMAIN.MAC
;
	EXTRN	CHRGTR:NEAR,READY:NEAR,STPRDY:NEAR,ISFLIO:NEAR,CONFC1:NEAR
	EXTRN	DEPTR:NEAR
	EXTRN	FINI:NEAR,FCERR:NEAR,FNDLIN:NEAR,SCNLIN:NEAR
	EXTRN	OCTCON:NEAR,HEXCON:NEAR,DBLCON:NEAR,DBLCN1:NEAR

;The following externs are defined in GWEVAL.MAC
;
	EXTRN	MAKUPL:NEAR,MAKUPS:NEAR,GETYPR:NEAR,OCTCNS:NEAR,DOCNVF:NEAR
	EXTRN	ISMID$:NEAR
	EXTRN	FRMEVL:NEAR,FRMCHK:NEAR,GETINT:NEAR,GETBYT:NEAR,GETIN2:NEAR
	EXTRN	SNGFLT:NEAR



	SUBTTL ROM VERSION INITALIZATION, AND CONSTANTS



;
; The reserved word tables are in another module.  Consequently
; many things must be declared external.  All of these things
; are in the code segement or are absolutes (like tokens).
; I.e., they are not in the data segment.
;

	EXTRN	ALPTAB:NEAR
	EXTRN	EQULTK:NEAR
	EXTRN	STMDSP:NEAR
	EXTRN	GREATK:NEAR
	EXTRN	INSRTK:NEAR
	EXTRN	LESSTK:NEAR,LSTOPK:NEAR
	EXTRN	MIDTK:NEAR,MINUTK:NEAR
	EXTRN	NMREL:NEAR,NOTTK:NEAR,NUMCMD:NEAR
	EXTRN	ONEFUN:NEAR
	EXTRN	POS:NEAR,PLUSTK:NEAR,PRINT:NEAR
	EXTRN	RESLST:NEAR
	EXTRN	SNGQTK:NEAR,SPCTAB:NEAR,SQRTK:NEAR,STEPTK:NEAR
	EXTRN	THENTK:NEAR
	EXTRN	USRTK:NEAR
	EXTRN	$DATA:NEAR,$DATCO:NEAR,$DELETE:NEAR
	EXTRN	$EDIT:NEAR,$ELSE:NEAR,$END:NEAR,$ERL:NEAR,$ERROR:NEAR
	EXTRN	$FN:NEAR,$FOR:NEAR
	EXTRN	$GOSUB:NEAR,$GOTO:NEAR
	EXTRN	$IF:NEAR,$INKEY$:NEAR,$INPUT:NEAR
	EXTRN	$LIST:NEAR,$LLIST:NEAR
	EXTRN	$NEXT:NEAR
	EXTRN	$POINT:NEAR,$PRINT:NEAR
	EXTRN	$REM:NEAR,$REMCO:NEAR,$RENUM:NEAR,$RESTORE:NEAR,$RESUME:NEAR
	EXTRN	$RETURN:NEAR,$RND:NEAR,$RUN:NEAR
	EXTRN	$SCREEN:NEAR,$STOP:NEAR,$STRING$:NEAR
	EXTRN	$THEN:NEAR,$TO:NEAR
	EXTRN	$USR:NEAR
	EXTRN	$VARPTR:NEAR
	EXTRN	$WEND:NEAR,$WHILE:NEAR

	SUBTTL EXTENDED LIST, DELETE, LLIST

PUBLIC	LLIST
LLIST:
	PUSH	BX
	MOV	BX,OFFSET 0-1
	MOV	PTRFIL,BX	;FDB pointer = LPT Pseudo FDB
	POP	BX
PUBLIC	LIST
LIST:

	POP	CX		;GET RID OF NEWSTT RETURN ADDR
	CALL	SCNLIN		;SCAN LINE RANGE
	PUSH	CX		;SAVE POINTER TO 1ST LINE
	CALL	PROCHK		;DONT EVEN LIST LINE #
	MOV	BX,TEMP		;Get Text pointer
	DEC	BX
	CALL	CHRGTR
	JZ	SHORT LIST4	;Brif LIST to CRT.
	CALL	SYNCHR
	DB	OFFSET 44D	; else must be ,"dev:"
	CALL	NAMSCN		;Crack File spec.
	EXTRN	NAMSCN:NEAR,NULOPM:NEAR
	MOV	AL,LOW OFFSET MD_SQO	;OPEN FILE 0 FOR OUTPUT
	CALL	NULOPM
LIST4:	MOV	BX,65535	;DONT ALLOW ^C TO CHANGE
	MOV	CURLIN,BX	;CONTINUE PARAMETERS
	POP	BX		;GET POINTER TO LINE
	POP	DX		;GET MAX LINE # OFF STACK
	MOV	CL,BYTE PTR [BX]	;[B,C]=THE LINK POINTING TO THE NEXT LINE
	INC	BX
	MOV	CH,BYTE PTR [BX]
	INC	BX
	MOV	AL,CH		;SEE IF END OF CHAIN
	OR	AL,CL
	JNZ	SHORT ??L000
	JMP	READY		;LAST LINE, STOP.  
??L000:
EXTRN	POLKEY:NEAR
	CALL	POLKEY		;check CTL C, queue typeahead
	PUSH	CX		;SAVE LINK
	MOV	CL,BYTE PTR [BX]	;PUSH THE LINE #
	INC	BX
	MOV	CH,BYTE PTR [BX]
	INC	BX
	PUSH	CX
	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;GET LINE # INTO [H,L]
	XCHG	BX,DX		;GET MAX LINE IN [H,L]
	CMP	BX,DX		;PAST LAST LINE IN RANGE?
	POP	CX		;TEXT POINTER TO [B,C]
	JAE	SHORT ??L001
	JMP	STPRDY		;IF PAST, THEN DONE LISTING.
??L001:
	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;SAVE MAX ON BOTTOM OF STACK
	PUSH	BX		;SAVE LINK ON TOP
	PUSH	CX		;SAVE TEXT POINTER BACK
	XCHG	BX,DX		;GET LINE # IN [H,L]
	MOV	DOT,BX		;SAVE FOR LATER EDIT OR LIST
				;AND WE WANT [H,L] ON THE STACK
	CALL	LINPRT		;PRINT AS INTEGER WITHOUT LEADING SPACE
	POP	BX
	MOV	AL,BYTE PTR [BX]	;GET BYTE FROM LINE
	CMP	AL,LOW 9	;IS IT A TAB?
	JZ	SHORT NOSPAL	;THEN DONT PRINT SPACE
	MOV	AL,LOW " "
	CALL	OUTDO		;PRINT A SPACE AFTER THE LINE #
NOSPAL:	CALL	BUFLIN		;UNPACK THE LINE INTO BUF
	MOV	BX,OFFSET BUF	;POINT AT THE START OF THE UNPACKED CHARACTERS
	CALL	LISPRT		;PRINT THE LINE
	CALL	CRDO		;PRINT CRLF
	JMP	SHORT LIST4	;GO BACK FOR NEXT LINE

PUBLIC	LISPRT
LISPRT:
	MOV	AL,BYTE PTR [BX]
	CMP	AL,LOW 10D
	JNZ	SHORT ??L002
	CALL	LISEOL		;erase to end-of-line after LF and 0
??L002:
	OR	AL,AL
	JNZ	SHORT ??L003
	CALL	LISEOL
??L003:
	JNZ	SHORT $+3
	RET			;IF =0 THEN END OF LINE
	EXTRN	OUTCH1:NEAR
	CALL	OUTCH1		;OUTPUT CHAR AND CHECK FOR LF
	INC	BX		;INCR POINTER
	JMP	SHORT LISPRT	;PRINT NEXT CHAR

;Routine to output sequence to Clear to end of line if current device
; is Standard Output (CRT)
; Exit - All registers preserved.
;
LISEOL:	LAHF			; PUSH PSW
	XCHG	AL,AH
	PUSH	AX
	XCHG	AL,AH
	CALL	ISFLIO
	JZ	SHORT ??L004
	JMP	LISELX		;return if listing to a file
??L004:
DSEG	SEGMENT PUBLIC 'DATASG'
	EXTRN	F_EDIT:WORD
DSEG	ENDS
	MOV	AL,BYTE PTR F_EDIT	;Save current edit mode
	LAHF			; PUSH PSW
	XCHG	AL,AH
	PUSH	AX
	XCHG	AL,AH
	MOV	AL,LOW 5	;05=erase to end-of-line for non-IBMLIK
	MOV	BYTE PTR F_EDIT,AL	;Enter edit mode(05 is control function)
	CALL	OUTDO		;output 05 to current device
	POP	AX		; POP PSW
	XCHG	AL,AH
	SAHF
	MOV	BYTE PTR F_EDIT,AL	;Restore edit mode
LISELX:	POP	AX		; POP PSW
	XCHG	AL,AH
	SAHF
	RET	

PUBLIC	BUFLIN
BUFLIN:	MOV	CX,OFFSET BUF	;GET START OF TEXT BUFFER
	MOV	DH,LOW OFFSET BUFLEN	;GET ITS LENGTH INTO [D]
	XOR	AL,AL		;SET MODE OF DECRUNCH
	MOV	BYTE PTR DORES,AL	;BIT0 IS QUOTE BIT1 IS DATA BIT2 IS REM
	XOR	AL,AL		;SET ON SPECIAL CHAR FOR SPACE INSERTION
	MOV	BYTE PTR TEMPA,AL
	CALL	PROCHK		;ONLY PROCEED IF OK
	JMP	SHORT PLOOP2	;START HERE

PLOOP:	INC	CX		;INCREMENT DEPOSIT PTR.
	INC	BX		;ADVANCE TEXT PTR
	DEC	DH		;BUMP DOWN COUNT
	JNZ	SHORT $+3
	RET			;IF BUFFER FULL, RETURN
PUBLIC	PLOOP2
PLOOP2:
	MOV	AL,BYTE PTR [BX]	;GET CHAR FROM BUF
	OR	AL,AL		;SET CC'S
	MOV	DI,CX
	STOSB			;SAVE THIS CHAR
	JNZ	SHORT $+3
	RET			;IF END OF SOURCE BUFFER, ALL DONE.
	CMP	AL,LOW OFFSET OCTCON	;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JB	SHORT NTEMBL	;YES, DONT TREAT AS ONE
	CMP	AL,LOW OFFSET DBLCN1	;IS IT EMBEDED CONSTANT?
	MOV	DL,AL		;SAVE CHAR IN [E]
	JB	SHORT PRTVAR	;PRINT LEADING SPACE IF NESC.
	CMP	AL,LOW 34D	;IS IT A QUOTATION
	JNZ	SHORT BFCHKC	;IF NOT CHECK FOR COLON
	MOV	AL,BYTE PTR DORES	;COMPLEMENT THE QUOTE BIT
	XOR	AL,LOW 1
	MOV	BYTE PTR DORES,AL
	MOV	AL,LOW 34D	;RESTORE THE CHARACTER
BFCHKC:	CMP	AL,LOW ":"	;IS IT A COLON ENDING DATA?
	JNZ	SHORT NTEMBL
	MOV	AL,BYTE PTR DORES	;DON'T END IF IN QUOTE
	RCR	AL,1
	JB	SHORT QTCOLN
	RCL	AL,1
	AND	AL,LOW 253D	;TURN OFF BIT1 (DATA BIT)
	MOV	BYTE PTR DORES,AL
QTCOLN:	MOV	AL,LOW ":"
NTEMBL:	OR	AL,AL		;SET CC'S
	JNS	SHORT ??L005
	JMP	PLOOPR		;RESERVED WORD OF SOME KIND
??L005:
	MOV	DL,AL		;SAVE CHAR IN [E]
	CMP	AL,LOW "."	;DOT IS PART OF VAR NAME
	JZ	SHORT PRTVAR
	CALL	TSTANM		;IS CHAR ALPHANUMERIC
	JAE	SHORT PRTVAR	;ALPHANUMERIC
	XOR	AL,AL		;MAKE SPECIAL
	JMP	SHORT PLOOPH
PRTVAR:	MOV	AL,BYTE PTR TEMPA	;WHAT DID WE DO LAST?
	OR	AL,AL		;SET CONDITION CODES
	JZ	SHORT PLOOPG	;SPECIAL, NEVER INSERT SPACE
	INC	AL		;IN RESERVED WORD?
	JNZ	SHORT PLOOPG	;NO
	MOV	AL,LOW " "	;PUT OUT SPACE BEFORE RESWORD
	MOV	DI,CX
	STOSB			;STORE IN BUFFER
	INC	CX		;INCRMENT POINTER INTO BUFFER
	DEC	DH		;SPACE LEFT?
	JNZ	SHORT $+3
	RET			;NO, DONE
PLOOPG:	MOV	AL,LOW 1	;STORE FLAG SAYING IN VAR
PLOOPH:	MOV	BYTE PTR TEMPA,AL
	MOV	AL,DL		;GET BACK CHAR WE HAD
	CMP	AL,LOW OFFSET OCTCON	;IS IT SMALLER THAN SMALLEST EMBEDDED CONSTANT?
	JB	SHORT PLOOPZ	;YES, DONT TREAT AS ONE
	CMP	AL,LOW OFFSET DBLCN1	;IS IT EMBEDED CONSTANT?
	JAE	SHORT ??L006
	JMP	NUMLIN		;YES, UNPACK IT
??L006:
PLOOPZ:	MOV	DI,CX
	STOSB			;MAKE SURE BYTE STORED AFTER SPACE
	JMP	PLOOP		;STORE IN BUFFER
PLOOPR:
	MOV	AL,BYTE PTR DORES	;SEEWHAT OUR UNCRUNCH MODE IS
	RCR	AL,1		;THE LSB IS THE QUOTE BIT
	JB	SHORT GPLOOP
	RCR	AL,1		;GET THE REM BIT
	RCR	AL,1		;AND SEE IF SET
	JAE	SHORT CHKDRS	;IF NOT JUST CHECK DATA BIT
	MOV	AL,BYTE PTR [BX]	;MUST SEE IF ITS SNGQTK
	CMP	AL,LOW OFFSET SNGQTK	;AND PRECEDED BY ":REM"
	PUSH	BX
	PUSH	CX		;SAVE BUFFER POINTER
	MOV	BX,OFFSET NOSNGQ	;PLACE TO RETURN ON FAILURE
	PUSH	BX
	JZ	SHORT $+3
	RET
	DEC	CX
	MOV	SI,CX
	MOV	AL,[SI]
	CMP	AL,LOW "M"
	JZ	SHORT $+3
	RET
	DEC	CX
	MOV	SI,CX
	MOV	AL,[SI]
	CMP	AL,LOW "E"
	JZ	SHORT $+3
	RET
	DEC	CX
	MOV	SI,CX
	MOV	AL,[SI]
	CMP	AL,LOW "R"
	JZ	SHORT $+3
	RET
	DEC	CX
	MOV	SI,CX
	MOV	AL,[SI]
	CMP	AL,LOW ":"
	JZ	SHORT $+3
	RET
	POP	AX		;GET RID OF RETURN ON FAIL ADDRESS
	POP	AX		;GET RID OF BAD BUFFER POINTER
	POP	BX		;GET BACK POINTER INTO LINE
	INC	DH		;UPDATE CHAR COUNT
	INC	DH
	INC	DH
	INC	DH
	JMP	SHORT RESEXP

NOSNGQ:	POP	CX		;GET BACK THE BUFFERPOINTER
	POP	BX		;GET BACK SOURCE LINE POINTER
	MOV	AL,BYTE PTR [BX]	;GET BACK THE CHARACTER
GPLOOP:	JMP	PLOOP

DATSET:	MOV	AL,BYTE PTR DORES	;BIT INDICATING INSIDE DATA
	OR	AL,LOW 2	;IS BIT1
SETDRS:	MOV	BYTE PTR DORES,AL
	XOR	AL,AL
	RET	

REMSET:	MOV	AL,BYTE PTR DORES
	OR	AL,LOW 4
	JMP	SHORT SETDRS

CHKDRS:	RCL	AL,1		;GET DATA BIT INTO CARRY
	JB	SHORT GPLOOP
	MOV	AL,BYTE PTR [BX]	;GET BACK THE CHARACTER
	CMP	AL,LOW OFFSET $DATA	;NEED TO SET A BIT
	JNZ	SHORT ??L007
	CALL	DATSET
??L007:
	CMP	AL,LOW OFFSET $REM
	JNZ	SHORT ??L008
	CALL	REMSET
??L008:
RESEXP:	MOV	AL,BYTE PTR [BX]

	INC	AL		;SET ZERO IF FN TOKEN
	MOV	AL,BYTE PTR [BX]	;GET CHAR BACK
	JNZ	SHORT NTFNTK	;NOT FUNCTION JUST TREAT NORMALLY
	INC	BX		;BUMP POINTER
	MOV	AL,BYTE PTR [BX]	;GET CHAR
	AND	AL,LOW 177O	;TURN OFF HIGH BIT
NTFNTK:	INC	BX		;ADVANCE TO POINT AFTER
NTQTTK:	CMP	AL,LOW OFFSET $ELSE	;ELSE?
	JNZ	SHORT NOTELS
	DEC	CX
	INC	DH
NOTELS:
	CMP	AL,LOW OFFSET $WHILE	;MIGHT HAVE AN EXTRA "+" IN WHILE FORMULA
	JNZ	SHORT BFNTWH	;SO SKIP OVER IT IF ITS THERE
	MOV	AL,BYTE PTR [BX]	;GET CHARACTER TO SEE IF ITS PLUSTK
	INC	BX		;ASSUME IS PLUSTK
	CMP	AL,LOW OFFSET PLUSTK	;MIGHT NOT BE PLUS IF BINARY SAVED IN
	MOV	AL,LOW OFFSET $WHILE	;RESTORE TOKEN VALUE
	JZ	SHORT BFNTWH	;VERSION OF BASIC BEFORE CRUNCH CHANGED
	DEC	BX		;MOVE POINTER BACK
BFNTWH:

	PUSH	BX		;SAVE TEXT PTR.
	PUSH	CX		;SAVE DEPOSIT PTR.
	PUSH	DX		;SAVE CHAR COUNT.
	EXTRN	LISTX:NEAR
	CALL	LISTX		;Handle extended reserved words.
	MOV	BX,OFFSET RESLST-1	;GET PTR TO START OF RESERVED WORD LIST
	MOV	CH,AL		;SAVE THIS CHAR IN [B]
	MOV	CL,LOW OFFSET "A"-1	;INIT LEADING CHAR VALUE
RESSR3:	INC	CL		;BUMP LEADING CHAR VALUE.
RESSR1:	INC	BX		;BUMP POINTER INTO RESLST
RESSRC:	MOV	DH,BH		;SAVE PTR TO START OF THIS RESWRD
	MOV	DL,BL
RESSR2:
	MOV	AL,BYTE PTR CS:[BX]	;GET CHAR FROM RESLST
	OR	AL,AL		;SET CC'S
	JZ	SHORT RESSR3	;IF END OF THIS CHARS TABLE, GO BACK & BUMP C
	LAHF
	INC	BX		;BUMP SOURCE PTR
	SAHF
	JS	SHORT ??L009
	JMP	RESSR2		;IF NOT END OF THIS RESWRD, THEN KEEP LOOKING
??L009:
	MOV	AL,BYTE PTR CS:[BX]	;GET PTR TO RESERVED WORD VALUE
	CMP	AL,CH		;SAME AS THE ONE WE SEARCH FOR?
	JNZ	SHORT RESSR1	;NO, KEEP LOOKING.
	XCHG	BX,DX		;SAVE FOUND PTR IN [H,L]
	CMP	AL,LOW OFFSET $USR	;USR FUNCTION TOKEN?
	JZ	SHORT NOISPA	;DONT INSERT SPACE
	CMP	AL,LOW OFFSET $FN	;IS IT FUNCTION TOKEN?
NOISPA:
	MOV	AL,CL		;GET LEADING CHAR
	POP	DX		;RESTORE LINE CHAR COUNT
	POP	CX		;RESTORE DEPOSIT PTR
	MOV	DL,AL		;SAVE LEADING CHAR
	JNZ	SHORT NTFNEX	;NOT "FN" EXPANSION
	MOV	AL,BYTE PTR TEMPA	;SET CC'S ON TEMPA
	OR	AL,AL
	MOV	AL,LOW 0	;CLEAR RESWRD FLAG - MARK AS SPECIAL
	MOV	BYTE PTR TEMPA,AL	;SET FLAG
	JMP	SHORT MORLNZ	;DO EXPANSION
NTFNEX:
	CMP	AL,LOW OFFSET "Z"+1	;WAS IT A SPECIAL CHAR?
	JNZ	SHORT NTSPCH	;NON-SPECIAL CHAR
	XOR	AL,AL		;SET NON-SPECIAL
	MOV	BYTE PTR TEMPA,AL
	JMP	SHORT MORPUR	;PRINT IT
NTSPCH:	MOV	AL,BYTE PTR TEMPA	;WHAT DID WE DO LAST?
	OR	AL,AL		;SPECIAL?
	MOV	AL,LOW 255	;FLAG IN RESERVED WORD
	MOV	BYTE PTR TEMPA,AL	;CLEAR FLAG
MORLNZ:	JZ	SHORT MORLN0	;GET CHAR AND PROCEED
	MOV	AL,LOW " "	;PUT SPACE IN BUFFER
	MOV	DI,CX
	STOSB
	INC	CX
	DEC	DH		;ANY SPACE LEFT IN BUFFER
	JNZ	SHORT ??L010
	JMP	PPSWRT		;NO, RETURN
??L010:
MORLN0:	MOV	AL,DL
	JMP	SHORT MORLN1	;CONTINUE
MORPUR:
	MOV	AL,BYTE PTR CS:[BX]	;GET BYTE FROM RESWRD
	INC	BX		;BUMP POINTER
MORLNP:	MOV	DL,AL		;SAVE CHAR
MORLN1:	AND	AL,LOW 177O	;AND OFF HIGH ORDER BIT FOR DISK & EDIT
	MOV	DI,CX
	STOSB			;STORE THIS CHAR
	INC	CX		;BUMP PTR
	DEC	DH		;BUMP DOWN REMAINING CHAR COUNT
	JNZ	SHORT ??L011
	JMP	PPSWRT		;IF END OF LINE, JUST RETURN
??L011:
	OR	AL,DL		;SET CC'S
	JS	SHORT ??L012
	JMP	MORPUR		;END OF RESWRD?
??L012:
	CMP	AL,LOW OFFSET "("+128	;SPC( OR TAB( ?
	JNZ	SHORT NTSPCT	;NO
	XOR	AL,AL		;CLEAR FLAG
	MOV	BYTE PTR TEMPA,AL	;TO INSERT SPACE AFTERWARDS
NTSPCT:
	POP	BX		;RESTORE SOURCE PTR.
	JMP	PLOOP2		;GET NEXT CHAR FROM LINE
PUBLIC	TSTANM
TSTANM:	CALL	ISLET2		;LETTER?
	JNAE	SHORT $+3
	RET			;YES
	CMP	AL,LOW "0"	;DIGIT?
	JNB	SHORT $+3
	RET			;TOO SMALL
	CMP	AL,LOW OFFSET "9"+1	;LAST DIGIT
	CMC			;MAKE CARRY RIGHT
	RET			;NO CARRY=DIGIT
NUMLIN:	DEC	BX		;MOVE POINTER BACK AS CHRGET INX'S
	CALL	CHRGTR		;SCAN THE CONSTANT
	PUSH	DX		;SAVE CHAR COUNT
	PUSH	CX		;SAVE DEPOSIT PTR
	PUSH	AX		;SAVE CONSTANT TYPE.
	CALL	CONFC1		;MOVE CONSTANT INTO FAC
	POP	AX		;RESTORE CONSTANT TYPE
	MOV	CX,OFFSET CONLIN	;PUT RETURN ADDR ON STACK
	PUSH	CX		;SAVE IT
	CMP	AL,LOW OFFSET OCTCON	;OCTAL CONSTANT?
	JNZ	SHORT ??L013
	JMP	FOUTO		;PRINT IT
??L013:
	CMP	AL,LOW OFFSET HEXCON	;HEX CONSTANT?
	JNZ	SHORT ??L014
	JMP	FOUTH		;PRINT IN HEX
??L014:
	MOV	BX,CONLO	;GET LINE # VALUE IF ONE.
	JMP	FOUT		;PRINT REMAINING POSSIBILITIES.
CONLIN:	POP	CX		;RESTORE DEPOSIT PTR.
	POP	DX		;RESTORE CHAR COUNT
	MOV	AL,BYTE PTR CONSAV	;GET SAVED CONSTANT TOKEN
	MOV	DL,LOW "O"	;ASSUME OCTAL CONSTANT
	CMP	AL,LOW OFFSET OCTCON	;OCTAL CONSTANT?
	JZ	SHORT SAVBAS	;YES, PRINT IT
	CMP	AL,LOW OFFSET HEXCON	;HEX CONSTANT?
	MOV	DL,LOW "H"	;ASSUME SO.
	JNZ	SHORT NUMSLN	;NOT BASE CONSTANT
SAVBAS:
	MOV	AL,LOW "&"	;PRINT LEADING BASE INDICATOR
	MOV	DI,CX
	STOSB			;SAVE IT
	INC	CX		;BUMP PTR
	DEC	DH		;BUMP DOWN CHAR COUNT
	JNZ	SHORT $+3
	RET			;RETURN IF END OF BUFFER
	MOV	AL,DL		;GET BASE CHAR
	MOV	DI,CX
	STOSB			;SAVE IT
	INC	CX		;BUMP PTR
	DEC	DH		;BUMP DOWN BASE COUNT
	JNZ	SHORT $+3
	RET			;END OF BUFFER, DONE
NUMSLN:
	MOV	AL,BYTE PTR CONTYP	;GET TYPE OF CONSTANT WE ARE
	CMP	AL,LOW 4	;IS IT SINGLE OR DOUBLE PREC?
	MOV	DL,LOW 0	;NO, NEVER PRINT TRAILING TYPE INDICATOR
	JB	SHORT TYPSET
	MOV	DL,LOW "!"	;ASSUME SINGLE PREC.
	JZ	SHORT TYPSET	;IS CONTYP=4, WAS SINGLE
	MOV	DL,LOW "#"	;DOUBLE PREC INDICATOR
TYPSET:
	MOV	AL,BYTE PTR [BX]	;GET LEADING CHAR
	CMP	AL,LOW " "	;LEADING SPACE
	JNZ	SHORT ??L015
	CALL	INXHRT		;GO BY IT
??L015:
NUMSL2:	MOV	AL,BYTE PTR [BX]	;GET CHAR FROM NUMBER BUFFER
	INC	BX		;BUMP POINTER
	OR	AL,AL		;SET CC'S
	JZ	SHORT NUMDN	;IF ZERO, ALL DONE.
	MOV	DI,CX
	STOSB			;SAVE CHAR IN BUF.
	INC	CX		;BUMP PTR
	DEC	DH		;SEE IF END OF BUFFER
	JNZ	SHORT $+3
	RET			;IF END OF BUFFER, RETURN
	MOV	AL,BYTE PTR CONTYP	;GET TYPE OF CONSTANT TO BE PRINTED
	CMP	AL,LOW 4	;TEST FOR SINGLE OR DOUBLE PRECISION
	JB	SHORT NUMSL2	;NO, WAS INTEGER
	LAHF
	DEC	CX		;PICK UP SAVED CHAR
	SAHF
	MOV	SI,CX
	MOV	AL,[SI]		;EASIER THAN PUSHING ON STACK
	LAHF
	INC	CX		;RESTORE TO POINT WHERE IT SHOULD
	SAHF
	JNZ	SHORT DBLSCN	;IF DOUBLE, DONT TEST FOR EMBEDED "."
	CMP	AL,LOW "."	;TEST FOR FRACTION
	JZ	SHORT ZERE	;IF SINGLE & EMBEDED ., THEN DONT PRINT !
DBLSCN:	CMP	AL,LOW "D"	;DOUBLE PREC. EXPONENT?
	JZ	SHORT ZERE	;YES, MARK NO VALUE TYPE INDICATOR NESC.
	CMP	AL,LOW "E"	;SINGLE PREC. EXPONENT?
	JNZ	SHORT NUMSL2	;NO, PROCEED
ZERE:	MOV	DL,LOW 0	;MARK NO PRINTING OF TYPE INDICATOR
	JMP	SHORT NUMSL2	;KEEP MOVING NUMBER CHARS INTO BUF

NUMDN:
	MOV	AL,DL		;GET FLAG TO INDICATE WHETHER TO INSERT
	OR	AL,AL		;A "D" AFTER DOUBLE PREC. #
	JZ	SHORT NOD	;NO, DONT INSERT IT
	MOV	DI,CX
	STOSB			;SAVE IN BUFFER
	INC	CX		;BUMP POINTER
	DEC	DH		;DECRMENT COUNT OF CHARS LEFT IN BUFFER
	JNZ	SHORT $+3
	RET			;=0, MUST TRUNCATE LIST OF THIS LINE.
NOD:
	MOV	BX,CONTXT	;GET BACK TEXT POINTER AFTER CONSTANT
	JMP	PLOOP2		;GET NEXT CHAR


;
; THE FOLLOWING CODE IS FOR THE DELETE RANGE
; COMMAND. BEFORE THE LINES ARE DELETED, 'OK'
; IS TYPED.
;
PUBLIC	DELETE
DELETE:
	CALL	SCNLIN		;SCAN LINE RANGE
	PUSH	CX
	CALL	DEPTR		;CHANGE POINTERS BACK TO NUMBERS
	POP	CX
	POP	DX		;POP MAX LINE OFF STACK
	PUSH	CX		;SAVE POINTER TO START OF DELETION
				;FOR USE BY CHEAD AFTER FINI
	PUSH	CX		;SAVE POINTER TO START OF 1ST LINE
	CALL	FNDLIN		;FIND THE LAST LINE
	JAE	SHORT FCERRG	;MUST HAVE A MATCH ON THE UPPER BOUND
	MOV	DH,BH		;[D,E]=POINTER AT THE START OF THE LINE
	MOV	DL,BL		;BEYOND THE LAST LINE IN THE RANGE
	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;SAVE THE POINTER TO THE NEXT LINE
	PUSH	BX		;SAVE THE POINTER TO THE START OF
				;THE FIRST LINE IN THE RANGE
	CMP	BX,DX		;MAKE SURE THE START COMES BEFORE THE END
FCERRG:	JNAE	SHORT ??L016
	JMP	FCERR		;IF NOT, "ILLEGAL FUNCTION CALL"
??L016:
				;DELETE must now be at EOL
	EXTRN	EOSCHK:NEAR
	MOV	BX,TEMP		;Get text pointer
	CALL	EOSCHK		;Check for end of statement
	MOV	BX,OFFSET REDDY	;PRINT "OK" PREMATURELY
	CALL	STROUT
	POP	CX		;GET POINTER TO FIRST IN [B,C]
	MOV	BX,OFFSET FINI	;GO BACK TO FINI WHEN DONE
	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;[H,L]=POINTER TO THE NEXT LINE
; ERASE A LINE FROM MEMORY
; [B,C]=START OF LINE BEING DELETED
; [H,L]=START OF NEXT LINE
;
PUBLIC	DEL
DEL:	XCHG	BX,DX		;[D,E] NOW HAVE THE POINTER TO THE LINE
				;BEYOND THIS ONE
	MOV	BX,VARTAB	;COMPACTIFYING TO VARTAB
MLOOP:	MOV	SI,DX
	MOV	AL,[SI]
	MOV	DI,CX
	STOSB			;SHOVING DOWN TO ELIMINATE A LINE
	INC	CX
	INC	DX
	CMP	BX,DX		;DONE COMPACTIFYING?
	JNZ	SHORT MLOOP	;NO
	MOV	BX,CX
;Clear SCALARS and ARRAYS incase we are replacing line which may force
; garbage collection in BLTU
	MOV	VARTAB,BX
	MOV	ARYTAB,BX
	MOV	STREND,BX
	RET	
	PAGE	


CSEG	ENDS
	END
