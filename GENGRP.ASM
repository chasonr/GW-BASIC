; [ This translation created 10-Feb-83 by Version 4.3 ]

	.RADIX  8		; To be safe

CSEG	SEGMENT PUBLIC 'CODESG' 
	ASSUME  CS:CSEG

INCLUDE	OEM.H

	TITLE   GENGRP  GENERALIZED GRAPHICS    /WHG

	.RADIX	10

CLIPPL=0
WINDOW=0
VIEW=0
TILE=0
HAL=0
PC8A=0
MODEL3=0
TSHIBA=0
ALPCPM=0
HALL4=0
	CLIPPL=PC8A OR TRSER2 OR MODEL3
	WINDOW=PC8A
	VIEW=PC8A OR TRSER2 OR MODEL3
	LINEST=0		;LINE STYLE SWITCH
	LINEST=PC8A OR TRSER2 OR MODEL3
;
;       SYSTEM DEPENDENT SWITCHES:
;

COMMENT	*

THESE ARE THE GENERAL GRAPHICS ROUTINES THAT ASSUME THE "MACHINE
INDEPENDENT" GRAPHICS INTERFACE. THEY DEAL WITHIN A  16-BIT
GRAPHICS COORDINATE SYSTEM FOR BOTH X AND Y. ATTRIBUTES RANGE FROM
0 TO 255.

*
;
; THESE ARE THE RAM LOCATIONS REQUIRED TO SUPPORT THIS PACKAGE
; NOTE THAT ALL OF THESE ARE LOCAL TO THIS CODE EXCEPT
; FORCLR AND BAKCLR WHICH NEED TO BE SET UP BY THE MACHINE DEPENDENT
; CODE EITHER AS CONSTANTS OR THROUGH A COLOR TYPE COMMAND
; NOTE THAT MAXUPD AND MINUPD ARE 3 BYTE CELLS WITH A JMP IN THE
; FIRST BYTE TO BE SET UP BY GRFINI OR INITIALIZED AT STARTUP
;
				;In GW versions, GETFBC is the OEM interface
	EXTRN	GETFBC:NEAR	;for FORCLR and BAKCLR (OEM vars)
	EXTRN	PIXSIZ:NEAR	;Get bits/pixel (0=no graphics available)
DSEG	SEGMENT PUBLIC 'DATASG'
	ASSUME DS:DSEG
	EXTRN	GXPOS:WORD,GYPOS:WORD
DSEG	ENDS
DSEG	SEGMENT PUBLIC 'DATASG'
	EXTRN	GRPACX:WORD,GRPACY:WORD
DSEG	ENDS
DSEG	SEGMENT PUBLIC 'DATASG'
	EXTRN	MAXDEL:WORD,MINDEL:WORD,MINUPD:WORD,MAXUPD:WORD
DSEG	ENDS
DSEG	SEGMENT PUBLIC 'DATASG'
	EXTRN	ATRBYT:WORD	;External form of the current attribute
DSEG	ENDS
				; used by PAINT for default
;
; THESE ARE THE ROUTINES CALLED INSIDE THE BASIC INTERPRETER
; AND THE SYNTAX TOKEN DEFINITIONS REFERENCED
;
	EXTRN	GETIN2:NEAR,GETBYT:NEAR,CHRGTR:NEAR,$STEP:NEAR,FCERR:NEAR
	EXTRN	CHRGT2:NEAR
	EXTRN	MINUTK:NEAR
	EXTRN	SYNCHR:NEAR
;
	PUBLIC	HLFDE,SCAND,ATRSCN,SCAN1,DOGRPH,XCHGX,XCHGY,XDELT,YDELT
; THESE ARE THE ENTRY POINTS INTO THE MACHINE DEPENDENT GRAPHICS
; ROUTINES. THEY FOLLOW STANDARD CONVENTIONS TO AVOID MODIFICATIONS
; TO CODE IN THIS PACKAGE FOR SPECIFIC IMPLEMENTATIONS
;
	EXTRN	SCALXY:NEAR,MAPXYC:NEAR,UPC:NEAR,DOWNC:NEAR,LEFTC:NEAR
	EXTRN	RIGHTC:NEAR
	EXTRN	READC:NEAR,SETATR:NEAR,NSETCX:NEAR
	EXTRN	FETCHC:NEAR,STOREC:NEAR,SETC_:NEAR

	PAGE	
	SUBTTL  SCAN A COORDINATE - SCAN1 AND SCAND

;
; ALLOW A COORDINATE OF THE FORM (X,Y) OR STEP(X,Y)
; THE LATTER IS RELATIVE TO THE GRAPHICS AC.
; THE GRAPHICS AC IS UPDATED WITH THE NEW VALUE
; RESULT IS RETURNED WITH [B,C]=X AND [D,E]=Y
; CALL SCAN1 TO GET FIRST IN A SET OF TWO PAIRS SINCE IT ALLOWS
; A NULL ARGUMENT TO IMPLY THE CURRENT AC VALUE AND
; IT WILL SKIP A "@" IF ONE IS PRESENT
;
SCAN1:	MOV	AL,BYTE PTR [BX]	;GET THE CURRENT CHARACTER
	CMP	AL,LOW "@"	;ALLOW MEANINGLESS "@"
	JNZ	SHORT ??L000
	CALL	CHRGTR		;BY SKIPPING OVER IT
??L000:
	MOV	CX,0		;ASSUME NO COODINATES AT ALL (-SECOND)
	MOV	DH,CH
	MOV	DL,CL
	CMP	AL,LOW OFFSET MINUTK	;SEE IF ITS SAME AS PREVIOUS
	JZ	SHORT SCANN	;USE GRAPHICS ACCUMULATOR
;
; THE STANDARD ENTRY POINT
;
SCAND:	MOV	AL,BYTE PTR [BX]	;GET THE CURRENT CHARACTER
	CMP	AL,LOW OFFSET $STEP	;IS IT RELATIVE?
	PUSHF			;REMEMBER
	JNZ	SHORT ??L001
	CALL	CHRGTR		;SKIP OVER $STEP TOKEN
??L001:
	CALL	SYNCHR
	DB	OFFSET "("	;SKIP OVER OPEN PAREN
	CALL	GETIN2		;SCAN X INTO [D,E]
	PUSH	DX		;SAVE WHILE SCANNING Y
	CALL	SYNCHR
	DB	OFFSET 44D	;SCAN COMMA
	CALL	GETIN2		;GET Y INTO [D,E]
	CALL	SYNCHR
	DB	OFFSET ")"
	POP	CX		;GET BACK X INTO [B,C]
	POPF			;RECALL IF RELATIVE OR NOT
SCANN:	PUSH	BX		;SAVE TEXT POINTER
	MOV	BX,GRPACX	;GET OLD POSITION
	JZ	SHORT SCXREL	;IF ZERO,RELATIVE SO USE OLD BASE
	MOV	BX,0		;IN ABSOLUTE CASE, JUST Y USE ARGEUMENT
SCXREL:	LAHF
	ADD	BX,CX		;ADD NEW VALUE
	RCR	SI,1
	SAHF
	RCL	SI,1
	MOV	GRPACX,BX	;UPDATE GRAPHICS ACCUMLATOR
	MOV	GXPOS,BX	;STORE SECOND COORDINTE FOR CALLER
	MOV	CX,BX		;RETURN X IN BC
	MOV	BX,GRPACY	;GET OLDY POSITION
	JZ	SHORT SCYREL	;IF ZERO, RELATIVE SO USE OLD BASE
	MOV	BX,0		;ABSOLUTE SO OFFSET BY 0
SCYREL:	ADD	BX,DX
	MOV	GRPACY,BX	;UPDATE Y PART OF ACCUMULATOR
	MOV	GYPOS,BX	;STORE Y FOR CALLER
	XCHG	BX,DX		;RETURN Y IN [D,E]
	POP	BX		;GET BACK THE TEXT POINTER
	RET	
	PAGE	
	SUBTTL  PSET,PRESET,POINT
;
; THESE ARE THE STATEMENT AND FUNCTION ENTRY POINTS DEFINED BY THIS
; PACKAGE. THE APPROPRIATE ENTRIES MUST BE SELECTED IN THE
; RESERVED WORD TABLES TO GET BASIC TO DISPATCH TO THESE ROUTINES
;
	PUBLIC	PSET,PRESET,POINT

;
; PSET (X,Y)[,ATTRIBUTE] DEFAULT ATTRIBUTE TO FORCLR
; PRESET (X,Y)[,ATTRIBUTE] DEFAULT ATTIBUTE TO BAKCLR
;
PRESET:	PUSH	BX		;Push text pointer
	STC			;Flag to get graphics colors
	CALL	GETFBC		;Get forground/background colors
	MOV	AL,BL		;Get background color
	JMP	PPRSET

PSET:	PUSH	BX		;Push text pointer
	STC			;Flag to get graphics colors
	CALL	GETFBC		;Get forground/background colors
PPRSET:	POP	BX		;Retrieve text pointer
PSETC:	PUSH	AX		;SAVE DEFAULT ATTRIBUTE
	CALL	SCAND		;SCAN A SINGLE COORDINATE
	POP	AX		;GET BACK DEFAULT ATTRIBUTE
	CALL	ATRENT		;SCAN POSSIBLE ATTRIBUTE
	PUSH	BX		;SAVE TEXT POINTER
	CALL	SCALXY		;SCALE INTO BOUNDS
	JAE	SHORT PSTNOT	;NO PSET IF NOT IN BOUNDS
	CALL	MAPXYC		;MAP INTO A "C"
	CALL	SETC_		;ACTUALLY DO THE SET
PSTNOT:	POP	BX
	RET	

;
; POINT (X,Y) RETURNS THE ATTRIBUTE STORED AT THAT POINT
; IT RETURNS -1 IF THE POINT IS OUT OF BOUNDS
;
POINT:
	CALL	CHRGTR		;POINT IS RECOGNIZED IN EVAL
				;SO NEED TO SKIP ONE MORE CHAR
	PUSH	BX		;Save the text pointer.
	CALL	FETCHC		;Preserve the graphics cursor, GXPOS,
	POP	DX		;and GYPOS across the POINT function
	PUSH	BX		;so cases like
	PUSH	AX		;LINE (x1,y1)-(x2,y2),POINT(x3,y3) will
	MOV	BX,GYPOS	;work correctly.
	PUSH	BX
	MOV	BX,GXPOS
	PUSH	BX
	MOV	BX,GRPACY
	PUSH	BX
	MOV	BX,GRPACX
	PUSH	BX
	XCHG	BX,DX		;Put the text pointer back in HL.
	CALL	SCAND		;READ THE SPECIFICATION OF THE POINT
	PUSH	BX		;SAVE THE TEXT POINTER
	CALL	SCALXY		;SCALE THE POINT
	MOV	BX,OFFSET 0-1	;ASSUME ILLEGAL POINT
	JAE	SHORT PNTNOT	;NOT LEGAL - RETURN -1
	CALL	MAPXYC
	CALL	READC		;READ OUT THE ATTRIBUTE
	MOV	BL,AL
	MOV	BH,LOW 0
	EXTRN	MAKINT:NEAR
PNTNOT:	CALL	MAKINT
	POP	DX		;Restore text pointer
	POP	BX
	MOV	GRPACX,BX
	POP	BX
	MOV	GRPACY,BX
	POP	BX		;Restore GXPOS, GYPOS, and the graphics
	MOV	GXPOS,BX	;cursor.
	POP	BX
	MOV	GYPOS,BX
	POP	AX
	POP	BX
	PUSH	DX
	CALL	STOREC
	POP	BX		;Retrieve the text pointer and return.
	RET	
;
; ATTRIBUTE SCAN
; LOOK AT THE CURRENT POSITION AND IF THERE IS AN ARGUMENT READ IT AS
; THE 8-BIT ATTRIBUTE VALUE TO SEND TO SETATR. IF STATEMENT HAS ENDED
; OR THERE IS A NULL ARGUMENT, SEND FORCLR  TO SETATR
;
ATRSCN:	PUSH	BX		;Save text pointer
	STC			;Flag to get graphics colors
	CALL	GETFBC		;Get forground/background colors
	POP	BX		;Retrieve text pointer
ATRENT:	PUSH	CX		;SAVE THE CURRENT POINT
	PUSH	DX
	MOV	DL,AL		;SAVE DEFAULT ATTRIBUTE IN [E]
	DEC	BX		;SEE IF STATEMENT ENDED
	CALL	CHRGTR
	JZ	SHORT ATRFIN	;USE DEFAULT
	CALL	SYNCHR
	DB	OFFSET 44D	;INSIST ON COMMA
	CMP	AL,LOW 44D	;ANOTHER COMMA FOLLOWS?
	JZ	SHORT ATRFIN	;IF SO, NULL ARGUMENT SO USE DEFAULT
	CALL	GETBYT		;GET THE BYTE
ATRFIN:	MOV	AL,DL		;GET ATTRIBUTE INTO [A]
	PUSH	BX		;SAVE THE TEXT POINTER
ATRFI2:
	PUSH	AX		;Save the attribute
	CALL	PIXSIZ		;Test for graphics capability
	OR	AL,AL
	JNZ	SHORT ??L002
	JMP	FCERR		;Graphics not available
??L002:
	POP	AX		;Restore the attributeA
	CALL	SETATR		;SET THE ATTRIBUTE AS THE CURRENT ONE
	JAE	SHORT ??L003
	JMP	FCERR		;ILLEGAL ATTRIBUTES GIVE FUNCTION CALL
??L003:
	MOV	BYTE PTR ATRBYT,AL	;Store legal atribute
	POP	BX
	POP	DX		;GET BACK CURRENT POINT
	POP	CX
	JMP	CHRGT2

	PAGE	
	SUBTTL  UTILITY ROUTINES FOR LINE CODE

;
; XDELT SETS [H,L]=ABS(GXPOS-[B,C]) AND SETS CARRY IF [B,C].GT.GXPOS
; ALL REGISTERS EXCEPT [H,L] AND [A,PSW] ARE PRESERVED
; NOTE: [H,L] WILL BE A DELTA BETWEEN GXPOS AND [B,C] - ADD 1 FOR AN X "COUNT"
;
XDELT:	MOV	BX,GXPOS	;GET ACCUMULATOR POSITION
	MOV	AL,BL
	SUB	AL,CL
	MOV	BL,AL		;DO SUBTRACT INTO [H,L]
	MOV	AL,BH
	SBB	AL,CH
	MOV	BH,AL
CNEGHL:	JNAE	SHORT $+3
	RET			;IF NO CARRY, NO NEED TO NEGATE COUNT
PUBLIC	NEGHL
NEGHL:	XOR	AL,AL		;STANDARD [H,L] NEGATE
	SUB	AL,BL
	MOV	BL,AL
	SBB	AL,BH
	SUB	AL,BL
	MOV	BH,AL
	STC			;FLAG THAT NEGATE TOOK PLACE
	RET	
;
; YDELT SETS [H,L]=ABS(GYPOS-[D,E]) AND SETS CARRY IF [D,E].GT.GYPOS
; ALL REGISTERS EXCEPT [H,L] AND [A,PSW] ARE PRESERVED
;
YDELT:	MOV	BX,GYPOS
	MOV	AL,BL
	SUB	AL,DL
	MOV	BL,AL
	MOV	AL,BH
	SBB	AL,DH
	MOV	BH,AL
	JMP	SHORT CNEGHL
;
; XCHGX EXCHANGES [B,C] WITH GXPOS
; XCHGY EXCHANGES [D,E] WITH GYPOS
; XCHGAC PERFORMS BOTH OF THE ABOVE
; NONE OF THE OTHER REGISTERS IS AFFECTED
;
XCHGY:	PUSH	BX
	MOV	BX,GYPOS
	XCHG	BX,DX
	MOV	GYPOS,BX
	POP	BX
	RET	
XCHGAC:	CALL	XCHGY
XCHGX:	PUSH	BX
	PUSH	CX
	MOV	BX,GXPOS
	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI
	MOV	GXPOS,BX
	POP	CX
	POP	BX
	RET	
	PAGE	
	SUBTTL  LINE COMMAND
;
; LINE [(X1,Y1)]-(X2,Y2) [,ATTRIBUTE[,B[F]]]
; DRAW A LINE FROM (X1,Y1) TO (X2,Y2) EITHER
; 1. STANDARD FORM -- JUST A LINE CONNECTING THE 2 POINTS
; 2. ,B=BOXLINE -- RECTANGLE TREATING (X1,Y1) AND (X2,Y2) AS OPPOSITE CORNERS
; 3. ,BF= BOXFILL --  FILLED RECTANGLE WITH (X1,Y1) AND (X2,Y2) AS OPPOSITE CORNERS
;
PUBLIC	GLINE
GLINE:
	CALL	SCAN1		;SCAN THE FIRST COORDINATE
	PUSH	CX		;SAVE THE POINT
	PUSH	DX
	CALL	SYNCHR
	DB	LOW OFFSET MINUTK	;MAKE SURE ITS PROPERLY SEPERATED
	CALL	SCAND		;SCAN THE SECOND SET
	CALL	ATRSCN		;SCAN THE ATTRIBUTE
	POP	DX		;GET BACK THE FIRST POINT
	POP	CX
	JZ	SHORT DOLINE	;IF STATEMENT ENDED ITS A NORMAL LINE
	CALL	SYNCHR
	DB	OFFSET 44D	;OTHERWISE MUST HAVE A COMMA
	CALL	SYNCHR
	DB	OFFSET "B"	;AND A "B"
	JNZ	SHORT ??L004
	JMP	BOXLIN		;IF JUST "B" THE NON-FILLED BOX
??L004:
	CALL	SYNCHR
	DB	OFFSET "F"	;MUST BE FILLED BOX
DOBOXF:	PUSH	BX		;SAVE THE TEXT POINTER
	CALL	SCALXY		;SCALE FIRST POINT
	CALL	XCHGAC		;SWITCH POINTS
	CALL	SCALXY		;SCALE SECOND POINT
	CALL	YDELT		;SEE HOW MANY LINES AND SET CARRY
	JNB	SHORT ??L005
	CALL	XCHGY		;MAKE [D,E] THE SMALLEST Y
??L005:
	INC	BX		;MAKE [H,L] INTO A COUNT
	PUSH	BX		;SAVE COUNT OF LINES
	CALL	XDELT		;GET WIDTH AND SMALLEST X
	JNB	SHORT ??L006
	CALL	XCHGX		;MAKE [B,C] THE SMALLEST X
??L006:
	INC	BX		;MAKE [H,L] INTO A WIDTH COUNT
	PUSH	BX		;SAVE WIDTH COUNT
	CALL	MAPXYC		;MAP INTO A "C"
	POP	DX		;GET WIDTH COUNT
	POP	CX		;GET LINE COUNT
BOXLOP:	PUSH	DX		;SAVE WIDTH
	PUSH	CX		;SAVE NUMBER OF LINES
	CALL	FETCHC		;LOOK AT CURRENT C
	PUSH	AX		;SAVE BIT MASK OF CURRENT "C"
	PUSH	BX		;SAVE ADDRESS
	XCHG	BX,DX		;SET UP FOR NSETCX WITH COUNT
	CALL	NSETCX		;IN [H,L] OF POINTS TO SETC
	POP	BX		;GET BACK STARTING C
	POP	AX		;ADDRESS AND BIT MASK
	CALL	STOREC		;SET UP AS CURRENT "C"
	CALL	DOWNC		;MOVE TO NEXT LINE DOWN IN Y
	POP	CX		;GET BACK NUMBER OF LINES
	POP	DX		;GET BACK WIDTH
	DEC	CX		;COUNT DOWN LINES
	MOV	AL,CH
	OR	AL,CL		;SEE IF ANY LEFT
	JNZ	SHORT BOXLOP	;KEEP DRAWING MORE LINES
	POP	BX
	RET	
DOLINE:	PUSH	CX		;SAVE COORDINATES
	PUSH	DX
	PUSH	BX		;SAVE TEXT POINTER
	CALL	DOGRPH
	MOV	BX,GRPACX	;RESTORE ORIGINAL SECOND COORDINATE
	MOV	GXPOS,BX
	MOV	BX,GRPACY	;FOR BOXLIN CODE
	MOV	GYPOS,BX
	POP	BX		;RESTORE TEXT POINTER
	POP	DX
	POP	CX
	RET	
BOXLIN:	PUSH	BX		;SAVE TEXT POINTER
	MOV	BX,GYPOS
	PUSH	BX		;SAVE Y2
	PUSH	DX		;SAVE Y1
	XCHG	BX,DX		;MOVE Y2 TO Y1
	CALL	DOLINE		;DO TOP LINE
	POP	BX		;MOVE Y1 TO Y2
	MOV	GYPOS,BX
	XCHG	BX,DX		;RESTORE Y1 TO [D,E]
	CALL	DOLINE
	POP	BX		;GET BACK Y2
	MOV	GYPOS,BX	;AND RESTORE
	MOV	BX,GXPOS	;GET X2
	PUSH	CX		;SAVE X1
	MOV	CX,BX		;SET X1=X2
	CALL	DOLINE
	POP	BX
	MOV	GXPOS,BX	;SET X2=X1
	MOV	CX,BX		;RESTORE X1 TO [B,C]
	CALL	DOLINE
	POP	BX		;RESTORE THE TEXT POINTER
	RET	
;
; DOGRPH DRAWS A LINE FROM ([B,C],[D,E]) TO (GXPOS,GYPOS)
;
DOGRPH:
	CALL	SCALXY		;CHEATY SCALING - JUST TRUNCATE FOR NOW
	CALL	XCHGAC
	CALL	SCALXY
DOGRP2:	CALL	YDELT		;GET COUNT DIFFERENCE IN [H,L]
	JNB	SHORT ??L007
	CALL	XCHGAC		;IF CURRENT Y IS SMALLER NO EXCHANGE
??L007:
	PUSH	DX		;SAVE Y1 COORDINATE
	PUSH	BX		;SAVE DELTA Y
	CALL	XDELT
	XCHG	BX,DX		;PUT DELTA X INTO [D,E]
	MOV	BX,OFFSET RIGHTC	;ASSUME X WILL GO RIGHT
	JAE	SHORT LINCN2
	MOV	BX,OFFSET LEFTC
LINCN2:	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;PUT ROUTINE ADDRESS ON STACK AND GET DELTA Y
	CMP	BX,DX		;SEE WHICH DELTA IS BIGGER
	JAE	SHORT YDLTBG	;YDELTA IS BIGGER OR EQUAL

	MOV	MINDEL,BX	;SAVE MINOR AXIS DELTA (Y)
	POP	BX		;GET X ACTION ROUTINE
	MOV	MAXUPD+1,BX	;SAVE IN MAJOR ACTION ADDRESS
	MOV	BX,OFFSET DOWNC	;ALWAYS INCREMENT Y
	MOV	MINUPD+1,BX	;WHICH IS THE MINOR AXIS
	XCHG	BX,DX		;[H,L]=DELTA X=MAJOR DELTA
	JMP	SHORT LINCN3	;MERGE WITH YDLTBG CASE AND DO DRAW

YDLTBG:	POP	SI		;XTHL
	XCHG	SI,BX
	PUSH	SI		;ACTION ROUTINE FOR X INTO [H,L]
				;SAVE DELTA Y ON THE STACK
	MOV	MINUPD+1,BX	;SAVE ADDRESS OF MINOR AXIS UPDATE
	MOV	BX,OFFSET DOWNC	;Y IS ALWAYS INCREMENT MODE
	MOV	MAXUPD+1,BX	;SAVE AS MAJOR AXIS UPDATE
	XCHG	BX,DX		;[H,L]=DELTA X
	MOV	MINDEL,BX	;SAVE MINOR DELTA
	POP	BX		;[H,L]=DELTA Y=MAJOR DELTA

LINCN3:	POP	DX		;GET BACK Y1

; MAJOR AXIS IS ONE WITH THE LARGEST DELTA
; MINOR IS THE OTHER
; READY TO DRAW NOW
; MINUPD+1=ADDRESS TO GO TO UPDATE MINOR AXIS COORDINATE
; MAXUPD+1=ADDRESS TO GO TO UPDATE MAJOR AXIS COORDINATE
; [H,L]=MAJOR AXIS DELTA=# OF POINTS-1
; MINDEL=DELTA ON MINOR AXIS
;
; IDEA IS
;  SET SUM=MAJOR DELTA/2
;  [B,C]=# OF POINTS
;  MAXDEL=-MAJOR DELTA (CONVENIENT FOR ADDING)
; LINE LOOP (LINLP3):
;       DRAW AT CURRENT POSITION
;       UPDATE MAJOR AXIS
;       SUM=SUM+MINOR DELTA
;       IF SUM.GT.MAJOR DELTA THEN UPDATE MINOR AND SUM=SUM-MAJOR DELTA
;       DECREMENT [B,C] AND TEST FOR 0 -- LOOP IF NOT
; END LOOP

	PUSH	BX		;SAVE FOR SETTING UP COUNT
	MOV	MAXDEL,BX	;SAVE MAJOR DELTA FOR SUMMING
	CALL	MAPXYC		;GET POSITION INTO BITMSK AND [H,L]
	POP	DX
	PUSH	DX		;START SUM AT MAXDEL/2
	CALL	HLFDE
	POP	CX		;GET COUNT IN [B,C]
	INC	CX		;NUMBER OF POINTS IS DELTA PLUS ONE
	EXTRN	LINLP3:NEAR
	JMP	LINLP3


HLFDE:	MOV	AL,DH
	OR	AL,AL		;CLEAR CARRY
	RCR	AL,1		;SCALE MEANS SHIFTING RIGHT ONE
	MOV	DH,AL
	MOV	AL,DL
	RCR	AL,1
	MOV	DL,AL
	RET	

	PAGE	
	PAGE	
	SUBTTL  Graphics Initialization

	PUBLIC	GRPINI,GRPRST	;Graphics Initialization routine

	EXTRN	GRPSIZ:NEAR	;Get screen pixel dimension routine (OEM)
DSEG	SEGMENT PUBLIC 'DATASG'
	EXTRN	DRWSCL:WORD,DRWANG:WORD
DSEG	ENDS

;GRPRST resets graphics.  It is called at CLEARC and during INIT
;Entry - none
;Exit  - all registers preserved
;
GRPRST:	PUSH	AX
	PUSH	CX
	PUSH	BX
	XOR	AL,AL
	MOV	BYTE PTR DRWSCL,AL	;Draw scale init
	MOV	BYTE PTR DRWANG,AL	;Draw angle init
	CALL	GRPINI		;Center the graphics cursor
	STC	
	CALL	GETFBC		;Get foreground/background colors
	CALL	SETATR		;Set the default DRAW color
	POP	BX
	POP	CX
	POP	AX
	RET	

;GRPINI - center the graphics cursor.  This routine has been documented to
;OEMs for versions of GW BASIC which are translated to ASM86.
;Entry - none
;Exit  - none
;
GRPINI:	CALL	GRPSIZ		;Get screen pixel dimensions
	PUSH	CX		;B,C has X dimension
	POP	BX		;Move X dimension to H
	INC	BX		;Adjust for zero relative
	SHR	BX,1
	MOV	GRPACX,BX	;Store as previous position
	PUSH	DX		;D,E has Y dimension
	POP	BX		;Move Y dimension to H
	INC	BX
	SHR	BX,1
	MOV	GRPACY,BX	;Store as previous position
	RET	

CSEG	ENDS
	END
