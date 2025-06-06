CSEG	SEGMENT PUBLIC 'CODESG' 
	ASSUME  CS:CSEG

;-----------------------------------------------------------------------------
; Initialization and termination
;-----------------------------------------------------------------------------

; Initialize hardware
; On entry: None
; Returns:  None
; Calls SCNSWI and GRPINI
; This is the place to query the video hardware and determine what graphics
; support may be available. Any interrupt handlers needed can be installed
; here.
PUBLIC GWINI
GWINI:
    INT 3

; Get OEM header string
; On entry: none
; Returns: BX = offset of header string to be printed at start
PUBLIC GETHED
GETHED:
    INT 3

; Copy code segment to new location
; On entry: DS = ES = SS = NEWDS, the new data segment
; Data segment already copied up to TSTACK; that is, the portion declared
; in GWDATA.ASM, but not the OEM portion, nor the part in GWRAM.ASM.
; If any interrupt handlers have been installed, the vectors need to be
; updated with the new code segment.
PUBLIC SEGINI
SEGINI:
    INT 3

; Shut down before exit
; On entry: None
; Returns:  None
; This is the place to remove interrupt handlers and reset the video hardware
; and any other hardware.
PUBLIC GWTERM
GWTERM:
    INT 3

;-----------------------------------------------------------------------------
; Keyboard support
;-----------------------------------------------------------------------------
; Poll the keyboard
; On entry: None
; Returns:  Z set if no key
;           If Z clear:
;               C set: 2 byte code
;                      0xFF00+code for various special keys
;                      0x8000+code for function keys and ALT-shifted letters
;               C clear, AL != 0xFE: normal single byte character
;               C clear, AL == 0xFE: two byte code in DX (DH = 0, DL = scan code)
PUBLIC KEYINP ; Keyboard
KEYINP:
    INT 3

; Map key for the screen editor
; It is not entirely clear what this function does.
; On entry: C set or clear and AL and DX set as returned from KEYINP
; Returns:  Z set to ignore the character
;           Z clear and C set if control character
;           Z clear and C clear if printable character
;           AH = 0 if two byte character, else 0xFF
PUBLIC EDTMAP
EDTMAP:
    INT 3

; Map key for echo to the screen
; On entry: AL = key
; Returns:  Z to ignore the character
;           AH = 0xFF and C set if editor function
;           AH = 0x80 and C set if function key or ALT-shifted key
;           AH = 0x00 and C clear otherwise
PUBLIC PRTMAP
PRTMAP:
    INT 3

; Map key for input via INKEY$
; On entry: C set and AL and DX set as from KEYINP
; Returns:  C set if two byte character
;           AL = first byte; AH = second byte
PUBLIC INKMAP ; Keyboard
INKMAP:
    INT 3

; Map key for input via INPUT statement
; On entry: C set and AL and DX set as from KEYINP
; Returns: Z set to ignore the key
;          C set if two byte sequence in AX
;          else C clear and one byte in AL
PUBLIC INFMAP ; Keyboard
INFMAP:
    INT 3

; "Map super shift key to letter in AL and count"
; On entry: AL = "super shift key"?
; Returns:  AL = letter?
;           CH = count
PUBLIC MAPSUP ; Keyboard
MAPSUP:
    INT 3

;-----------------------------------------------------------------------------
; Text mode screen support
;-----------------------------------------------------------------------------

; Print a character to the screen
; On entry:
;     AL = character
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
; No caller checks the C flag
PUBLIC SCROUT
SCROUT:
    INT 3

; Read a character from the screen
; On entry:
;     C set if screen editor is calling
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
;     AL = character; AH = 0
; Only the screen editor checks the C flag
PUBLIC SCRINP
SCRINP:
    INT 3

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row AL
; Rows and columns begin at 1
; Returns: None
PUBLIC SCROLL
SCROLL:
    INT 3

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
PUBLIC CLRSCN
CLRSCN:
    INT 3

; Clear to end of line
; On entry: DH = requested column (1 = left)
;           DL = requested row (1 = top)
; Returns: None.
PUBLIC CLREOL
CLREOL:
    INT 3

; Sets the cursor visibility and size according to the parameters to the
; LOCATE statement.
; On entry: AH = LOCATE parameter 3 present if nonzero
;           AL = LOCATE parameter 3: cursor is visible if nonzero
;           BH = LOCATE parameter 4 present
;           BL = LOCATE parameter 4: start line of cursor
;           CH = LOCATE parameter 5 present
;           CL = LOCATE parameter 5: end line of cursor
; Returns:  C set if a parameter was out of the valid range
PUBLIC CSRATR
CSRATR:
    INT 3

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
; Returns: none
PUBLIC CSRDSP
CSRDSP:
    INT 3

; Print the screen
; On entry: none
; Returns:  none
PUBLIC LCPY
LCPY:
    INT 3

; Read attribute at requested position
; On entry: AL = column (1 = left)
;           BL = row (1 = top)
; Returns:  BL = attribute; BH = 0
PUBLIC SCRATR
SCRATR:
    INT 3

; Process parameters to the SCREEN statement
; On entry: BX = list of parameters:
;                BX[0] = 1 byte: number of parameters specified
;                followed by two bytes for each parameter:
;                     A nonzero byte if the parameter was specified
;                     A byte for the value of the parameter (ignored if the
;                         first parameter is zero)
; Call SCNSWI if BIOS mode changed
; Returns: C set if error
PUBLIC SCRSTT
SCRSTT:
    INT 3

; Process parameters to the COLOR statement
; On entry: BX = list of parameters:
;                BX[0] = 1 byte: number of parameters specified
;                followed by two bytes for each parameter:
;                     A nonzero byte if the parameter was specified
;                     A byte for the value of the parameter (ignored if the
;                         first parameter is zero)
; Call SCNSWI if BIOS mode changed
; Returns: C set if error
PUBLIC SETCLR
SETCLR:
    INT 3

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
PUBLIC SWIDTH
SWIDTH:
    INT 3

; On entry: ES:DX = address to be checked
;           AL = byte to be POKEd
; Returns: Z set if the POKE was filtered. This is not an error; the POKE will
; just not be done
;
; Minimum specified filtering is done if DEF SEG has not been changed from its
; initial value:
; * POKE 138,0 clears the keyboard buffer
; * POKE 106,0 turns off key expansion
; * POKEs to 0x002E-0x002F call BASVAR, BX = 0
; * POKEs to 0x0347-0x0348 call BASVAR, BX = 1
; * POKEs to 0x0030-0x0031 call BASVAR, BX = 2
; * POKEs to 0x0358-0x0359 call BASVAR, BX = 3
PUBLIC POKFLT
POKFLT:
    INT 3

; On entry: ES:DX = address to be checked
; Returns: Z set if the PEEK was filtered. This is not an error; the PEEK will
; just not be done
;
; Minimum specified filtering is done if DEF SEG has not been changed from its
; initial value:
; * PEEKs to 0x002C-0x002D access the TOPMEM variable directly
; * PEEK(106) calls FKYSNS and returns 0xFF if C is set, else 0x00
; * PEEKs to 0x002E-0x002F call BASVAR, BX = 0
; * PEEKs to 0x0347-0x0348 call BASVAR, BX = 1
; * PEEKs to 0x0030-0x0031 call BASVAR, BX = 2
; * PEEKs to 0x0358-0x0359 call BASVAR, BX = 3
PUBLIC PEKFLT
PEKFLT:
    INT 3

; Query foreground and background colors
; On entry: C set to get text colors, clear for graphics colors
; Returns: AX = foreground color
;          BX = background color
PUBLIC GETFBC
GETFBC:
    INT 3

; Set foreground and background colors
; On entry: AX = foreground color
;           BX = background color
; Returns: none
PUBLIC SETFBC
SETFBC:
    INT 3

; Return format for function key display
; Returns: BX points to a three byte structure
;          byte 0: characters per key
;          byte 1: number of keys to display
;          byte 2: number of first function key
PUBLIC FKYFMT
FKYFMT:
    INT 3

; Unclear what this does. It seems to depend on Z set or clear and have
; to do with display of function keys.
; It sets internal variables giving the first key displayed and the number
; of lines not displaying function keys. Z is set if function keys are
; not displayed.
PUBLIC FKYADV
FKYADV:
    INT 3

;-----------------------------------------------------------------------------
; Graphics mode screen support
;-----------------------------------------------------------------------------

; Return size of graphics screen
; Returns: CX = maximum X coordinate
;          DX = maximum Y coordinate
PUBLIC GRPSIZ
GRPSIZ:
    INT 3

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
PUBLIC STOREC
STOREC:
    INT 3

; Retrieve the cursor position to be saved by STOREC
; Returns: AL:BX = cursor position
PUBLIC FETCHC
FETCHC:
    INT 3

; Move one pixel up
; On entry: none
; Returns   none
; Nothing in the released sources calls this
PUBLIC UPC
UPC:
    INT 3

; Move one pixel down
; On entry: none
; Returns   none
PUBLIC DOWNC
DOWNC:
    INT 3

; Move one pixel left
; On entry: none
; Returns   none
PUBLIC LEFTC
LEFTC:
    INT 3

; Move one pixel right
; On entry: none
; Returns   none
PUBLIC RIGHTC
RIGHTC:
    INT 3

; Clip coordinates to the screen boundaries
; On entry: CX = X coordinate as signed integer
;           DX = Y coordinate as signed integer
; Returns:  CX = clipped X coordinate
;           DX = clipped Y coordinate
;           C set if coordinates are in bounds
PUBLIC SCALXY
SCALXY:
    INT 3

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           CY = Y coordinate
; Returns: none
PUBLIC MAPXYC
MAPXYC:
    INT 3

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  none
PUBLIC SETATR
SETATR:
    INT 3

; Read pixel at current position
; Returns: AL = pixel attribute
PUBLIC READC
READC:
    INT 3

; Write pixel at current location, using current attribute
; Returns: none
PUBLIC SETC_
SETC_:
    INT 3

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
PUBLIC NSETCX
NSETCX:
    INT 3

; Return aspect ratio of pixel
; Returns: BX = pixel width divided by pixel height as 8.8 fixed point
;          DX = pixel height divided by pixel width as 8.8 fixed point
PUBLIC GTASPC
GTASPC:
    INT 3

; Return number of bits per pixel, or 0 if text mode
; Returns:  pixel size in AL
PUBLIC PIXSIZ
PIXSIZ:
    INT 3

;-----------------------------------------------------------------------------
; Bit-blit support via the GET and PUT statements
;-----------------------------------------------------------------------------

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
PUBLIC PGINIT
PGINIT:
    INT 3

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
PUBLIC NREAD
NREAD:
    INT 3

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
PUBLIC NWRITE
NWRITE:
    INT 3

;-----------------------------------------------------------------------------
; Support for the PAINT statement
;-----------------------------------------------------------------------------
; Set up flood fill algorithm
; On entry: AL = boundary attribute
; Returns: none
PUBLIC PNTINI
PNTINI:
    INT 3

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
PUBLIC TDOWNC
TDOWNC:
    INT 3

; Move current position up with boundary check
; Returns: C set if moving up would pass the top of the screen;
;          the current position is unchanged in that case
; This differs from UPC only in the boundary check
PUBLIC TUPC
TUPC:
    INT 3

; On entry: Setup done with PNTINI
;           DX = number of boundary pixels to skip right
;           No pixels are painted if this many pixels in the boundary color
;           are found
; Returns:  BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           CSAVEA and CSAVEM set to the point where drawing began, in the
;           format returned by FETCHC
PUBLIC SCANR
SCANR:
    INT 3

; Fill pixels to the right until the boundary color is found
; On entry: Setup done with PNTINI
; Returns:  BX = number of pixels painted
;           CL != 0 if at least one pixel changed
PUBLIC SCANL
SCANL:
    INT 3

;-----------------------------------------------------------------------------
; Speaker support
;-----------------------------------------------------------------------------

; Play a tone on the speaker
; On entry: AL = 0 to wait for tone to complete
;                1 to queue the tone and return
;                0xFF to turn off speaker
;           if AL != 0xFF:
;               CX = frequency in hertz
;               DX = duration in 1/18.2 second clock ticks
; Returns: C set on error
PUBLIC DONOTE
DONOTE:
    INT 3

;-----------------------------------------------------------------------------
; Support for COMx ports
;-----------------------------------------------------------------------------

; Set up receive buffers for COMx ports
; On entry: CX = COM buffer segment
;           Z set if /C: option was not specified on the command line
;           If Z not set: DX = receive buffer size
; Returns:  C set if error
PUBLIC SETCBF
SETCBF:
    INT 3

; Set up COMx port
; On entry: AH = unit number; 0 for COM1, 1 for COM2
;           DS:BX = address of DCB
;       _DEVID=0D               ;RS232 Channel ID (0..n)
;       _BAUDR=1D               ;baud rate (least significant byte 1st)
;                               ;(0=disable device, 9600=9600 baud etc.)
;       _BYTSZ=3D               ;bits per byte (4..8)
;       _PARIT=4D               ;parity (0..4)=...(NONE, ODD, EVEN, MARK, SPACE)
;       _STOPB=5D               ;(0..2)= (1, 1.5, 2) stop bits
;       _RLSTO=6D               ;RLSD (rec. line signal detect) timeout
;       _CTSTO=8D               ;CTS (clear to send) timeout
;       _DSRTO=10D              ;DSR (data set ready) timeout
;                               ;All timeout values are in milliseconds.
;                               ;0=infinite, LSB is always 1st.
;                               ;Support of Timeout Flags by BIOS is
;                               ;optional.
;       _CMFLG=12D              ;Boolean attributes mask for this device
;       _CMBIN=1O               ;(0/1)=ASCII/BINARY (ASC option not in filename)
;       _CMRTS=2O               ;non-zero=Suppress Request-To-Send (RS option)
;       _CMCOD=20O              ;non-zero=user specified ASC or BIN in filename
;       _CMCTS=40O              ;non-zero=CTS parm not defaulted
;       _CMCLF=100O             ;non-zero=Send line feed after CR
;       _CMCRF=200O             ;non-zero=last char sent was Carriage Return
; On return: AH = 0
; On error:  AH = 0xFF: BASIC signals "Illegal file name"
;                       Otherwise same as STACOM
PUBLIC INICOM
INICOM:
    INT 3

; Receive a byte from a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2
; On return:  AH = 0
;             Z clear if byte received
;             AL = byte received
; On error:   AH = nonzero error code, as for STACOM
PUBLIC RECCOM
RECCOM:
    INT 3

; Query status of COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2
; On return:  AH = 0
;             CX = free bytes in queue
;             DX = total bytes in queue
; On error:   AH = nonzero error code
;                  1 = buffer overflow
;                  2 = parity error
;                  3, 4, 5 = device timeout
;                  others are mapped to "I/O error"
PUBLIC STACOM
STACOM:
    INT 3

; Send a byte to a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2
;             AL = byte to send
; On return:  AH = 0
; On error:   AH = nonzero error code, as for STACOM
PUBLIC SNDCOM
SNDCOM:
    INT 3

; End access to a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2
; On return:  AH = 0
; On error:   AH = nonzero error code, as for STACOM
PUBLIC TRMCOM
TRMCOM:
    INT 3

;-----------------------------------------------------------------------------
; Support for LPTx ports
;-----------------------------------------------------------------------------

; Send byte to LPTx device
; On entry: AH = LPTx device index (0 = LPT1)
;           AL = byte to send
; Returns:  AH = 0 if success
;           AH != 0 mapped to BASIC errors as follows:
;           1: Device unavailable
;           2: Device timeout
;           3: Out of paper
;           others: I/O error
PUBLIC SNDLPT
SNDLPT:
    INT 3

; Perform light pen functions
; On entry: AL selects the function
;           Functions 0-9 as listed in GWSTS.ASM:
;               n=0: Return -1 if pen was down since last poll, else 0.
;               n=1: Return X Graphics Coordinate where pen was last activated.
;               n=2: Return Y Graphics Coordinate where pen was last activated.
;               n=3: Return -1 if pen is currently down, 0 if currently up.
;               n=4: Return last known valid X Graphics Coordinate.
;               n=5: Return last known valid Y Graphics Coordinate.
;               n=6: Return character row where pen was last activated.
;               n=7: Return character column where pen was last activated.
;               n=8: Return last known character row.
;               n=9: Return last known character column.
;           AL = 0xFE: PEN ON statement
;           AL = 0xFF: PEN OFF statement
; Returns:  BX = value selected by functions 0-9
;           None for 0xFE and 0xFF
PUBLIC RDPEN
RDPEN:
    INT 3

;-----------------------------------------------------------------------------
; Joystick support
;-----------------------------------------------------------------------------

; Read a joystick button
; On entry: AH = 0 to query whether the button is now pressed
;                1 to query whether the button was pressed since the last call
;           AL = index of button to read
; Returns:  AL = 1 if the button is or was pressed, else 0
PUBLIC RDTRIG
RDTRIG:
    INT 3

; Read a joystick axis
; On entry: AL = joystick axis to read
; Returns:  C clear if success
;           BX = joystick axis as a signed integer
; This implementation expects AL=0 to be called first. It will query all axes
; on AL=0, and return their state when called with AL != 0.
PUBLIC RDSTIK
RDSTIK:
    INT 3

;-----------------------------------------------------------------------------
; Event support
;-----------------------------------------------------------------------------

; Test for trappable events
; Return: Z clear if any event occurred
PUBLIC POLLEV
POLLEV:
    INT 3

CSEG ENDS
END
