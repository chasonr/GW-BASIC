CSEG	segment public 'CODESG' 
	assume  cs:CSEG, ds:DSEG

include OEM.H

.radix 10

extrn SCNSWI:near
extrn GRPINI:near
extrn SCNCLR:near

; For COMx port comnfiguration
COM_rx_size = 256 ; Default size of receive buffer
COM_tx_size = 128 ; Size of transmit buffer

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
; TODO: Query video hardware and determine supported modes.
; For now, we support only text mode in 80x25.
GWINI:
    push ax
    push cx

    ; Assume we're in text mode at 80 x 25
    mov  al,80
    mov  cl,25
    call SCNSWI
    mov al, 0
    call CLRSCN
    call GRPINI

    pop cx
    pop ax
    ret

; Get OEM header string
; On entry: none
; Returns: Z set if a header string is provided
;          If Z set:
;              BX = Offset from CS of header string to be printed at start
;                   String ends in a null character
PUBLIC GETHED
GETHED:
    xor bx, bx                  ; Set Z to indicate a header string is provided
    mov bx, offset oem_header   ; Address of the header string
    ret

oem_header db "GW-BASIC for FreeDOS, copyright 2025 Ray Chason.", 0Dh, 0Ah, 0

; Copy code segment to new location
; On entry: DS = ES = SS = NEWDS, the new data segment
; Data segment already copied to its new location.
; If any interrupt handlers have been installed, the vectors need to be
; updated with the new code segment.
PUBLIC SEGINI
SEGINI proc near

    ; Keep It Simple, um, Sancho
    mov BASIC_DS, ds
    ret

SEGINI endp

; Data segment for interrupt handlers
BASIC_DS dw ?

; Shut down before exit
; On entry: None
; Returns:  None
; This is the place to remove interrupt handlers and reset the video hardware
; and any other hardware.
PUBLIC GWTERM
GWTERM proc near

    ; Nothing to shut down yet
    ret

GWTERM endp

;-----------------------------------------------------------------------------
; Keyboard support
;-----------------------------------------------------------------------------
; Poll the keyboard
; On entry: None
; Returns:  Z set if no key
;           If Z clear:
;               C set: 2 byte code
;                      0xFF00+code for CTRL-x keys
;                      0x8000+code for function keys and ALT-shifted letters
;               C clear, AL != 0xFE: normal single byte character
;               C clear, AL == 0xFE: two byte code in DX (DH = 0, DL = scan code)
PUBLIC KEYINP
KEYINP proc near

    ; Check for an available key
    mov ah, 01h
    int 16h
    jnz @have_key
    ret

@have_key:
    ; AH = scan code; AL = ASCII character
    xor ah, ah
    int 16h
    or ax, ax
    jnz @F
        ret
    @@:
    or al, al
    jz @function
        ; Regular key
        cmp al, 0FEh
        jne @F
            mov dx, 00FEh
        @@:
        xor ah, ah
        cmp al, 7Fh ; delete key
        je @control
        cmp al, 20h
        jae @end_normal
        @control:
            mov ah, 0FFh ; control keys
        @end_normal:
        or al, al
        clc
        ret

@function:
    ; Return the 0xFF00 keys
    cmp ah, 053h ; Delete key
    jne @F
        mov ax, 0FF7Fh
        or ax, ax
        clc
        ret
    @@:
    ; Search ctrl-table for the scan code
    push cx
    push di
    push es
    mov cx, cs
    mov es, cx
    mov cx, end_ctrl_table - ctrl_table
    mov di, offset ctrl_table
    mov al, ah
    cld
    repne scasb
    jne @special
    mov ax, 0FF00h + (end_ctrl_table - ctrl_table) - 1
    sub ax, cx
    pop es
    pop di
    pop cx
    or ax, ax
    stc
    ret

@special:
    ; Function keys and ALT-shifted keys
    mov al, ah
    sub al, 10h
    jc @other
    cmp al, end_alt_table - alt_table
    jbe @alt_key        ; ALT-shifted letters
    mov al, ah
    sub al, 3Bh
    jc @other
    cmp al, 10
    jb @function_key    ; Unshifted function keys F1-F10
    jmp @other

    ; Alt-shifted keys
@alt_key:
    mov di, ax
    and di, 00FFh
    mov al, alt_table[di]
    or al, al
    je @other
    jmp @8000_key

    ; Function keys
@function_key:
    add al, 20h
@8000_key:
    mov ah, 80h
    pop es
    pop di
    pop cx
    or ax, ax
    stc
    ret

    ; Anything not found above
@other:
    mov dl, ah
    mov dh, 0
    mov al, 0FEh
    pop es
    pop di
    pop cx
    or ax, ax
    clc
    ret

KEYINP endp

; Special keys with these scan codes shall be returned as the corresponding
; control character, with 0xFF in AH.
ctrl_table label byte
    db 00h      ; ctrl-@
    db 00h      ; ctrl-A
    db 73h      ; ctrl-B <- Ctrl-Left
    db 00h      ; ctrl-C
    db 00h      ; ctrl-D
    db 75h      ; ctrl-E <- Ctrl-End
    db 74h      ; ctrl-F <- Ctrl-Right
    db 00h      ; ctrl-G
    db 00h      ; ctrl-H
    db 00h      ; ctrl-I
    db 00h      ; ctrl-J
    db 47h      ; ctrl-K <- Home
    db 00h      ; ctrl-L
    db 4Fh      ; ctrl-M <- End
    db 00h      ; ctrl-N
    db 00h      ; ctrl-O
    db 00h      ; ctrl-P
    db 00h      ; ctrl-Q
    db 52h      ; ctrl-R <- Insert
    db 00h      ; ctrl-S
    db 00h      ; ctrl-T
    db 00h      ; ctrl-U
    db 00h      ; ctrl-V
    db 00h      ; ctrl-W
    db 00h      ; ctrl-X
    db 00h      ; ctrl-Y
    db 76h      ; ctrl-Z <- Ctrl-Page Down
    db 00h      ; ctrl-[
    db 4Dh      ; ctrl-\ <- Right
    db 4Bh      ; ctrl-] <- Left
    db 48h      ; ctrl-^ <- Up
    db 50h      ; ctrl-_ <- Down
end_ctrl_table:

; Special keys with alt codes 10h through the end of this table are returned
; as 0x8000 plus the byte here
alt_table label byte
    db 'Q'      ; scan code 10h: Alt-Q
    db 'W'      ; scan code 11h: Alt-W
    db 'E'      ; scan code 12h: Alt-E
    db 'R'      ; scan code 13h: Alt-R
    db 'T'      ; scan code 14h: Alt-T
    db 'Y'      ; scan code 15h: Alt-Y
    db 'U'      ; scan code 16h: Alt-U
    db 'I'      ; scan code 17h: Alt-I
    db 'O'      ; scan code 18h: Alt-O
    db 'P'      ; scan code 19h: Alt-P
    db 00h      ; scan code 1Ah: none
    db 00h      ; scan code 1Bh: none
    db 00h      ; scan code 1Ch: none
    db 00h      ; scan code 1Dh: none
    db 'A'      ; scan code 1Eh: Alt-A
    db 'S'      ; scan code 1Fh: Alt-S
    db 'D'      ; scan code 10h: Alt-D
    db 'F'      ; scan code 11h: Alt-F
    db 'G'      ; scan code 12h: Alt-G
    db 'H'      ; scan code 13h: Alt-H
    db 'J'      ; scan code 14h: Alt-J
    db 'K'      ; scan code 15h: Alt-K
    db 'L'      ; scan code 16h: Alt-L
    db 00h      ; scan code 17h: none
    db 00h      ; scan code 18h: none
    db 00h      ; scan code 19h: none
    db 00h      ; scan code 1Ah: none
    db 00h      ; scan code 1Bh: none
    db 'Z'      ; scan code 1Ch: Alt-Z
    db 'X'      ; scan code 1Dh: Alt-X
    db 'C'      ; scan code 1Eh: Alt-C
    db 'V'      ; scan code 1Fh: Alt-V
    db 'B'      ; scan code 10h: Alt-B
    db 'N'      ; scan code 11h: Alt-N
    db 'M'      ; scan code 12h: Alt-M
end_alt_table:

; Map key for the screen editor
; It is not entirely clear what this function does.
; On entry: C set or clear and AL and DX set as returned from KEYINP
; Returns:  Z set to ignore the character
;           Z clear and C set if control character
;           Z clear and C clear if printable character
;           AH = 0 if two byte character, else 0xFF
PUBLIC EDTMAP
EDTMAP proc near

    ; TODO: This is a bare minimum function
    cmp al, 20h
    jb @control
    cmp al, 7Fh
    je @control
    mov ah, 0   ; treat as printable
    or al, al
    clc
    ret

@control:
    mov ah, 0FFh
    or ah, ah
    stc
    ret

EDTMAP endp

; Map key for echo to the screen
; On entry: AL = key
; Returns:  Z to ignore the character
;           AH = 0xFF and C set if control character
;                Responses to control characters are defined in FUNTAB
;                in SCNDRV.ASM
;           AH = 0x80 and C set if function key or ALT-shifted key
;           AH = 0x00 and C clear otherwise
PUBLIC PRTMAP
PRTMAP proc near

    ; TODO: This is a bare minimum function
    cmp al, 20h
    jb @control
    cmp al, 7Fh
    je @control
    mov ah, 0   ; treat as printable
    or al, al
    clc
    ret

@control:
    mov ah, 0FFh
    or ah, ah
    stc
    ret

PRTMAP endp

; Map key for input via INKEY$
; On entry: C set and AL and DX set as from KEYINP
; Returns:  C set if two byte character
;           AL = first byte; AH = second byte
PUBLIC INKMAP ; Keyboard
INKMAP proc near

    jc @two_byte
    cmp al, 0FEh
    je @three_byte

        ; One byte character
        or ax, ax
        clc
        ret

    @two_byte:
        ; Two byte character
        ; Check for function and ALT keys
        cmp ah, 80h
        jne @end_80
            ; 20h to 29h are function keys
            cmp al, 20h
            jb @check_alt
            cmp al, 29h
            ja @check_alt
                sub ax, 8020h - 3Bh
                stc
                ret
            @check_alt:
                ; Scan alt_table
                push cx
                push di
                push es
                mov cx, cs
                mov es, cx
                mov cx, end_alt_table - alt_table
                mov di, offset alt_table
                cld
                repne scasb
                jne @ret_80
                    ; Found in table
                    mov ax, 10h + end_alt_table - alt_table - 1
                    sub ax, cx
                @ret_80:
                pop es
                pop di
                pop cx
                stc
                ret
        @end_80:
        ; Check for character in ctrl_table
        cmp ah, 0FFh
        jne @end2
        cmp al, end_ctrl_table - ctrl_table
        jae @end2
            push bx
            mov bl, al
            xor bh, bh
            mov bl, ctrl_table[bx]
            or bl, bl
            je @end2a
                mov ax, bx
            @end2a:
            pop bx
        @end2:
        or ax, ax
        stc
        ret

    @three_byte:
        ; Three byte character
        mov ax, dx
        or ax, ax
        stc
        ret

INKMAP endp

; Map key for input via INPUT statement
; On entry: C set and AL and DX set as from KEYINP
; Returns: Z set to ignore the key
;          C set if two byte sequence in AX
;          else C clear and one byte in AL
PUBLIC INFMAP ; Keyboard
INFMAP proc near

    ; Stub
    ret

INFMAP endp

; "Map super shift key to letter in AL and count"
; On entry: AL = "super shift key"?
; Returns:  AL = letter?
;           CH = count
PUBLIC MAPSUP ; Keyboard
MAPSUP proc near

    ; Stub
    ret

MAPSUP endp

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
SCROUT proc near

    ; TODO: set the page and attribute
if 0
    push ax
    push bx
    push cx
    push dx
    call convert_cursor
    call set_cursor
    mov ah, 09h
    mov bx, 0007h
    mov cx, 1
    int 10h
    pop dx
    pop cx
    pop bx
    pop ax
    clc
    ret
else
    ; An alternate implementation, writing directly to the frame buffer
    push ax
    push di
    push es

    call xy_to_address
    jc @F
        mov ah, 07h ; attribute
        mov es:[di], ax
    @@:

    pop es
    pop di
    pop ax
    ret
endif

SCROUT endp

; Convert 1-based (column, row) to text frame buffer address
; On entry: DX = 1-based (column, row)
; Returns:  C set if out of bounds
;           else ES:DI = frame buffer address of character cell
if 1
xy_to_address proc near private

    ; TODO: allow sizes other than 80x25
    ; TODO: address the monochrome frame buffer
    ; TODO: include the page in the address
    push dx
    dec dh          ; 1-based to 0-based
    dec dl
    cmp dh, 80      ; check bounds
    jae @bounds
    cmp dl, 25
    jae @bounds

    push ax
    mov al, dl      ; AL <- row
    mov dl, dh      ; DL <- column
    xor dh, dh      ; zero extend column
    mov di, dx      ; DI <- column (MUL clobbers DX)
    xor ah, ah      ; zero extend row
    mov dx, 80      ; AX <- row * 80
    mul dx
    add di, ax      ; DI <- row*80 + column
    shl di, 1       ; Two bytes per character
    mov ax, 0B800h  ; Segment for color modes
    mov es, ax
    pop ax
    pop dx
    clc
    ret

@bounds:
    pop dx
    stc
    ret

xy_to_address endp
endif

; Read a character from the screen
; On entry:
;     C set if screen editor is calling
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
;     AL = character; AH = 0
; Only the screen editor checks the C flag
PUBLIC SCRINP
SCRINP proc near

    ; TODO: support graphics modes
    push di
    push es
    call xy_to_address
    mov ax, 0
    jc @end
        mov al, es:[di]
    @end:
    pop es
    pop di
    ret

SCRINP endp

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
PUBLIC SCROLL
SCROLL proc near

    push ax
    push bx
    push cx
    push dx

    ; Make all coordinates 0-based
    dec ah
    dec al
    dec bh
    dec bl

    ; Scroll vertically
    ; AH <- left; BH <- max(AH, BH)
    cmp ah, bh
    jb @F
        xchg ah, bh
    @@:
    ; BH <- right
    add bh, ch
    dec bh
    ; DH <- BIOS function
    mov dh, 06h ; scroll up
    mov dl, al
    sub dl, bl
    jnc @F
        mov dh, 07h ; scroll down
        neg dl
        xchg al, bl
    @@:
    ; BL = top
    ; AL <- bottom
    add al, cl
    dec al

    ; Have: AH = left; BL = top; BH = right; AL = bottom; DL = distance; DH = function
    ; Want: CL = left; CH = top; DL = right, DH = bottom; AL = distance; AH = function
    mov ch, bl  ; CH <- top
    mov cl, ah  ; CL <- left
    mov ah, dh  ; AH <- function
    mov dh, al  ; DH <- bottom
    mov al, dl  ; AL <- distance
    mov dl, bh  ; DL <- right
    mov bh, 07h ; attribute
    int 10h

    ; TODO: scroll horizontally

    pop dx
    pop cx
    pop bx
    pop ax
    ret

SCROLL endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
PUBLIC CLRSCN
CLRSCN proc near

    cmp al, 0
    stc
    jne CLRSCN_end

        push ax
        push bx
        push cx
        push dx

        ; TODO: Only 80x25 text supported
        mov ax, 0600h
        mov bh, 07h
        xor cx, cx
        mov dx, (25-1)*256 + (80-1)
        int 10h

        call SCNCLR

        pop dx
        pop cx
        pop bx
        pop ax
        clc

    CLRSCN_end:
    ret

CLRSCN endp

; Clear to end of line
; On entry: DH = requested column (1 = left)
;           DL = requested row (1 = top)
; Returns: None.
PUBLIC CLREOL
CLREOL proc near

    push ax
    push bx
    push cx
    push dx

    call convert_cursor
    call set_cursor

    mov cx, 80
    sub cl, dl
    mov ax, 0920h
    mov bx, 0007h
    int 10h

    pop dx
    pop cx
    pop bx
    pop ax
    ret

CLREOL endp

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
;                It is uncertain that DX is set
; Returns: none
PUBLIC CSRDSP
CSRDSP proc near

    push ax
    push bx
    push cx
    push dx

    ; TODO: For now, assume 16 line character
    cmp al, 0
    jne @F
        mov cx, 1F1Fh ; start line 31, end line 31
        jmp @set_cursor
    @@:
    cmp al, 1
    jne @F
        mov cx, 000Dh ; start line 0, end line 13
        jmp @set_cursor
    @@:
    cmp al, 2
    jne @F
        mov cx, 0B0Dh ; start line 11, end line 13
        jmp @set_cursor
    @@:
        mov cx, cursor_shape
    @set_cursor:
    mov cursor_shape, cx

    ; According to the Ralf Brown Interrupt List, some BIOSes lock up if
    ; AL is not equal to the video mode
    mov ah, 0Fh
    int 10h     ; AL <- video mode

    ; Set the cursor shape
    mov ah, 01h
    int 10h

    ; Set the cursor position
    call convert_cursor
    call set_cursor

    pop dx
    pop cx
    pop bx
    pop ax
    ret

CSRDSP endp

; Convert 1-based (column, row) to 0-based (row, column)
convert_cursor proc near private

    xchg dl, dh
    dec dl
    dec dh
    ret

convert_cursor endp

; Set the cursor to 0-based (row, column)
set_cursor proc near private

    cmp dh, 25
    jae @F
    cmp dl, 80
    jae @F
        push ax
        push bx
        mov bh, 0
        mov ah, 02h
        int 10h
        pop bx
        pop ax
    @@:
    ret

set_cursor endp

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
    ; TODO: honor the C flag
    mov al, foreground_color
    mov ah, 0
    mov bl, background_color
    mov bh, 0
    ret

; Set foreground and background colors
; On entry: AX = foreground color
;           BX = background color
; Returns: none
PUBLIC SETFBC
SETFBC:
    mov foreground_color, al
    mov background_color, bl
    ret

; Return format for function key display
; Returns: BX points to a three byte structure
;          byte 0: characters per key
;          byte 1: number of keys to display
;          byte 2: number of first function key
PUBLIC FKYFMT
FKYFMT proc near

    ; Stub: fixed format for 80 column display
    mov bx, offset fkey_format
    mov byte ptr 0[bx], 6
    mov byte ptr 1[bx], 10
    mov byte ptr 2[bx], 1
    ret

FKYFMT endp

; Unclear what this does. It seems to depend on Z set or clear and have
; to do with display of function keys.
; It sets internal variables giving the first key displayed and the number
; of lines not displaying function keys. Z is set if function keys are
; not displayed.
; Returns:  Z clear if function keys are to be displayed
PUBLIC FKYADV
FKYADV proc near

    ; TODO: handle 40 column display
    push ax
    jnz @keys_off
        mov max_line, 23
        mov ax, 1
        jmp @end
    @keys_off:
        mov max_line, 24
        mov ax, 0
    @end:
    or ax, ax
    pop ax
    ret

FKYADV endp

;-----------------------------------------------------------------------------
; Graphics mode screen support
;-----------------------------------------------------------------------------

; Return size of graphics screen
; Returns: CX = maximum X coordinate
;          DX = maximum Y coordinate
PUBLIC GRPSIZ
GRPSIZ:
    ; TODO: For now, we support only 80x25 text mode, but GRPINI calls this
    mov cx,80*8 - 1
    mov dx,25*8 - 1
    ret

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
    ; TODO: Graphics modes are not yet supported. For 16 color EGA and VGA
    ; modes, set the hardware registers here.
    mov graph_attr, al
    ret

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
    ; TODO: The speaker is not yet implemented, but SNDRST calls this
    ret

;-----------------------------------------------------------------------------
; Support for COMx ports
;-----------------------------------------------------------------------------

; Set up receive buffers for COMx ports
; On entry: CX = COM buffer segment
;           Z set if /C: option was not specified on the command line
;           If Z not set: DX = receive buffer size
; Returns:  C set if error
;           If C clear: DX = configured receive buffer size
PUBLIC SETCBF
SETCBF proc near

if NMCOMT eq 0
    ; No COMx ports configured
    mov dx, 0
    clc
    ret
else
    ; TODO COMx ports not yet supported
    push ax
    push bx
    push cx
    push si
    push di

    mov si, cx ; Segment in SI

    ; Set default receive buffer size
    jnz @F
        mov dx, COM_rx_size
    @@:

    ; Align to multiple of 16 bytes
    add dx, 15
    jc SETCBF_error
    mov cl, 4
    shr dx, cl

    ; Allocate this much per COMx device
    mov cx, NMCOMT ; How many ports
    xor si, si     ; Offset to next receive or transmit buffer
    xor bx, bx     ; Offset to next control structure
    SETCBF_allocate:
        ; TODO: place the receive buffer segment
        add si, dx
        jc SETCBF_error
        ; TODO: place the transmit buffer segment
        add si, (COM_tx_size+15)/16
        jc SETCBF_error
    loop SETCBF_allocate
    ; Check total allocation for overflow
    ; Set C if *more* than 64K allocated
    cmp si, 65536/16
    cmc
    jc SETCBF_error

    ; Return size in DX
    mov dx, si
    mov cl, 4
    shl dx, cl

    clc
SETCBF_error:
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    ret
endif

SETCBF endp

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
POLLEV proc near

    ; TODO: stub; no events currently supported
    push ax
    xor ax, ax
    pop ax
    ret

POLLEV endp

CSEG ENDS

DSEG segment public 'DATASG'

; Function key format returned by FKYFMT
fkey_format db 3 dup (?)

; Shape of text cursor set by CSRDSP
cursor_shape dw 0B0Dh ; Initially in overwrite mode

; Colors set by SETFBC and returned by GETFBC
foreground_color db 7
background_color db 0

; Color set by SETATR
graph_attr db 0

; Maximum line for SCROLL
max_line db 0

DSEG ends

END
