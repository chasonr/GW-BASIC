CSEG	segment public 'CODESG' 
	assume  cs:CSEG, ds:DSEG

include OEM.H

.radix 10

extrn SCNSWI:near
extrn GRPINI:near
extrn SCNCLR:near
extrn KYBCLR:near
extrn SFTOFF:near
extrn BASVAR:near
extrn FKYSNS:near

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
GWINI proc near

    push ax
    push bx
    push cx
    push es

    ; If we have an EGA or VGA, set blink mode
    mov bx, 0040h
    mov es, bx
    mov al, es:[0065h]
    mov blink_flag, al ; to restore on exit
    mov bl, 1
    mov ax, 1003h
    int 10h

    ; Set up screen mode 0
    mov bx, offset screen_mode_0
    mov mode_ptr, bx
    call mode_0_SCRSTT_init

    mov  al,text_width
    mov  cl,25
    call SCNSWI
    mov al, 0
    call CLRSCN
    call GRPINI

    pop es
    pop cx
    pop bx
    pop ax
    ret

GWINI endp

; Get OEM header string
; On entry: none
; Returns: Z set if a header string is provided
;          If Z set:
;              BX = Offset from CS of header string to be printed at start
;                   String ends in a null character
PUBLIC GETHED
GETHED proc near

    xor bx, bx                  ; Set Z to indicate a header string is provided
    mov bx, offset oem_header   ; Address of the header string
    ret

GETHED endp

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

    push ax
    push bx
    push cx

    ; Set blink mode back to starting state
    mov bl, blink_flag
    mov cl, 5
    shr bl, cl
    and bl, 1

    mov ax, 1003h
    int 10h

    ; Return to 80 column text mode
    mov ah, 0Fh     ; Do this if not already 80 column text
    int 10h
    cmp al, 02h
    je @end_video
    cmp al, 03h
    je @end_video
    cmp al, 07h
    je @end_video
        mov ax, 0003h   ; CGA 80 column mode
        int 10h
        mov ah, 0Fh
        int 10h
        cmp al, 03h     ; Did it work?
        je @end_video
            mov ax, 0007h ; Monochrome 80 column mode
            int 10h
    @end_video:

    pop cx
    pop bx
    pop ax
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
            ; Return ESC as 0xFF15
            cmp al, 1Bh
            jne @F
                mov ax, 0FF15h
                or ax, ax
                stc
                ret
            @@:
            ; If 0xFF00+ctrl represents an editor key, return 0x0000+ctrl
            push bx
            xor bh, bh
            mov bl, al
            mov bl, ctrl_table[bx]
            or bl, bl
            pop bx
            je @end_normal
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
        ; Check for ESC
        cmp ax, 0FF15h
        jne @F
            mov ax, 001Bh
            or ax, ax
            clc
            ret
        @@:
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

; Map ALT-shifted letter to a letter in AL and a count
; On entry: AX = key code representing an ALT-shifted letter, as returned
;                by KEYINP
; Returns:  AL = letter
;           CH = count
PUBLIC MAPSUP ; Keyboard
MAPSUP proc near

    mov ch, 1
    ret

MAPSUP endp

;-----------------------------------------------------------------------------
; PEEK and POKE filters
;-----------------------------------------------------------------------------

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
POKFLT proc near

    ; If DEF SEG has been changed from the default, allow all POKEs
    push bx
    push dx
    mov bx, es
    cmp bx, BASIC_DS
    jne @end_filter

    cmp dx, 138
    jne @F
    cmp al, 0
    jne @F
        call KYBCLR  ; Clear the keyboard queue
        jmp @filtered
    @@:

    cmp dx, 106
    jne @F
    cmp al, 0
    jne @F
        call SFTOFF  ; Turn off function key expansion
        jmp @filtered
    @@:

    call basvar_index ; BX <- index to BASVAR variable
    jc @no_filter
        push cx
        mov cl, al
        ; Read the variable to AX
        clc
        call BASVAR
        ; Change the low or the high byte
        test dx, 1
        jne @odd
            mov al, cl
        jmp @write
        @odd:
            mov ah, cl
        @write:
        ; Write the variable from AX
        stc
        call BASVAR
        pop cx
        
    jmp @filtered
    @no_filter:
        ; No further filters
        mov bx, 1
        or bx, bx
        jmp @end_filter

@filtered:
    xor bx, bx ; Set Z
@end_filter:
    pop dx
    pop bx
    ret

POKFLT endp

; On entry: ES:BX = address to be checked
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
PEKFLT proc near

    ; If DEF SEG has been changed from the default, allow all PEEKs
    push bx
    push dx
    mov dx, bx
    mov bx, es
    cmp bx, BASIC_DS
    jne @end_filter

    cmp dx, 106
    jne @F
        xor al, al
        call FKYSNS
        sbb al, al  ; AL <- 0FFh if C set
        jmp @filtered
    @@:

    cmp dx, 002Ch
    jne @F
        mov al, byte ptr TOPMEM+0
        jmp @filtered
    @@:

    cmp dx, 002Dh
    jne @F
        mov al, byte ptr TOPMEM+1
        jmp @filtered
    @@:

    call basvar_index ; BX <- index to BASVAR variable
    jc @no_filter
        ; Read the variable to AX
        clc
        call BASVAR
        ; Return the low or the high byte
        test dx, 1
        je @F
            mov al, ah
        @@:
        xor ah, ah
        jmp @filtered
    jmp @filtered
    @no_filter:
        ; No further filters
        mov bx, 1
        or bx, bx
        jmp @end_filter

@filtered:
    xor bx, bx ; Set Z
@end_filter:
    pop dx
    pop bx
    ret

PEKFLT endp

; For POKFLT and PEKFLT: Find addresses that access variables controlled by
; BASVAR and return the index in BX.
; On entry: address to check in DX
; Returns:  C set and DX unchanged if BASVAR does not control the address
;           C clear and DX set to 0 for low byte, or 1 for high byte, if
;               BASVAR controls the address
basvar_index proc near private

    push ax
    mov bx, basvar_table_size - 2
    @scan:
        mov ax, dx               ; AX <- target address
        sub ax, basvar_table[bx] ; AX <- offset from target address
        cmp ax, 2                ; match if 0 or 1
        jb @match
    sub bx, 2
    jnc @scan  ; until end of table
    ; No match
    pop ax
    stc
    ret

    ; Match
    @match:
    mov dx, ax
    pop ax
    shr bx, 1
    clc
    ret

basvar_table dw 002Eh
             dw 0347h
             dw 0030h
             dw 0358h
basvar_table_size = $ - basvar_table

basvar_index endp

;-----------------------------------------------------------------------------
; Dispatchers for the various screen modes
;-----------------------------------------------------------------------------

; Structure describing support for a screen mode
Screen_Mode struc
    ; Handlers for various functions
    SCRSTT_init    dw ?
    SCRSTT_color   dw ?
    SCRSTT_actpage dw ?
    SCRSTT_vispage dw ?
    SCROUT_handler dw ?
    SCRINP_handler dw ?
    SCROLL_handler dw ?
    CLRSCN_handler dw ?
    CLREOL_handler dw ?
    CSRATR_handler dw ?
    CSRDSP_handler dw ?
    LCPY_handler   dw ?
    SCRATR_handler dw ?
    SETCLR_handler dw ?
    SWIDTH_handler dw ?
    GETFBC_handler dw ?
    SETFBC_handler dw ?
    FKYFMT_handler dw ?
    FKYADV_handler dw ?
    GRPSIZ_handler dw ?
    STOREC_handler dw ?
    FETCHC_handler dw ?
    UPC_handler    dw ?
    DOWNC_handler  dw ?
    LEFTC_handler  dw ?
    RIGHTC_handler dw ?
    MAPXYC_handler dw ?
    SETATR_handler dw ?
    READC_handler  dw ?
    SETC_handler   dw ?
    NSETCX_handler dw ?
    PGINIT_handler dw ?
    NREAD_handler  dw ?
    NWRITE_handler dw ?
    PNTINI_handler dw ?
    TDOWNC_handler dw ?
    TUPC_handler   dw ?
    SCANR_handler  dw ?
    SCANL_handler  dw ?

    ; Constants for each mode
    text_columns   db ? ; Text columns
    text_rows      db ? ; Text rows
    x_res          dw ? ; X pixel resolution
    y_res          dw ? ; Y pixel resolution
    width_height   dw ? ; Pixel width over pixel height as 8.8 fixed
    height_width   dw ? ; Pixel height over pixel width as 8.8 fixed
    num_pages      db ? ; Number of supported pages
    pixel_size     db ? ; Number of bits per pixel
Screen_Mode ends

; Table of supported screen modes
mode_table label word
    dw offset screen_mode_0
    dw offset screen_mode_1
mode_table_size = ($ - mode_table)/2

; JWASM complains that the literal is too long if I try to use a structure.
; Declare the fields individually.
screen_mode_0 label word
    dw mode_0_SCRSTT_init     ; SCRSTT_init
    dw mode_0_SCRSTT_color    ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw mode_0_SCROUT          ; SCROUT_handler
    dw mode_0_SCRINP          ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw generic_CLRSCN         ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw generic_CSRDSP         ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw mode_0_SCRATR          ; SCRATR_handler
    dw mode_0_SETCLR          ; SETCLR_handler
    dw mode_0_SWIDTH          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw mode_0_SETFBC          ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw graphics_stub          ; GRPSIZ_handler
    dw graphics_stub          ; STOREC_handler
    dw graphics_stub          ; FETCHC_handler
    dw graphics_stub          ; UPC_handler
    dw graphics_stub          ; DOWNC_handler
    dw graphics_stub          ; LEFTC_handler
    dw graphics_stub          ; RIGHTC_handler
    dw graphics_stub          ; MAPXYC_handler
    dw graphics_stub          ; SETATR_handler
    dw graphics_stub          ; READC_handler
    dw graphics_stub          ; SETC_handler
    dw graphics_stub          ; NSETCX_handler
    dw graphics_stub          ; PGINIT_handler
    dw graphics_stub          ; NREAD_handler
    dw graphics_stub          ; NWRITE_handler
    dw graphics_stub          ; PNTINI_handler
    dw graphics_stub          ; TDOWNC_handler
    dw graphics_stub          ; TUPC_handler
    dw graphics_stub          ; SCANR_handler
    dw graphics_stub          ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 400                    ; y_res
    dw 0100h                  ; width_height
    dw 0100h                  ; height_width
    db 8                      ; num_pages
    db 0                      ; pixel_size

screen_mode_1 label word
    dw mode_1_SCRSTT_init     ; SCRSTT_init
    dw mode_1_SCRSTT_color    ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw cga_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw generic_CSRDSP         ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw mode_1_SETCLR          ; SETCLR_handler
    dw mode_1_SWIDTH          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw mode_1_SETFBC          ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw mode_1_STOREC          ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw cga_UPC                ; UPC_handler
    dw cga_DOWNC              ; DOWNC_handler
    dw mode_1_LEFTC           ; LEFTC_handler
    dw mode_1_RIGHTC          ; RIGHTC_handler
    dw mode_1_MAPXYC          ; MAPXYC_handler
    dw mode_1_SETATR          ; SETATR_handler
    dw mode_1_READC           ; READC_handler
    dw cga_SETC               ; SETC_handler
    dw mode_1_NSETCX          ; NSETCX_handler
    dw mode_1_PGINIT          ; PGINIT_handler
    dw mode_1_NREAD           ; NREAD_handler
    dw mode_1_NWRITE          ; NWRITE_handler
    dw mode_1_PNTINI          ; PNTINI_handler
    dw cga_TDOWNC             ; TDOWNC_handler
    dw cga_TUPC               ; TUPC_handler
    dw mode_1_SCANR           ; SCANR_handler
    dw mode_1_SCANL           ; SCANL_handler
    db 40                     ; text_columns
    db 25                     ; text_rows
    dw 320                    ; x_res
    dw 200                    ; y_res
    dw 00D5h                  ; width_height (0.833)
    dw 0133h                  ; height_width (1.200)
    db 1                      ; num_pages
    db 2                      ; pixel_size

; Most functions will use this macro to select the correct handler
dispatch macro handler

    push di
    mov di, mode_ptr
    call cs:Screen_Mode.handler[di]
    pop di
    ret

endm

; Process parameters to the SCREEN statement
; On entry: BX = list of parameters:
;                BX[0] = 1 byte: number of parameters specified
;                followed by two bytes for each parameter:
;                     A nonzero byte if the parameter was specified
;                     A byte for the value of the parameter (ignored if the
;                         first parameter is zero)
; Call SCNSWI if BIOS mode changed
; Returns: C set if error
public SCRSTT
SCRSTT proc near

    push ax
    push bx
    push cx
    push di

    mov di, mode_ptr

    ; Get number of parameters
    mov cl, [bx]
    inc bx

    ; Parameter 1: screen mode
    dec cl
    js @error   ; error if no parameters
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_1
        ; Determine if the mode is valid
        cmp ah, mode_table_size
        jae @error              ; invalid if outside of mode_table
        mov al, ah
        xor ah, ah
        mov di, ax
        shl di, 1
        mov di, mode_table[di]
        or di, di
        jz @error               ; or if mode_table has 0
        ; Set up the mode
        call cs:Screen_Mode.SCRSTT_init[di]
        jc @error
        mov mode_ptr, di
        mov  al,text_width
        mov  cl,25
        call SCNSWI
        call GRPINI
        call SCNCLR
    @end_1:

    ; Parameter 2: color flag
    dec cl
    js @exit
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_2
        mov al, ah
        xor ah, ah
        call cs:Screen_Mode.SCRSTT_color[di]
    @end_2:

    ; Parameter 3: active page
    dec cl
    js @exit
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_3
        cmp ah, cs:Screen_Mode.num_pages[di]
        jae @error
        mov al, ah
        xor ah, ah
        call cs:Screen_Mode.SCRSTT_actpage[di]
        jc @error
        mov active_page, al
    @end_3:

    ; Parameter 4: visible page
    dec cl
    js @exit
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_4
        cmp ah, cs:Screen_Mode.num_pages[di]
        jae @error
        mov al, ah
        xor ah, ah
        call cs:Screen_Mode.SCRSTT_vispage[di]
        jc @error
        mov visible_page, al
    @end_4:

@exit:
    pop di
    pop cx
    pop bx
    pop ax
    clc
    ret

@error:
    pop di
    pop cx
    pop bx
    pop ax
    stc
    ret

SCRSTT endp

; Print a character to the screen
; On entry:
;     AL = character
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
; No caller checks the C flag
public SCROUT
SCROUT proc near

    dispatch SCROUT_handler

SCROUT endp

; Read a character from the screen
; On entry:
;     C set if screen editor is calling
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
;     AL = character; AH = 0
; Only the screen editor checks the C flag
public SCRINP
SCRINP proc near

    dispatch SCRINP_handler

SCRINP endp

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
public SCROLL
SCROLL proc near

    dispatch SCROLL_handler

SCROLL endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
public CLRSCN
CLRSCN proc near

    dispatch CLRSCN_handler

CLRSCN endp

; Clear to end of line
; On entry: DH = requested column (1 = left)
;           DL = requested row (1 = top)
; Returns: None.
public CLREOL
CLREOL proc near

    dispatch CLREOL_handler

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
public CSRATR
CSRATR proc near

    dispatch CSRATR_handler

CSRATR endp

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
;                It is uncertain that DX is set
; Returns: none
public CSRDSP
CSRDSP proc near

    dispatch CSRDSP_handler

CSRDSP endp

; Print the screen
; On entry: none
; Returns:  none
LCPY proc near

    dispatch LCPY_handler

LCPY endp

; Read attribute at requested position
; On entry: AL = column (1 = left)
;           BL = row (1 = top)
; Returns:  BL = attribute; BH = 0
public SCRATR
SCRATR proc near

    dispatch SCRATR_handler

SCRATR endp

; Process parameters to the COLOR statement
; On entry: BX = list of parameters:
;                BX[0] = 1 byte: number of parameters specified
;                followed by two bytes for each parameter:
;                     A nonzero byte if the parameter was specified
;                     A byte for the value of the parameter (ignored if the
;                         first parameter is zero)
; Call SCNSWI if BIOS mode changed
; Returns: C set if error
public SETCLR
SETCLR proc near

    push ax
    push bx
    push cx
    push di

    ; Get number of parameters
    mov cl, [bx]
    inc bx

    ; Parameter 1: foreground color
    dec cl
    js @error   ; error if no parameters
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_1
        mov foreground_color, ah
    @end_1:

    ; Parameter 2: background color
    dec cl
    js @exit
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_2
        mov background_color, ah
    @end_2:

    ; Parameter 3: border color
    dec cl
    js @exit
    mov ax, [bx]
    add bx, 2
    or al, al
    je @end_3
        mov border_color, ah
    @end_3:

@exit:
    mov di, mode_ptr
    call cs:Screen_Mode.SETCLR_handler[di]

    pop di
    pop cx
    pop bx
    pop ax
    clc
    ret

@error:
    pop cx
    pop bx
    pop ax
    stc
    ret

SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
public SWIDTH
SWIDTH proc near

    push ax
    push di
    mov di, mode_ptr
    call cs:Screen_Mode.SWIDTH_handler[di]
    pop di
    pop ax
    jc @F
        mov text_width, al
        push cx
        mov cl, 25
        call SCNSWI
        pop cx
        call GRPINI
        call SCNCLR
        clc
    @@:
    ret

SWIDTH endp

; Query foreground and background colors
; On entry: C set to get text colors, clear for graphics colors
; Returns: AX = foreground color
;          BX = background color
public GETFBC
GETFBC proc near

    dispatch GETFBC_handler

GETFBC endp

; Set foreground and background colors
; On entry: AX = foreground color
;           BX = background color
; Returns: none
public SETFBC
SETFBC proc near

    mov foreground_color, al
    mov background_color, bl
    dispatch SETFBC_handler

SETFBC endp

; Return format for function key display
; Returns: BX points to a three byte structure
;          byte 0: characters per key
;          byte 1: number of keys to display
;          byte 2: number of first function key
public FKYFMT
FKYFMT proc near

    dispatch FKYFMT_handler

FKYFMT endp

; Advance to the next display format for function keys in response to
; control-T.
; On entry: Z clear if function keys are currently displayed
; Returns:  Z clear if function keys are to be displayed
;           First key to display is updated if necessary
public FKYADV
FKYADV proc near

    dispatch FKYADV_handler

FKYADV endp

; Return size of graphics screen
; Returns: CX = maximum X coordinate
;          DX = maximum Y coordinate
public GRPSIZ
GRPSIZ proc near

    dispatch GRPSIZ_handler

GRPSIZ endp

;-----------------------------------------------------------------------------
; Graphics mode screen support
;-----------------------------------------------------------------------------

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
public STOREC
STOREC proc near

    dispatch STOREC_handler

STOREC endp

; Retrieve the cursor position to be saved by STOREC
; Returns: AL:BX = cursor position
public FETCHC
FETCHC proc near

    dispatch FETCHC_handler

FETCHC endp

; Move one pixel up
; On entry: none
; Returns   none
; Nothing in the released sources calls this
public UPC
UPC proc near

    dispatch UPC_handler

UPC endp

; Move one pixel down
; On entry: none
; Returns   none
public DOWNC
DOWNC proc near

    dispatch DOWNC_handler

DOWNC endp

; Move one pixel left
; On entry: none
; Returns   none
public LEFTC
LEFTC proc near

    dispatch LEFTC_handler

LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
public RIGHTC
RIGHTC proc near

    dispatch RIGHTC_handler

RIGHTC endp

; Clip coordinates to the screen boundaries
; On entry: CX = X coordinate as signed integer
;           DX = Y coordinate as signed integer
; Returns:  CX = clipped X coordinate
;           DX = clipped Y coordinate
;           C set if coordinates are in bounds
public SCALXY
SCALXY proc near

    push ax
    push di
    mov di, mode_ptr
    mov al, 1

    ; Left boundary
    or cx, cx
    jns @F
        ; CX < 0
        xor cx, cx
        xor ax, ax
    @@:

    ; Right boundary
    cmp cx, cs:Screen_Mode.x_res[di]
    jl @F
        ; CX >= x_res
        mov cx, cs:Screen_Mode.x_res[di]
        dec cx
        xor ax, ax
    @@:

    ; Top boundary
    or dx, dx
    jns @F
        ; DX < 0
        xor dx, dx
        xor ax, ax
    @@:

    ; Bottom boundary
    cmp dx, cs:Screen_Mode.y_res[di]
    jl @F
        ; DX >= y_res
        mov dx, cs:Screen_Mode.y_res[di]
        dec dx
        xor ax, ax
    @@:

    shr al, 1 ; clear C if any bound exceeded
    pop di
    pop ax
    ret

SCALXY endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
public MAPXYC
MAPXYC proc near

    dispatch MAPXYC_handler

MAPXYC endp

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  C set if error
public SETATR
SETATR proc near

    dispatch SETATR_handler

SETATR endp

; Read pixel at current position
; Returns: AL = pixel attribute
public READC
READC proc near

    dispatch READC_handler

READC endp

; Write pixel at current location, using current attribute
; Returns: none
public SETC_
SETC_ proc near

    dispatch SETC_handler

SETC_ endp

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
public NSETCX
NSETCX proc near

    dispatch NSETCX_handler

NSETCX endp

; Return aspect ratio of pixel
; Returns: BX = pixel width divided by pixel height as 8.8 fixed point
;          DX = pixel height divided by pixel width as 8.8 fixed point
public GTASPC
GTASPC proc near

    push di
    mov di, mode_ptr
    mov bx, cs:Screen_Mode.width_height[di]
    mov dx, cs:Screen_Mode.height_width[di]
    pop di
    ret

GTASPC endp

; Return number of bits per pixel, or 0 if text mode
; Returns:  pixel size in AL
public PIXSIZ
PIXSIZ proc near

    push di
    mov di, mode_ptr
    mov al, cs:Screen_Mode.pixel_size[di]
    pop di
    ret

PIXSIZ endp

;-----------------------------------------------------------------------------
; Bit-blit support via the GET and PUT statements
;-----------------------------------------------------------------------------

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
public PGINIT
PGINIT proc near

    dispatch PGINIT_handler

PGINIT endp

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
public NREAD
NREAD proc near

    dispatch NREAD_handler

NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
public NWRITE
NWRITE proc near

    dispatch NWRITE_handler

NWRITE endp

;-----------------------------------------------------------------------------
; Support for the PAINT statement
;-----------------------------------------------------------------------------
; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
public PNTINI
PNTINI proc near

    dispatch PNTINI_handler

PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
public TDOWNC
TDOWNC proc near

    dispatch TDOWNC_handler

TDOWNC endp

; Move current position up with boundary check
; Returns: C set if moving up would pass the top of the screen;
;          the current position is unchanged in that case
; This differs from UPC only in the boundary check
public TUPC
TUPC proc near

    dispatch TUPC_handler

TUPC endp

; On entry: Setup done with PNTINI
;           DX = number of border pixels to skip right
;           No pixels are painted if this many pixels in the border color
;           are found
; Returns:  BX = number of pixels painted
;           DX reduced by number of border pixels skipped
;           CL != 0 if at least one pixel changed
;           CSAVEA and CSAVEM set to the point where drawing began, in the
;           format returned by FETCHC
;           Current position updated
public SCANR
SCANR proc near

    dispatch SCANR_handler

SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
public SCANL
SCANL proc near

    dispatch SCANL_handler

SCANL endp

;-----------------------------------------------------------------------------
; Generic handlers for modes not requiring special handling
;-----------------------------------------------------------------------------

; Stub for unimplemented graphics handlers
graphics_stub proc near private

    stc
    ret

graphics_stub endp

; Set the active page
generic_SCRSTT_actpage proc near private

    clc
    ret

generic_SCRSTT_actpage endp

; Set the visible page
; On entry: AL = requested page
generic_SCRSTT_vispage proc near private

    push bx
    push cx

    ; Save for comparison afterward
    mov cl, al

    ; Set the page
    mov ah, 05h
    int 10h

    ; Did we get the page we wanted?
    mov ah, 0Fh
    int 10h
    cmp bh, cl
    jne @error

    ; We got the page we wanted
    pop cx
    pop bx
    clc
    ret

@error:
    ; The page was not set
    pop cx
    pop bx
    stc
    ret

generic_SCRSTT_vispage endp

; Print a character to the screen
; On entry:
;     AL = character
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
; No caller checks the C flag
generic_SCROUT proc near private

    push ax
    push bx
    push cx
    push dx
    call convert_cursor
    call set_cursor
    mov ah, 09h
    mov bh, 0
    mov bl, 07h
    mov cx, 1
    int 10h
    pop dx
    pop cx
    pop bx
    pop ax
    clc
    ret

generic_SCROUT endp

; Read a character from the screen
; On entry:
;     C set if screen editor is calling
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
;     AL = character; AH = 0
; Only the screen editor checks the C flag
generic_SCRINP proc near private

    push bx
    push dx

    call convert_cursor ; to 0-based (row, column)
    call set_cursor

    mov bh, active_page ; read character at cursor position
    mov ah, 08h
    int 10h
    xor ah, ah

    ; Return null character as space
    or al, al
    jne @F
        mov al, ' '
    @@:

    pop dx
    pop bx
    clc
    ret

generic_SCRINP endp

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
generic_SCROLL proc near private

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

generic_SCROLL endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
generic_CLRSCN proc near private

    cmp al, 0
    stc
    jne @end

        push ax
        push bx
        push cx
        push dx

        mov ax, 0600h
        mov bh, 07h
        xor cx, cx
        mov dh, max_line
        inc dh
        mov dl, text_width
        int 10h

        call SCNCLR
        call GRPINI

        pop dx
        pop cx
        pop bx
        pop ax
        clc

    @end:
    ret

generic_CLRSCN endp

; Clear to end of line
; On entry: DH = requested column (1 = left)
;           DL = requested row (1 = top)
; Returns: None.
generic_CLREOL proc near private

    push ax
    push bx
    push cx
    push dx

    call convert_cursor
    call set_cursor

    mov cl, text_width
    xor ch, ch
    sub cl, dl
    jbe @F      ; Don't try to clear zero or fewer columns
        mov ax, 0920h
        mov bx, 0007h
        int 10h
    @@:

    pop dx
    pop cx
    pop bx
    pop ax
    ret

generic_CLREOL endp

; Sets the cursor visibility and size according to the parameters to the
; LOCATE statement.
; On entry: AH = LOCATE parameter 3 present if nonzero
;           AL = LOCATE parameter 3: cursor is visible if nonzero
;           BH = LOCATE parameter 4 present
;           BL = LOCATE parameter 4: start line of cursor
;           CH = LOCATE parameter 5 present
;           CL = LOCATE parameter 5: end line of cursor
; Returns:  C set if a parameter was out of the valid range
generic_CSRATR proc near private

    ; TODO: implement the cursor visible flag
    test bh, bh
    je @F
        cmp bl, 31
        ja @error
        mov byte ptr cursor_shape+1, bl
    @@:
    test ch, ch
    je @F
        cmp cl, 31
        ja @error
        mov byte ptr cursor_shape+0, cl
    @@:

    clc
    ret

@error:
    stc
    ret

generic_CSRATR endp

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
;                It is uncertain that DX is set
; Returns: none
generic_CSRDSP proc near private

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

generic_CSRDSP endp

; Print the screen
; On entry: none
; Returns:  none
generic_LCPY proc near private

    int 05h ; Print-screen BIOS call
    ret

generic_LCPY endp

; Query foreground and background colors
; On entry: C set to get text colors, clear for graphics colors
; Returns: AX = foreground color
;          BX = background color
generic_GETFBC proc near private

    ; TODO: honor the C flag
    mov al, foreground_color
    mov ah, 0
    mov bl, background_color
    mov bh, 0
    ret

generic_GETFBC endp

; Return format for function key display
; Returns: BX points to a three byte structure
;          byte 0: characters per key
;          byte 1: number of keys to display
;          byte 2: number of first function key
generic_FKYFMT proc near private

    mov bx, offset fkey_format
    mov byte ptr 0[bx], 6
    mov byte ptr 1[bx], 10
    cmp byte ptr 2[bx], 0
    jne @F
        mov byte ptr 2[bx], 1
    @@:
    ret

generic_FKYFMT endp

; Advance to the next display format for function keys in response to
; control-T.
; On entry: Z clear if function keys are currently displayed
; Returns:  Z clear if function keys are to be displayed
;           First key to display is updated if necessary
generic_FKYADV proc near private

    push ax
    jnz @keys_are_on
        ; Function keys are currently off
        ; Turn them on and set the first one displayed to 1
        mov max_line, 23
        mov fkey_format+2, 1
        mov ax, 1
        jmp @end
    @keys_are_on:
        ; Function keys are currently on
        mov al, fkey_format+2 ; First key displayed
        add al, 5             ; If 40 columns, advance five keys
        cmp text_width, 40
        jbe @F
            add al, 5         ; If 80 columns, turn keys off
        @@:
        cmp al, 10
        jbe @keys_on
            ; Turn keys off
            mov fkey_format+2, 1
            xor ax, ax
            jmp @end
        @keys_on:
            mov fkey_format+2, al
    @end:
    or ax, ax
    pop ax
    ret

generic_FKYADV endp

; Return size of graphics screen
; Returns: CX = maximum X coordinate
;          DX = maximum Y coordinate
generic_GRPSIZ proc near private

    mov cx, cs:Screen_Mode.x_res[di]
    dec cx
    mov dx, cs:Screen_Mode.y_res[di]
    dec dx
    ret

generic_GRPSIZ endp

; Retrieve the cursor position to be saved by STOREC
; Returns: AL:BX = cursor position
generic_FETCHC proc near private

    mov bx, video_pos
    mov al, video_bitmask
    ret

generic_FETCHC endp

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
    cmp dl, text_width
    jae @F
        push ax
        push bx
        mov bh, active_page
        mov ah, 02h
        int 10h
        pop bx
        pop ax
    @@:
    ret

set_cursor endp

;-----------------------------------------------------------------------------
; Screen mode 0: monochrome or color text
;-----------------------------------------------------------------------------

; Set up mode 0
mode_0_SCRSTT_init proc near private

    push bx
    push cx

    ; Query the current mode and width
    mov ah, 0Fh
    int 10h
    cmp ah, 40
    mov al, 3     ; BIOS mode for 80 columns
    jg @F
        mov al, 1 ; BIOS mode for 40 columns
    @@:
    mov cl, al    ; Save this to check for success
    mov ah, 00h
    int 10h       ; Set the BIOS mode

    ; Did the mode switch succeed?
    mov ah, 0Fh
    int 10h
    cmp al, cl
    jne @monochrome
        ; We got the requested mode.
        mov text_width, ah
        mov ax, 0B800h
        mov video_seg, ax 
    jmp @end
    @monochrome:
        ; A monochrome adapter (MDA or Hercules) will reject the switch to
        ; BIOS mode 1 or 3.
        mov ax, 0B000h
        mov video_seg, ax
        ; Set BIOS mode 7, to ensure the registers are set up correctly
        mov ax, 0007h
        int 10h
        mov text_width, 80
    @end:

    pop cx
    pop bx
    clc
    ret

mode_0_SCRSTT_init endp

; Color flag for mode 0
; On entry: AX = color flag (SCREEN parameter 2)
mode_0_SCRSTT_color proc near private

    push bx
    push cx

    ; Get BIOS video mode
    mov ch, al
    mov ah, 0Fh
    int 10h
    ; Affects BIOS modes 0-3 only; ignored for mode 7
    cmp al, 3
    ja @end
        shr al, 1
        cmp ch, 1 ; set C if CH != 0
        cmc
        rcl al, 1 ; rotate C into bit 1 of mode
        mov ah, 00h
        int 10h
        call SCNSWI
    @end:

    pop cx
    pop bx
    ret

mode_0_SCRSTT_color endp

; Print a character to the screen
; On entry:
;     AL = character
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
; No caller checks the C flag
mode_0_SCROUT proc near private

    push ax
    push di
    push es

    call mode_0_xy_to_address
    jc @F
        mov ah, text_attr
        mov es:[di], ax
    @@:

    pop es
    pop di
    pop ax
    ret

mode_0_SCROUT endp

; Read a character from the screen
; On entry:
;     C set if screen editor is calling
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
;     AL = character; AH = 0
; Only the screen editor checks the C flag
mode_0_SCRINP proc near private

    push di
    push es
    call mode_0_xy_to_address
    mov ax, 0
    jc @end
        mov al, es:[di]
    @end:
    pop es
    pop di
    ret

mode_0_SCRINP endp

; Read attribute at requested position
; On entry: AL = column (1 = left)
;           BL = row (1 = top)
; Returns:  BL = attribute; BH = 0
mode_0_SCRATR proc near private

    push di
    push es
    push dx
    mov dx, bx
    call mode_0_xy_to_address
    mov bx, 0
    jc @end
        mov bl, es:[di+1]
    @end:
    pop dx
    pop es
    pop di
    ret

mode_0_SCRATR endp

; Set up configured colors
; On entry: foreground_color, background_color and border_color updated
mode_0_SETCLR proc near private

    call mode_0_set_text_attr
    mov bl, border_color
    mov bh, 0
    mov ah, 0Bh
    int 10h
    clc
    ret

mode_0_SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
mode_0_SWIDTH proc near private

    cmp video_seg, 0B000h
    je @monochrome
        ; Color adapters can do 40 or 80 columns
        cmp al, 40
        je @40_columns
        cmp al, 80
        jne @error
            ; Set 80 columns
            push bx
            mov ah, 0Fh     ; Get current BIOS mode
            int 10h
            or al, 02h      ; Set the corresponding 80 column mode
            mov ah, 00h
            int 10h
            pop bx
            jmp @ok
        @40_columns:
            ; Set 40 columns
            push bx
            mov ah, 0Fh     ; Get current BIOS mode
            int 10h
            and al, 0FDh    ; Set the corresponding 40 column mode
            mov ah, 00h
            int 10h
            pop bx
            jmp @ok
    @monochrome:
        ; Monochrome adapters can do 80 columns only
        cmp al, 80
        jne @error
        ; No action is needed

@ok:
    clc
    ret

@error:
    stc
    ret

mode_0_SWIDTH endp

; Set foreground and background colors
; On entry: foreground_color and background_color set
; Returns: none
mode_0_SETFBC proc near private

    call mode_0_set_text_attr
    ret

mode_0_SETFBC endp

; Return size of graphics screen
; Returns: CX = maximum X coordinate
;          DX = maximum Y coordinate
mode_0_GRPSIZ proc near private

    mov dl,text_width
    xor dh, dh
    mov cl, 3
    shl dx, cl
    dec cx
    mov cx, dx
    mov dx,25*8 - 1
    ret

mode_0_GRPSIZ endp

; Convert 1-based (column, row) to text frame buffer address
; On entry: DX = 1-based (column, row)
; Returns:  C set if out of bounds
;           else ES:DI = frame buffer address of character cell
mode_0_xy_to_address proc near private

    ; TODO: If we support modes greater than 80x25, we'll need to check
    ; the row at the configured height, and calculate the page differently

    push dx
    dec dh              ; 1-based to 0-based
    dec dl
    cmp dh, text_width  ; check bounds
    jae @bounds
    cmp dl, 25
    jae @bounds

    push ax
    mov al, dl          ; AL <- row
    mov dl, dh          ; DL <- column
    xor dh, dh          ; zero extend column
    mov di, dx          ; DI <- column (MUL clobbers DX)
    xor ah, ah          ; zero extend row
    mul text_width      ; AX <- row * text_width
    add di, ax          ; DI <- row*text_width + column
    shl di, 1           ; Two bytes per character
    mov ah, active_page ; Page on which we draw
    mov al, 0           ; Load into high byte
    repeat 4
        shl ah, 1       ; shift left four, for page * 4096
    endm
    add di, ax
    mov es, video_seg   ; Segment for color modes
    pop ax
    pop dx
    clc
    ret

@bounds:
    pop dx
    stc
    ret

mode_0_xy_to_address endp

; On entry: foreground_color and background_color set
; Returns: none in registers
; Sets text_attr for use by text routines
mode_0_set_text_attr proc near private

    push cx

    mov ch, background_color ; CH =  x  x  x  x  x b2 b1 b0
    and ch, 07h              ; CH =  0  0  0  0  0 b2 b1 b0
    mov cl, 4
    shl ch, cl               ; CH =  0 b2 b1 b0  0  0  0  0
    mov cl, foreground_color
    test cl, 10h             ; highlight/blink bit
    je @F
        or ch, 80h           ; CH = f4 b2 b1 b0  0  0  0  0
    @@:
    and cl, 0Fh
    or ch, cl                ; CH = f4 b2 b1 b0 f3 f2 f1 f0
    mov text_attr, ch

    pop cx
    ret

mode_0_set_text_attr endp

;-----------------------------------------------------------------------------
; Screen mode 1: CGA 320x200, 4 colors
;-----------------------------------------------------------------------------

; Set up mode 1
mode_1_SCRSTT_init proc near private

    ; Set BIOS mode 5
    mov ax, 0005h
    int 10h

    ; Did it work?
    push bx
    mov ah, 0Fh
    int 10h
    pop bx
    cmp al, 5
    jne @error

    ; We're good.
    mov text_width, 40
    clc
    ret

    ; Oops.
    @error:
    stc
    ret

mode_1_SCRSTT_init endp

; Color flag for mode 1
; On entry: AX = color flag (SCREEN parameter 2)
mode_1_SCRSTT_color proc near private

    push bx
    push cx

    ; Get BIOS video mode
    mov ch, al
    mov ah, 0Fh
    int 10h

    ; This differs from mode_0_SCRSTT_color in that bit 0 set means
    ; monochrome rather than color, and we don't need to check for the BIOS
    ; mode in range (we are always in mode 4 or 5).
    shr al, 1
    cmp ch, 1 ; set C if CH == 0
    rcl al, 1 ; rotate C into bit 1 of mode
    mov ah, 00h
    int 10h
    call SCNSWI

    pop cx
    pop bx
    ret

mode_1_SCRSTT_color endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
; Modes 1 and 2 both use this
cga_CLRSCN proc near

    cmp al, 0
    stc
    jne @end

        push ax
        push cx
        push dx
        push es

        cld
        mov al, max_line
        inc al
        xor ah, ah
        mov dx, 40*4 ; 40 words per raster line, 4 lines per text row
        mul dx
        mov dx, ax
        mov es, video_seg
        xor di, di
        xor ax, ax
        mov cx, dx
        rep stosw   ; clear the even lines
        mov di, 2000h
        mov cx, dx
        rep stosw   ; clear the odd lines

        call SCNCLR
        call GRPINI

        pop es
        pop dx
        pop cx
        pop ax

        clc

    @end:
    ret

cga_CLRSCN endp

; Set up configured colors
; On entry: foreground_color, background_color and border_color updated
mode_1_SETCLR proc near private

    ; Mode 1 uses foreground_color as background color and background_color
    ; to select between two possible palettes.
    mov bl, foreground_color ; COLOR parameter 1
    and bl, 0Fh
    mov bh, 0
    mov ah, 0Bh
    int 10h
    mov bl, background_color ; COLOR parameter 2
    and bl, 01h
    mov bh, 1
    mov ah, 0Bh
    int 10h
    clc
    ret

mode_1_SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
mode_1_SWIDTH proc near private

    ; TODO: this will switch to mode 2, but first we need mode 2 implemented
    stc
    ret

mode_1_SWIDTH endp

; Set foreground and background colors
; On entry: foreground_color and background_color set
; Returns: none
mode_1_SETFBC proc near private

    call mode_0_set_text_attr
    ret

mode_1_SETFBC endp

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
mode_1_STOREC proc near private

    push ax
    push dx

    mov video_pos, bx
    mov video_bitmask, al

    ; Recover the X coordinate
    mov ax, bx
    and ax, 1FFFh ; exclude the odd-line bit
    mov dl, 80
    div dl        ; AH <- byte offset within line
    mov dl, ah    ; to 16 bit in DX
    xor dh, dh
    shl dx, 1     ; convert to pixel coordinate
    shl dx, 1
    mov al, video_bitmask
    test al, 0Fh  ; get the lower two bits
    je @F
        or dx, 2
    @@:
    test al, 33h
    je @F
        or dx, 1
    @@:

    mov x_coordinate, dx

    pop dx
    pop ax
    ret

mode_1_STOREC endp

; Move one pixel up
; On entry: none
; Returns   none
; Modes 1 and 2 both use this
cga_UPC proc near private

    push ax

    mov ax, video_pos
    sub ax, 2000h
    jnc @F
        add ax, 4000h - 80
    @@:
    mov video_pos, ax

    pop ax
    ret

cga_UPC endp

; Move one pixel down
; On entry: none
; Returns   none
; Modes 1 and 2 both use this
cga_DOWNC proc near private

    push ax

    mov ax, video_pos
    add ax, 2000h
    cmp ax, 4000h
    jb @F
        sub ax, 4000h - 80
    @@:
    mov video_pos, ax

    pop ax
    ret

cga_DOWNC endp

; Move one pixel left
; On entry: none
; Returns   none
mode_1_LEFTC proc near private

    push ax

    dec x_coordinate

    ; Shift the bit mask left
    mov al, video_bitmask
    rol al, 1
    rol al, 1
    mov video_bitmask, al

    ; Carry into the address
    jnc @F
        dec video_pos
    @@:

    pop ax
    ret

mode_1_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
mode_1_RIGHTC proc near private

    push ax

    inc x_coordinate

    ; Shift the bit mask right
    mov al, video_bitmask
    ror al, 1
    ror al, 1
    mov video_bitmask, al

    ; Carry into the address
    jnc @F
        inc video_pos
    @@:

    pop ax
    ret

mode_1_RIGHTC endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
mode_1_MAPXYC proc near private

    push ax
    push bx
    push cx
    push di

    mov x_coordinate, cx

    ; Bit 1 of the Y coordinate goes to bit 13 of the address
    xor di, di
    shr dl, 1
    rcr di, 1
    shr di, 1
    shr di, 1

    ; Rest of the Y coordinate times 80
    mov al, 80
    mul dl
    add di, ax

    ; X coordinate divided by 4
    mov ax, cx
    shr ax, 1
    shr ax, 1
    add di, ax

    ; Bit mask from X mod 4
    and cl, 03h
    shl cl, 1
    mov al, 0C0h
    shr al, cl

    ; Save the graphics cursor position
    mov video_pos, di
    mov video_bitmask, al

    pop di
    pop dx
    pop cx
    pop ax
    ret

mode_1_MAPXYC endp

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  C set if error
mode_1_SETATR proc near private

    ; Duplicate lower two bits through the entire byte
    push bx
    and ax, 03h
    mov bx, ax
    mov al, bit_dup[bx]
    pop bx
    mov graph_attr, al
    clc
    ret

mode_1_SETATR endp

; Shared by mode_1_SETATR and mode_1_PNTINI
bit_dup db 00h, 55h, 0AAh, 0FFh

; Read pixel at current position
; Returns: AL = pixel attribute
mode_1_READC proc near private

    push bx
    push cx
    push es

    ; Address in frame buffer
    les bx, video_addr

    ; Current pixels
    mov al, es:[bx]

    ; Shift count
    mov cx, x_coordinate
    and cl, 03h ; 0 1 2 3
    xor cl, 03h ; 3 2 1 0
    shl cl, 1   ; 6 4 2 0

    ; Shift the bits into place
    shr al, cl
    and al, 03h

    pop es
    pop cx
    pop bx
    ret

mode_1_READC endp

; Write pixel at current location, using current attribute
; Returns: none
; Modes 1 and 2 both use this
cga_SETC proc near private

    push ax
    push bx
    push es

    ; Address in frame buffer
    les bx, video_addr

    ; Current pixels
    mov al, es:[bx]

    ; Pixel to substitute
    mov ah, graph_attr

    ; Replace the pixel at video_bitmask
    xor ah, al
    and ah, video_bitmask
    xor ah, al

    ; Update pixels
    mov es:[bx], ah

    pop es
    pop bx
    pop ax
    ret

cga_SETC endp

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
mode_1_NSETCX proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    cld

    ; Start and end of draw
    mov dx, x_coordinate
    add bx, dx
    cmp bx, 320
    jb @F
        mov bx, 320
    @@:
    dec bx

    ; Does the draw cross a byte boundary?
    mov ax, bx
    xor ax, dx
    and ax, 0FFFCh
    jnz @multibyte

        ; Draw lies within a single byte
        mov si, dx
        and si, 03h
        mov cl, left_mask_2[si]
        mov si, bx
        and si, 03h
        and cl, right_mask_2[si] ; CL has 1 where bits will be replaced
        les di, video_addr
        mov ah, es:[di]        ; AL <- existing byte
        mov al, graph_attr
        ; where CL has 0: AL <- AH
        ; where CL has 1: AL <- AL
        xor al, ah
        and al, cl
        xor al, ah
        stosb

    jmp @end
    @multibyte:

        ; Draw spans two or more bytes
        ; Draw the first byte
        mov ch, graph_attr
        les di, video_addr
        mov si, dx
        and si, 03h
        mov cl, left_mask_2[si]
        mov ah, es:[di]        ; AL <- existing byte
        mov al, ch
        ; where CL has 0: AL <- AH
        ; where CL has 1: AL <- AL
        xor al, ah
        and al, cl
        xor al, ah
        stosb

        ; Draw zero or more whole bytes
        mov si, bx      ; Save right partial byte for the end
        and si, 03h
        mov cl, 2
        shr bx, cl
        shr dx, cl
        sub bx, dx
        dec bx
        je @end_whole
            mov al, ch  ; AL <- graph_attr
            mov cx, bx
            rep stosb
            mov ch, al  ; CH <- graph_attr
        @end_whole:

        ; Draw the last byte
        mov cl, right_mask_2[si]
        mov ah, es:[di]        ; AH <- existing byte
        mov al, ch
        ; where CL has 0: AL <- AH
        ; where CL has 1: AL <- AL
        xor al, ah
        and al, cl
        xor al, ah
        stosb

    @end:

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

left_mask_2 db 0FFh, 03Fh, 0Fh, 03h
right_mask_2 db 0C0h, 0F0h, 0FCh, 0FFh

mode_1_NSETCX endp

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
mode_1_PGINIT proc near private

    mov blit_addr, bx
    mov blit_bits, cx
    add cx, 7
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov blit_bytes, cx
    mov cx, blit_bits
    jnc @F
        push ax
        push bx
        mov bl, al
        xor bh, bh
        shl bx, 1
        mov ax, draw_table[bx]
        mov blit_mixer, ax
        pop bx
        pop ax
    @@:
    ret

draw_table dw draw_or
           dw draw_and
           dw draw_preset
           dw draw_pset
           dw draw_xor

mode_1_PGINIT endp

; Drawing routines set by PGINIT
; Existing byte in AH; byte to transfer in AL;
; return combined byte in AL
draw_or proc near private

    or al, ah
    ret

draw_or endp

draw_and proc near private

    and al, ah
    ret

draw_and endp

draw_preset proc near private

    not al
    ret

draw_preset endp

draw_pset proc near private

    ret

draw_pset endp

draw_xor proc near private

    xor al, ah
    ret

draw_xor endp

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
mode_1_NREAD proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    cld

    ; Shift left this many bits
    mov cx, x_coordinate
    shl cl, 1
    and cl, 07h

    ; Copy and shift the bytes:
    mov bx, blit_bytes
    mov dx, ds          ; ES:DI <- DS:blit_addr
    mov es, dx
    mov di, blit_addr
    lds si, video_addr  ; DS:SI <- video_addr
    ; The first byte supplies the initial carry bits
    lodsb
    xor ah, ah
    shl ax, cl
    mov ch, al
    ; Copy the rest
    @copy:
        lodsb
        xor ah, ah
        shl ax, cl
        xchg ah, al
        or al, ch       ; Include the previous carry bits
        stosb
        mov ch, ah      ; Keep the next set of carry bits
    dec bx
    jne @copy
    mov ds, dx          ; point DS to data segment again

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

mode_1_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
mode_1_NWRITE proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    cld

    ; Does the transfer cross a byte boundary?
    mov cx, x_coordinate
    shl cx, 1
    and cx, 07h ; this is the shift count
    mov dx, cx
    add dx, blit_bits
    dec dx
    cmp dx, 8
    jge @multibyte

        ; Transfer lies within a single byte
        ; Build bit mask in DH
        mov bx, cx
        mov dh, left_mask_1[bx]
        mov bx, dx
        and dh, right_mask_1[bx]
        ; Byte to transfer in AL
        mov si, blit_addr
        lodsb
        shr al, cl
        ; Existing byte in AH
        les di, video_addr
        mov ah, es:[di]

        ; Perform the configured operation, leaving combined byte in AL
        call blit_mixer

        ; Keep bits where DH is 0; change bits where DH is 1
        xor al, ah
        and al, dh
        xor al, ah
        stosb

    jmp @end
    @multibyte:

        ; DS:SI <- local memory address
        mov si, blit_addr
        ; ES:DI <- frame buffer
        les di, video_addr

        ; Transfer left byte
        lodsb           ; Byte to write
        mov ah, al
        xor al, al
        shr ax, cl
        mov ch, al      ; Carry bits
        mov al, ah
        mov ah, es:[di] ; Existing byte
        call blit_mixer ; AL <- combined byte
        mov bl, cl
        xor bh, bh
        mov bl, left_mask_1[bx]
        ; Keep bits where BL is 0; change bits where BL is 1
        xor al, ah
        and al, bl
        xor al, ah
        stosb

        ; Transfer zero or more whole bytes
        mov bl, cl
        xor bh, bh
        add bx, blit_bits
        dec bx
        shr bx, 1
        shr bx, 1
        shr bx, 1
        dec bx
        jz @end_bytes
        @bytes:
            lodsb       ; Byte to write
            mov ah, al
            xor al, al
            shr ax, cl
            or ah, ch   ; Carry in
            mov ch, al  ; Carry out
            mov al, ah
            mov ah, es:[di] ; Existing byte
            call blit_mixer ; AL <- combined byte
            stosb
        dec bx
        jnz @bytes
        @end_bytes:

        ; Transfer right byte
        lodsb
        mov ah, al
        xor al, al
        shr ax, cl
        or ah, ch   ; Carry in
        mov al, ah
        mov ah, es:[di] ; Existing byte
        call blit_mixer ; AL <- combined byte
        mov bx, cx
        add bx, blit_bits
        dec bx
        and bx, 07h
        mov bl, right_mask_1[bx]
        ; Keep bits where BL is 0; change bits where BL is 1
        xor al, ah
        and al, bl
        xor al, ah
        stosb

    @end:

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

left_mask_1 db 0FFh, 7Fh, 3Fh, 1Fh, 0Fh, 07h, 03h, 01h
right_mask_1 db 80h, 0C0h, 0E0h, 0F0h, 0F8h, 0FCh, 0FEh, 0FFh

mode_1_NWRITE endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
mode_1_PNTINI proc near private

    ; Duplicate lower two bits through the entire byte
    push bx
    and ax, 03h
    mov bx, ax
    mov al, bit_dup[bx]
    pop bx
    mov border_attr, al
    clc
    ret

mode_1_PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
; Modes 1 and 2 both use this
cga_TDOWNC proc near private

    push ax

    mov ax, video_pos
    cmp ax, 3EF0h
    jae @error
    add ax, 2000h
    cmp ax, 4000h
    jb @F
        sub ax, 4000h - 80
    @@:
    mov video_pos, ax

    pop ax
    clc
    ret

@error:
    pop ax
    stc
    ret

cga_TDOWNC endp

; Move current position up with boundary check
; Returns: C set if moving up would pass the top of the screen;
;          the current position is unchanged in that case
; This differs from UPC only in the boundary check
; Modes 1 and 2 both use this
cga_TUPC proc near private

    push ax

    mov ax, video_pos
    cmp ax, 80
    jb @error
    sub ax, 2000h
    jnc @F
        add ax, 4000h - 80
    @@:
    mov video_pos, ax

    pop ax
    clc
    ret

@error:
    pop ax
    stc
    ret

cga_TUPC endp

; On entry: Setup done with PNTINI
;           DX = number of border pixels to skip right
;           No pixels are painted if this many pixels in the border color
;           are found
; Returns:  BX = number of pixels painted
;           DX reduced by number of border pixels skipped
;           CL != 0 if at least one pixel changed
;           CSAVEA and CSAVEM set to the point where drawing began, in the
;           format returned by FETCHC
;           Current position updated
mode_1_SCANR proc near private

    push ax
    push si
    push di
    push bp
    push es

    ; Skip right
    mov cl, 2
    mov bp, 319
    mov bx, dx
    les di, video_addr
    mov dh, border_attr
    mov dl, video_bitmask
    mov si, x_coordinate
    cmp bx, 0
    je @paint_not_found
    @border_scan:
        ; End loop if a non-border pixel is found
        mov al, es:[di]
        xor al, dh          ; border_attr
        and al, dl          ; video_bitmask
        jne @paint_found
        ; Go to next pixel
        cmp si, bp
        jae @paint_not_found
        inc si              ; x_coordinate
        ror dl, cl
        jnc @F
            inc di
        @@:
    dec bx
    jnz @border_scan
    @paint_not_found:
        ; No matching pixel found
        mov video_bitmask, dl
        mov video_pos, di
        mov x_coordinate, si
        xor bx, bx ; Nothing painted
        xor cl, cl
        xor dx, dx ; No border pixels remain
        jmp @end

    @paint_found:

    ; Set position where drawing begins
    mov CSAVEA, di
    mov CSAVEM, dl
    push bx             ; remaining border pixel count
    xor ch, ch
    xor bx, bx          ; number of pixels painted

    @paint:
        ; End loop if a border pixel is found
        mov al, es:[di]
        mov ah, al
        xor al, dh          ; border_attr
        and al, dl          ; video_bitmask
        je @end_paint
        ; Set the new pixel
        mov al, graph_attr
        xor al, ah
        and al, dl          ; video_bitmask
        or ch, al           ; nonzero if a pixel changed
        xor al, ah
        mov es:[di], al
        ; Go to next pixel
        cmp si, bp
        jae @end_paint
        inc si              ; x_coordinate
        ror dl, cl
        inc bx
        jnc @paint
            inc di
    jmp @paint
    @end_paint:

    mov cl, ch          ; nonzero if any pixel changed
    mov x_coordinate, si
    mov video_pos, di
    mov video_bitmask, dl
    pop dx              ; remaining border pixel count

@end:
    pop es
    pop bp
    pop di
    pop si
    pop ax
    ret

mode_1_SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
mode_1_SCANL proc near private

    push ax
    push dx
    push si
    push di
    push es

    mov cl, 2
    les di, video_addr
    mov dl, video_bitmask
    mov dh, border_attr
    mov si, x_coordinate
    xor ch, ch
    xor bx, bx

    @paint:
        ; Go to next pixel
        rol dl, cl
        jnc @F
            dec di
        @@:
        dec si          ; x_coordinate
        js @end_paint
        ; End loop if a border pixel is found
        mov al, es:[di]
        mov ah, al
        xor al, dh      ; border_attr
        and al, dl      ; video_bitmask
        je @end_paint
        ; Set the new pixel
        mov al, graph_attr
        xor al, ah
        and al, dl      ; video_bitmask
        or ch, al       ; nonzero if a pixel changed
        xor al, ah
        cmp al, ah
        mov es:[di], al
        inc bx
    jmp @paint
    @end_paint:
    ; Move back one pixel to the right
    inc si              ; x_coordinate
    ror dl, cl
    jnc @F
        inc di
    @@:
    mov cl, ch              ; At least one pixel changed
    mov video_pos, di
    mov video_bitmask, dl
    mov x_coordinate, si

    pop es
    pop di
    pop si
    pop dx
    pop ax
    ret

mode_1_SCANL endp

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
DONOTE proc near

    ; TODO: The speaker is not yet implemented, but SNDRST calls this
    ret

DONOTE endp

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

extrn TOPMEM:word
extrn CSAVEA:word
extrn CSAVEM:byte

; Blink state saved at init, to restore on exit
blink_flag db ?

; Function key format returned by FKYFMT
fkey_format db 3 dup (?)

; Pointer to screen mode structure
mode_ptr dw ?

; Keep video_addr, video_pos and video_seg together
; Label so we can use LES to load the graphics position
video_addr label dword
; Offset from frame buffer to graphics position
video_pos dw ?
; Segment of video frame buffer
video_seg dw ?

; Bit mask of current graphics position
video_bitmask db ?

; X coordinate for certain graphics routines
x_coordinate dw ?

; Width of text screen in columns
text_width db ?

; Shape of text cursor set by CSRDSP
cursor_shape dw 0B0Dh ; Initially in overwrite mode

; Colors set by SETFBC and SETCLR and returned by GETFBC
foreground_color db 7
background_color db 0
border_color db 0
text_attr db 07h

; Color set by SETATR
graph_attr db 0

; Color set by PNTINI
border_attr db 0

; Maximum line for SCROLL
max_line db 0

; Active page (where we draw) and visible page (what we see)
active_page db 0
visible_page db 0

; Bit-blit parameters
blit_addr dw ?
blit_bits dw ?
blit_bytes dw ?
blit_mixer dw ?

DSEG ends

END
