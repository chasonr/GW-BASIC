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
; For now, text mode is 40x25 or 80x25.
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
    xor al, al
    call set_screen_mode

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
    bios_mode      db ? ; BIOS video mode
Screen_Mode ends

; Table of supported screen modes
mode_table label word
    dw offset screen_mode_0  ; 40x25 or 80x25 text
    dw offset screen_mode_1  ; CGA 320x200, 4 colors
    dw offset screen_mode_2  ; CGA 640x200, 2 colors
    dw 0                     ; Hercules 720x348, 2 colors
    dw 0                     ; Olivetti; properties unknown
    dw 0                     ; undefined
    dw 0                     ; undefined
    dw offset screen_mode_7  ; EGA 320x200, 16 colors
    dw offset screen_mode_8  ; EGA 640x200, 16 colors
    dw offset screen_mode_9  ; EGA 640x350, 16 colors
    dw 0                     ; EGA 640x350, monochrome
    dw 0                     ; VGA 640x480, monochrome
    dw offset screen_mode_12 ; VGA 640x480, 16 colors
    dw offset screen_mode_13 ; VGA 320x200, 256 colors
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
    dw mode_0_SCROLL          ; SCROLL_handler
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
    db 3                      ; bios_mode (actually 0-3 or 7)

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
    dw cga_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw mode_1_SETCLR          ; SETCLR_handler
    dw mode_1_SWIDTH          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw mode_1_SETFBC          ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw cga_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw cga_UPC                ; UPC_handler
    dw cga_DOWNC              ; DOWNC_handler
    dw cga_LEFTC              ; LEFTC_handler
    dw cga_RIGHTC             ; RIGHTC_handler
    dw cga_MAPXYC             ; MAPXYC_handler
    dw mode_1_SETATR          ; SETATR_handler
    dw cga_READC              ; READC_handler
    dw cga_SETC               ; SETC_handler
    dw cga_NSETCX             ; NSETCX_handler
    dw cga_PGINIT             ; PGINIT_handler
    dw cga_NREAD              ; NREAD_handler
    dw cga_NWRITE             ; NWRITE_handler
    dw mode_1_PNTINI          ; PNTINI_handler
    dw cga_TDOWNC             ; TDOWNC_handler
    dw cga_TUPC               ; TUPC_handler
    dw cga_SCANR              ; SCANR_handler
    dw cga_SCANL              ; SCANL_handler
    db 40                     ; text_columns
    db 25                     ; text_rows
    dw 320                    ; x_res
    dw 200                    ; y_res
    dw 00D5h                  ; width_height (0.833)
    dw 0133h                  ; height_width (1.200)
    db 1                      ; num_pages
    db 2                      ; pixel_size
    db 5                      ; bios_mode (actually 4 or 5)

screen_mode_2 label word
    dw mode_2_SCRSTT_init     ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw cga_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw cga_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw mode_2_SETCLR          ; SETCLR_handler
    dw mode_2_SWIDTH          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw graphics_stub          ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw cga_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw cga_UPC                ; UPC_handler
    dw cga_DOWNC              ; DOWNC_handler
    dw cga_LEFTC              ; LEFTC_handler
    dw cga_RIGHTC             ; RIGHTC_handler
    dw cga_MAPXYC             ; MAPXYC_handler
    dw mode_2_SETATR          ; SETATR_handler
    dw cga_READC              ; READC_handler
    dw cga_SETC               ; SETC_handler
    dw cga_NSETCX             ; NSETCX_handler
    dw cga_PGINIT             ; PGINIT_handler
    dw cga_NREAD              ; NREAD_handler
    dw cga_NWRITE             ; NWRITE_handler
    dw mode_2_PNTINI          ; PNTINI_handler
    dw cga_TDOWNC             ; TDOWNC_handler
    dw cga_TUPC               ; TUPC_handler
    dw cga_SCANR              ; SCANR_handler
    dw cga_SCANL              ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 200                    ; y_res
    dw 006Bh                  ; width_height (0.4167)
    dw 0266h                  ; height_width (2.4000)
    db 1                      ; num_pages
    db 1                      ; pixel_size
    db 6                      ; bios_mode

screen_mode_7 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw ega_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw ega_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw ega_SETCLR             ; SETCLR_handler
    dw ega_SWIDTH_80          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw ega_SETFBC             ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw ega_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw ega_UPC                ; UPC_handler
    dw ega_DOWNC              ; DOWNC_handler
    dw ega_LEFTC              ; LEFTC_handler
    dw ega_RIGHTC             ; RIGHTC_handler
    dw ega_MAPXYC             ; MAPXYC_handler
    dw ega_SETATR             ; SETATR_handler
    dw ega_READC              ; READC_handler
    dw ega_SETC               ; SETC_handler
    dw ega_NSETCX             ; NSETCX_handler
    dw ega_PGINIT             ; PGINIT_handler
    dw ega_NREAD              ; NREAD_handler
    dw ega_NWRITE             ; NWRITE_handler
    dw ega_PNTINI             ; PNTINI_handler
    dw ega_TDOWNC             ; TDOWNC_handler
    dw ega_TUPC               ; TUPC_handler
    dw ega_SCANR              ; SCANR_handler
    dw ega_SCANL              ; SCANL_handler
    db 40                     ; text_columns
    db 25                     ; text_rows
    dw 320                    ; x_res
    dw 200                    ; y_res
    dw 00D5h                  ; width_height (0.833)
    dw 0133h                  ; height_width (1.200)
    db 1                      ; num_pages
    db 4                      ; pixel_size
    db 0Dh                    ; bios_mode

screen_mode_8 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw ega_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw ega_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw ega_SETCLR             ; SETCLR_handler
    dw ega_SWIDTH_40          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw ega_SETFBC             ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw ega_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw ega_UPC                ; UPC_handler
    dw ega_DOWNC              ; DOWNC_handler
    dw ega_LEFTC              ; LEFTC_handler
    dw ega_RIGHTC             ; RIGHTC_handler
    dw ega_MAPXYC             ; MAPXYC_handler
    dw ega_SETATR             ; SETATR_handler
    dw ega_READC              ; READC_handler
    dw ega_SETC               ; SETC_handler
    dw ega_NSETCX             ; NSETCX_handler
    dw ega_PGINIT             ; PGINIT_handler
    dw ega_NREAD              ; NREAD_handler
    dw ega_NWRITE             ; NWRITE_handler
    dw ega_PNTINI             ; PNTINI_handler
    dw ega_TDOWNC             ; TDOWNC_handler
    dw ega_TUPC               ; TUPC_handler
    dw ega_SCANR              ; SCANR_handler
    dw ega_SCANL              ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 200                    ; y_res
    dw 006Bh                  ; width_height (0.4167)
    dw 0266h                  ; height_width (2.4000)
    db 1                      ; num_pages
    db 4                      ; pixel_size
    db 0Eh                    ; bios_mode

screen_mode_9 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw ega_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw ega_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw ega_SETCLR             ; SETCLR_handler
    dw ega_SWIDTH_40          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw ega_SETFBC             ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw ega_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw ega_UPC                ; UPC_handler
    dw ega_DOWNC              ; DOWNC_handler
    dw ega_LEFTC              ; LEFTC_handler
    dw ega_RIGHTC             ; RIGHTC_handler
    dw ega_MAPXYC             ; MAPXYC_handler
    dw ega_SETATR             ; SETATR_handler
    dw ega_READC              ; READC_handler
    dw ega_SETC               ; SETC_handler
    dw ega_NSETCX             ; NSETCX_handler
    dw ega_PGINIT             ; PGINIT_handler
    dw ega_NREAD              ; NREAD_handler
    dw ega_NWRITE             ; NWRITE_handler
    dw ega_PNTINI             ; PNTINI_handler
    dw ega_TDOWNC             ; TDOWNC_handler
    dw ega_TUPC               ; TUPC_handler
    dw ega_SCANR              ; SCANR_handler
    dw ega_SCANL              ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 350                    ; y_res
    dw 00BBh                  ; width_height (0.729)
    dw 015Fh                  ; height_width (1.200)
    db 1                      ; num_pages
    db 4                      ; pixel_size
    db 10h                    ; bios_mode

screen_mode_12 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw ega_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw ega_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw ega_SETCLR             ; SETCLR_handler
    dw ega_SWIDTH_40          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw ega_SETFBC             ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw ega_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw ega_UPC                ; UPC_handler
    dw ega_DOWNC              ; DOWNC_handler
    dw ega_LEFTC              ; LEFTC_handler
    dw ega_RIGHTC             ; RIGHTC_handler
    dw ega_MAPXYC             ; MAPXYC_handler
    dw ega_SETATR             ; SETATR_handler
    dw ega_READC              ; READC_handler
    dw ega_SETC               ; SETC_handler
    dw ega_NSETCX             ; NSETCX_handler
    dw ega_PGINIT             ; PGINIT_handler
    dw ega_NREAD              ; NREAD_handler
    dw ega_NWRITE             ; NWRITE_handler
    dw ega_PNTINI             ; PNTINI_handler
    dw ega_TDOWNC             ; TDOWNC_handler
    dw ega_TUPC               ; TUPC_handler
    dw ega_SCANR              ; SCANR_handler
    dw ega_SCANL              ; SCANL_handler
    db 80                     ; text_columns
    db 30                     ; text_rows
    dw 640                    ; x_res
    dw 480                    ; y_res
    dw 0100h                  ; width_height (1.0)
    dw 0100h                  ; height_width (1.0)
    db 1                      ; num_pages
    db 4                      ; pixel_size
    db 12h                    ; bios_mode

screen_mode_13 label word
    dw vga_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw generic_SCROLL         ; SCROLL_handler
    dw vga_CLRSCN             ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw vga_CSRDSP             ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw vga_SETCLR             ; SETCLR_handler
    dw graphics_stub          ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw vga_SETFBC             ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw vga_STOREC             ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw vga_UPC                ; UPC_handler
    dw vga_DOWNC              ; DOWNC_handler
    dw vga_LEFTC              ; LEFTC_handler
    dw vga_RIGHTC             ; RIGHTC_handler
    dw vga_MAPXYC             ; MAPXYC_handler
    dw vga_SETATR             ; SETATR_handler
    dw vga_READC              ; READC_handler
    dw vga_SETC               ; SETC_handler
    dw vga_NSETCX             ; NSETCX_handler
    dw vga_PGINIT             ; PGINIT_handler
    dw vga_NREAD              ; NREAD_handler
    dw vga_NWRITE             ; NWRITE_handler
    dw vga_PNTINI             ; PNTINI_handler
    dw vga_TDOWNC             ; TDOWNC_handler
    dw vga_TUPC               ; TUPC_handler
    dw vga_SCANR              ; SCANR_handler
    dw vga_SCANL              ; SCANL_handler
    db 40                     ; text_columns
    db 25                     ; text_rows
    dw 320                    ; x_res
    dw 200                    ; y_res
    dw 00D5h                  ; width_height (0.833)
    dw 0133h                  ; height_width (1.200)
    db 1                      ; num_pages
    db 8                      ; pixel_size
    db 13h                    ; bios_mode

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
        call set_screen_mode
        jc @error
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

; On entry: AL = screen mode to set
; Returns: C set if error
;          if C clear: DI points to new mode table
set_screen_mode proc near private

    push cx
    push di

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
    mov  cl,text_height
    call SCNSWI
    call GRPINI
    call SCNCLR
    clc

    pop cx ; Discard saved DI
    pop cx
    ret

@error:
    pop di
    pop cx
    stc
    ret

set_screen_mode endp

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
    push dx
    push di

    ; Address parameters from SI
    mov di, bx

    ; Get number of parameters
    mov al, [di]

    ; Mark parameters as unspecified
    xor bx, bx
    xor cx, cx
    xor dx, dx

    dec al
    stc
    js @error   ; Must have at least one
        mov bx, 1[di]

    dec al
    js @end
        mov cx, 3[di]

    dec al
    js @end
        mov dx, 5[di]

    @end:
    ; Go to mode-specific handler
    mov di, mode_ptr
    call cs:Screen_Mode.SETCLR_handler[di]

@error:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
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
        mov cl, text_height
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
    mov bl, text_attr
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
        mov al, text_height
        sub al, 2
        mov max_line, al
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
            mov al, text_height
            dec al
            mov max_line, al
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

    cmp dh, text_height
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
    mov text_height, 25
    mov foreground_color, 7
    mov background_color, 0
    call mode_0_set_text_attr

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

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
mode_0_SCROLL proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    ; Convert coordinates to 0-based
    dec ah
    dec al
    dec bh
    dec bl

    ; Set up the scrolling:
    ; Save the registers
    mov scroll_c1, ah
    mov scroll_r1, al
    mov scroll_c2, bh
    mov scroll_r2, bl
    mov scroll_width, ch
    mov scroll_height, cl

    ; Convert coordinates to addresses:
    mov bh, active_page
    rol bh, 1
    rol bh, 1
    rol bh, 1
    rol bh, 1
    mov cl, text_width
    ; convert scroll_from
    mov al, scroll_r1
    mul cl
    mov bl, scroll_c1
    add ax, bx
    shl ax, 1
    mov scroll_from, ax
    ; convert scroll_to
    mov al, scroll_r2
    mul cl
    mov bl, scroll_c2
    add ax, bx
    shl ax, 1
    mov scroll_to, ax

    ; Address the frame buffer
    mov es, video_seg

    ; Which way are we scrolling?
    cmp ax, scroll_from
    jae @scroll_higher
        ; Scroll from higher to lower address
        cld
        mov dl, scroll_height
        @scroll_1:
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsw es:[di], es:[si]
            mov al, text_width
            xor ah, ah
            shl ax, 1
            add scroll_from, ax
            add scroll_to, ax
        dec dl
        jnz @scroll_1
    jmp @scroll_end
    @scroll_higher:
        ; Scroll from lower to higher address
        ; Point starting addresses to last cell to scroll
        mov al, scroll_height   ; AX <- (((scroll_height-1)*text_width)+(scroll_width-1))*2
        dec al
        mul text_width
        add al, scroll_width
        adc ah, 0
        dec ax
        shl ax, 1
        add scroll_from, ax
        add scroll_to, ax
        ; Begin loop
        std
        mov dl, scroll_height
        @scroll_2:
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsw es:[di], es:[si]
            mov al, text_width
            xor ah, ah
            shl ax, 1
            sub scroll_from, ax
            sub scroll_to, ax
        dec dl
        jnz @scroll_2
    @scroll_end:

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

mode_0_SCROLL endp

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
; On entry: BL != 0 if parameter 1 specified
;           BH contains parameter 1
;           CL != 0 if parameter 2 specified
;           CH contains parameter 1
;           DL != 0 if parameter 3 specified
;           DH contains parameter 3
; Returns: C set if error
mode_0_SETCLR proc near private

    test bl, bl
    je @F
        mov foreground_color, bh
    @@:

    test cl, cl
    je @F
        mov background_color, ch
    @@:

    test dl, dl
    je @F
        push ax
        push bx
        mov bl, dh  ; border color
        mov bh, 0
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    call mode_0_set_text_attr
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
    cmp dl, text_height
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
    mov text_height, 25
    mov text_attr, 3
    mov cursor_pos, 0FFFFh
    mov video_seg, 0B800h
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
cga_CLRSCN proc near private

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

        mov cursor_pos, 0FFFFh

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

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
; Returns: none
; Modes 1 and 2 both use this
cga_CSRDSP proc near private

    push ax
    push bx
    push dx
    push si
    push es

    ; Set BX to flip one byte
    mov bx, 00FFh

    ; Develop the address of the text cell
    push ax
    xchg dh, dl
    dec dl  ; 0-based column
    dec dh  ; 0-based row
    ; DL to byte offset
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 1
        shl dl, 1
        ; Flip two bytes
        mov bx, 0FFFFh
    @@:

    ; AX <- DH * 320
    mov al, 160
    mul dh
    shl ax, 1
    xor dh, dh
    add ax, dx
    mov si, ax  ; SI = address
    pop ax
    mov es, video_seg

    ; Remove any existing cursor
    call cga_cursor_off

    cmp al, 0
    je @end

        ; Set the cursor shape
        cmp al, 1
        jne @two
            mov ax, 0007h
            jmp @on
        @two:
        cmp al, 2
        jne @three
            mov ax, 0607h
            jmp @on
        @three:
            mov ax, cursor_shape
        @on:

        ; Display the new cursor
        call cga_cursor_flip
        mov cursor_pos, si
        mov cursor_state, ax

    @end:

    pop es
    pop si
    pop dx
    pop bx
    pop ax
    ret

cga_CSRDSP endp

; Turn off the cursor
cga_cursor_off proc near private

    cmp cursor_pos, 0FFFFh
    je @end ; it isn't already off
        push ax
        push si
        push es

        mov es, video_seg
        mov si, cursor_pos
        mov ax, cursor_state
        call cga_cursor_flip

        pop es
        pop si
        pop ax
        mov cursor_pos, 0FFFFh
    @end:

    ret

cga_cursor_off endp

; Reverse the area of text that the cursor occupies
; On entry: AH = start line; AL = end_line
;           ES:SI = address of line 0 of the text cell
;           BX = FFFF for mode 1, 00FF for mode 2
cga_cursor_flip proc near private

    push cx
    push si

    mov cl, 0
    @flip:

        ; Flip even line
        cmp cl, ah
        jb @F
        cmp cl, al
        ja @F
            xor word ptr es:[si], bx
        @@:
        inc cl

        ; Advance to odd line
        add si, 2000h

        ; Flip odd line
        cmp cl, ah
        jb @F
        cmp cl, al
        ja @F
            xor word ptr es:[si], bx
        @@:
        inc cl

        ; Advance to even line
        sub si, 2000h - 80

    cmp cl, 8
    jb @flip

    pop si
    pop cx
    ret

cga_cursor_flip endp

; Set up configured colors
; On entry: BL != 0 if parameter 1 specified
;           BH contains parameter 1
;           CL != 0 if parameter 2 specified
;           CH contains parameter 1
;           DL != 0 if parameter 3 specified
;           DH contains parameter 3
; Returns: C set if error
mode_1_SETCLR proc near private

    or cl, cl
    je @F
        ; Mode 1 palette selection
        push ax
        push bx
        mov bl, ch
        and bl, 1
        mov bh, 1
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    or bl, bl
    je @F
        ; Mode 1 background color
        push ax
        push bx
        mov bl, bh
        xor bl, 10h ; Keep the foreground in bright colors
        mov bh, 0
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    clc
    ret

mode_1_SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
mode_1_SWIDTH proc near private

    cmp AL, 80
    je @80_columns
    cmp AL, 40
    je @40_columns
        ; anything else is an error
        stc
        ret

    @40_columns:
        clc
        ret

    @80_columns:
        ; Switch to mode 2
        mov al, 2
        call set_screen_mode
        ret

mode_1_SWIDTH endp

; Set foreground and background colors
; On entry: foreground_color and background_color set
; Returns: none
mode_1_SETFBC proc near private

    ret

mode_1_SETFBC endp

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
; Modes 1 and 2 both use this
cga_STOREC proc near private

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
    ; For mode 2
    cmp cs:Screen_Mode.pixel_size[di], 1
    jne @F
    test al, 55h
    je @F
        or dx, 1
    @@:

    mov x_coordinate, dx

    pop dx
    pop ax
    ret

cga_STOREC endp

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
; Modes 1 and 2 both use this
cga_LEFTC proc near private

    push ax
    push cx

    dec x_coordinate

    ; Shift the bit mask left
    mov al, video_bitmask
    mov cl,cs:Screen_Mode.pixel_size[di]
    rol al, cl
    mov video_bitmask, al

    ; Carry into the address
    jnc @F
        dec video_pos
    @@:

    pop cx
    pop ax
    ret

cga_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
; Modes 1 and 2 both use this
cga_RIGHTC proc near private

    push ax
    push cx

    inc x_coordinate

    ; Shift the bit mask right
    mov al, video_bitmask
    mov cl,cs:Screen_Mode.pixel_size[di]
    ror al, cl
    mov video_bitmask, al

    ; Carry into the address
    jnc @F
        inc video_pos
    @@:

    pop cx
    pop ax
    ret

cga_RIGHTC endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
; Modes 1 and 2 both use this
cga_MAPXYC proc near private

    push ax
    push bx
    push cx
    push si

    mov x_coordinate, cx

    ; Bit 1 of the Y coordinate goes to bit 13 of the address
    xor si, si
    shr dl, 1
    rcr si, 1
    shr si, 1
    shr si, 1

    ; Rest of the Y coordinate times 80
    mov al, 80
    mul dl
    add si, ax

    ; X coordinate divided by 4 or 8
    mov ax, cx
    shr ax, 1
    shr ax, 1
    cmp cs:Screen_Mode.pixel_size[di], 1
    jne @F
        shr ax, 1
    @@:
    add si, ax

    ; Bit mask from X mod 4 or 8
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @mode_2
        ; Mode 1
        and cl, 03h
        shl cl, 1
        mov al, 0C0h
    jmp @end_mode
    @mode_2:
        ; Mode 2
        and cl, 07h
        mov al, 080h
    @end_mode:
    shr al, cl

    ; Save the graphics cursor position
    mov video_pos, si
    mov video_bitmask, al

    pop si
    pop dx
    pop cx
    pop ax
    ret

cga_MAPXYC endp

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
; Modes 1 and 2 both use this
cga_READC proc near private

    push bx
    push cx
    push es

    ; Address in frame buffer
    les bx, video_addr

    ; Current pixels
    mov al, es:[bx]

    ; Shift count
    mov cx, x_coordinate
    xor cl, 07h
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @mode_2
        ; Mode 1
        shl cl, 1
        mov ah, 03h
    jmp @end_mode
    @mode_2:
        ; Mode 2
        mov ah, 01h
    @end_mode:
    and cl, 07h

    ; Shift the bits into place
    shr al, cl
    and al, ah

    pop es
    pop cx
    pop bx
    ret

cga_READC endp

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
; Modes 1 and 2 both use this
cga_NSETCX proc near private

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
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 1
        shl bx, 1
        shl dx, 1
    @@:
    cmp bx, 640
    jb @F
        mov bx, 640
    @@:
    dec bx

    ; Does the draw cross a byte boundary?
    mov ax, bx
    xor ax, dx
    and ax, 0FFF8h
    jnz @multibyte

        ; Draw lies within a single byte
        mov si, dx
        and si, 07h
        mov cl, left_mask_1[si]
        mov si, bx
        and si, 07h
        and cl, right_mask_1[si] ; CL has 1 where bits will be replaced
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
        and si, 07h
        mov cl, left_mask_1[si]
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
        and si, 07h
        mov cl, 3
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
        mov cl, right_mask_1[si]
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

cga_NSETCX endp

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
; Modes 1 and 2 both use this
cga_PGINIT proc near private

    mov blit_addr, bx
    mov blit_bits, cx
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
    add cx, 7
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov blit_bytes, cx
    mov cx, blit_bits
    ret

cga_PGINIT endp

draw_table dw draw_or
           dw draw_and
           dw draw_preset
           dw draw_pset
           dw draw_xor

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
; Modes 1 and 2 both use this
cga_NREAD proc near private

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
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 1
        shl cl, 1
    @@:
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

cga_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
; Modes 1 and 2 both use this
cga_NWRITE proc near private

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
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 1
        shl cl, 1
    @@:
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

cga_NWRITE endp

left_mask_1 db 0FFh, 7Fh, 3Fh, 1Fh, 0Fh, 07h, 03h, 01h
right_mask_1 db 80h, 0C0h, 0E0h, 0F0h, 0F8h, 0FCh, 0FEh, 0FFh

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
; Modes 1 and 2 both use this
cga_SCANR proc near private

    push ax
    push si
    push di
    push bp
    push es

    ; Skip right
    mov cl, cs:Screen_Mode.pixel_size[di]
    cmp cl, 1
    je @mode_2
        ; Mode 1
        mov bp, 319
    jmp @end_mode
    @mode_2:
        ; Mode 2
        mov bp, 639
    @end_mode:
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

cga_SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
; Modes 1 and 2 both use this
cga_SCANL proc near private

    push ax
    push dx
    push si
    push di
    push es

    mov cl, cs:Screen_Mode.pixel_size[di]
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

cga_SCANL endp
if 0
;#############################################################################
;RLC Create debug logs
;#############################################################################
debug_handle dw ?
debug_name db "DEBUG.TXT"
debug_buf db 32 dup (?)
debug_open proc near private
    push ax
    push bx
    push cx
    push dx
    push ds

    ; Open as existing file
    mov ax, cs
    mov ds, ax
    mov dx, offset debug_name
    mov ax, 3D01h
    int 21h
    jnc @file_ok
        ; Try to create the file
        mov dx, offset debug_name
        xor cx, cx
        mov dx, offset debug_name
        mov ax, 3C00h
        int 21h
        jc @error
    @file_ok:
    mov debug_handle, ax
    ; Seek to end
    mov bx, ax
    xor cx, cx
    xor dx, dx
    mov ax, 4202h
    int 21h

@end:
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
    ret

@error:
    mov debug_handle, -1
    jmp @end

debug_open endp

debug_close proc
    push ax
    push bx

    mov bx, debug_handle
    cmp bx, -1
    je @end

    mov ah, 3Eh
    int 21h

    @end:
    pop bx
    pop ax
    ret
debug_close endp

debug_write proc
    cmp debug_handle, -1
    jne @F
        ret
    @@:
    mov word ptr debug_buf+2, ax
    mov word ptr debug_buf+4, bx
    mov word ptr debug_buf+6, cx
    mov word ptr debug_buf+8, dx
    mov word ptr debug_buf+0Ah, bp
    mov word ptr debug_buf+0Ch, si
    mov word ptr debug_buf+0Eh, di
    mov word ptr debug_buf+10h, ds
    mov word ptr debug_buf+12h, es

    ; Return address
    pop ax
    mov word ptr debug_buf+0, ax
    push ax

    mov ax, video_pos
    mov word ptr debug_buf+14h, ax
    mov al, video_bitmask
    mov byte ptr debug_buf+16h, al
    mov ax, CSAVEA
    mov word ptr debug_buf+17h, ax
    mov al, CSAVEM
    mov byte ptr debug_buf+19h, al
    mov ax, cursor_state
    mov word ptr debug_buf+1Ah, ax
    mov ax, cursor_pos
    mov word ptr debug_buf+1Ch, ax

    mov bx, debug_handle
    mov ax, cs
    mov ds, ax
    mov dx, offset debug_buf
    mov cx, 1Eh
    mov ah, 40h
    int 21h

    mov ax, word ptr debug_buf+2
    mov bx, word ptr debug_buf+4
    mov cx, word ptr debug_buf+6
    mov dx, word ptr debug_buf+8
    mov bp, word ptr debug_buf+0Ah
    mov si, word ptr debug_buf+0Ch
    mov di, word ptr debug_buf+0Eh
    mov ds, word ptr debug_buf+10h
    mov es, word ptr debug_buf+12h

    ret
debug_write endp

;#############################################################################
;RLC End debug logs
;#############################################################################
endif
;-----------------------------------------------------------------------------
; Screen mode 2: CGA 640x200, 2 colors
;-----------------------------------------------------------------------------
; Most of this is shared with mode 1

; Set up mode 2
mode_2_SCRSTT_init proc near private

    ; Set BIOS mode 6
    mov ax, 0006h
    int 10h

    ; Did it work?
    push bx
    mov ah, 0Fh
    int 10h
    pop bx
    cmp al, 6
    jne @error

    ; We're good.
    mov text_width, 80
    mov text_height, 25
    mov text_attr, 1
    mov video_seg, 0B800h
    clc
    ret

    ; Oops.
    @error:
    stc
    ret

mode_2_SCRSTT_init endp

; Set up configured colors
; On entry: BL != 0 if parameter 1 specified
;           BH contains parameter 1
;           CL != 0 if parameter 2 specified
;           CH contains parameter 1
;           DL != 0 if parameter 3 specified
;           DH contains parameter 3
; Returns: C set if error
mode_2_SETCLR proc near private

    ; Original GW-BASIC doesn't allow COLOR in mode 2, but I see no need to
    ; restrict the user here
    or bl, bl
    je @F
        ; Mode 1 background color
        push ax
        push bx
        mov bl, bh
        xor bl, 10h ; Keep the foreground in bright colors
        mov bh, 0
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    clc
    ret

mode_2_SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
mode_2_SWIDTH proc near private

    cmp AL, 80
    je @80_columns
    cmp AL, 40
    je @40_columns
        ; anything else is an error
        stc
        ret

    @40_columns:
        ; Switch to mode 1
        mov al, 1
        call set_screen_mode
        ret

    @80_columns:
        clc
        ret

mode_2_SWIDTH endp

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  C set if error
mode_2_SETATR proc near private

    ; Duplicate lower bit through the entire byte
    and al, 1
    neg al
    mov graph_attr, al
    clc
    ret

mode_2_SETATR endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
mode_2_PNTINI proc near private

    ; Duplicate lower bit through the entire byte
    and al, 1
    neg al
    mov border_attr, al
    clc
    ret

mode_2_PNTINI endp

;-----------------------------------------------------------------------------
; Screen mode  7: EGA 320x200, 16 colors
; Screen mode  8: EGA 640x200, 16 colors
; Screen mode  9: EGA 640x350, 16 colors
; Screen mode 10: EGA 640x350, monochrome
; Screen mode 11: VGA 640x480, monochrome
; Screen mode 12: VGA 640x480, 16 colors
;-----------------------------------------------------------------------------

; Macro to set an indexed EGA/VGA register
; This overwrites AL and DX
write_ega_reg macro port, index, value
    mov dx, port
    mov al, index
    out dx, al
    jmp short $+2
    inc dx
    mov al, value
    out dx, al
    jmp short $+2
endm

; Macro to read an indexed EGA/VGA register
; This overwrites AL and DX
read_ega_reg macro port, index
    mov dx, port
    mov al, index
    out dx, al
    jmp $+2
    inc dx
    in al, dx
    jmp $+2
endm

; Set an EGA/VGA planar mode
ega_SCRSTT_init proc near private

    ; Set the correct BIOS mode
    mov al, cs:Screen_Mode.bios_mode[di]
    mov ah, 00h
    int 10h

    ; Did it work?
    push bx
    mov ah, 0Fh
    int 10h
    pop bx
    cmp al, cs:Screen_Mode.bios_mode[di]
    jne @error

    ; We're good.
    mov al, cs:Screen_Mode.text_columns[di]
    mov text_width, al
    mov al, cs:Screen_Mode.text_rows[di]
    mov text_height, al
    mov cursor_pos, 0FFFFh
    mov video_seg, 0A000h
    mov text_attr, 15
    ; Select read mode 0 and write mode 0
    push dx
    write_ega_reg 03CEh, 5, 0
    pop dx
    mov ega_set_pixel, 1
    clc
    ret

    ; Oops.
    @error:
    stc
    ret

ega_SCRSTT_init endp

; Print a character to the screen
; On entry:
;     AL = character
;     DH = column (1 left)
;     DL = row (1 top)
; Returns C clear for success
; No caller checks the C flag
ega_SCROUT proc near private

    mov ega_set_pixel, 1
    jmp generic_SCROUT

ega_SCROUT endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
ega_CLRSCN proc near private

    cmp al, 0
    stc
    jne @end

        push ax
        push cx
        push dx
        push es

        write_ega_reg 03CEh, 1, 0       ; No set/reset
        write_ega_reg 03CEh, 3, 000o    ; replace operation, 0 bits rotation
        write_ega_reg 03CEh, 8, 0FFh    ; Write all bits
        write_ega_reg 03C4h, 2, 0Fh     ; Write all planes

        ; Determine the size to fill
        mov ax, cs:Screen_Mode.x_res[di]
        mov cl, 4
        shr ax, cl
        mul cs:Screen_Mode.y_res[di]
        mov cx, ax

        ; Fill
        mov es, video_seg
        xor di, di
        xor ax, ax
        cld
        rep stosw

        mov cursor_pos, 0FFFFh

        call SCNCLR
        call GRPINI
        mov ega_set_pixel, 1

        pop es
        pop dx
        pop cx
        pop ax

        clc

    @end:
    ret

ega_CLRSCN endp

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
; Returns: none
ega_CSRDSP proc near private

    push ax
    push bx
    push dx
    push si
    push es

    ; Get the font height
    ; TODO: This should be configurable
    push ax
    mov ax, cs:Screen_Mode.y_res[di]
    mov bl, cs:Screen_Mode.text_rows[di]
    div bl
    mov bl, al
    pop ax

    ; Develop the address of the text cell
    push ax
    xchg dh, dl
    dec dl  ; 0-based column
    dec dh  ; 0-based row
    mov ax, cs:Screen_Mode.x_res[di] ; AL <- x_res/8
    shr ax, 1
    shr ax, 1
    shr ax, 1
    mul dh
    xor dh, dh
    mov si, dx
    xor bh, bh
    mul bx
    add si, ax  ; SI = address
    mov es, video_seg

    ; Set up EGA registers for XOR operation
    write_ega_reg 03CEh, 1, 0       ; No set/reset
    write_ega_reg 03CEh, 3, 030o    ; XOR operation, 0 bits rotation
    write_ega_reg 03CEh, 8, 0FFh    ; Write all bits
    write_ega_reg 03C4h, 2, 0Fh     ; Write all planes
    mov ega_set_pixel, 1

    ; Remove any existing cursor
    call ega_cursor_off

    pop ax
    cmp al, 0
    je @end

        ; Set the cursor shape
        cmp al, 1
        jne @two
            mov ah, 0
            mov al, bl
            dec al
            jmp @on
        @two:
        cmp al, 2
        jne @three
            mov al, bl
            dec al
            mov ah, al
            dec ah
            jmp @on
        @three:
            mov ax, cursor_shape
        @on:

        ; Display the new cursor
        call ega_cursor_flip
        mov cursor_pos, si
        mov cursor_state, ax

    @end:

    write_ega_reg 03CEh, 3, 0
    pop es
    pop si
    pop dx
    pop bx
    pop ax
    ret

ega_CSRDSP endp

; Turn off the cursor
ega_cursor_off proc near private

    cmp cursor_pos, 0FFFFh
    je @end ; it isn't already off
        push ax
        push si
        push es

        mov es, video_seg
        mov si, cursor_pos
        mov ax, cursor_state
        call ega_cursor_flip

        pop es
        pop si
        pop ax
        mov cursor_pos, 0FFFFh
    @end:

    ret

ega_cursor_off endp

; Reverse the area of text that the cursor occupies
; On entry: AH = start line; AL = end_line
;           ES:SI = address of line 0 of the text cell
;           BL = height of character cell
;           DI = address of Screen_Mode block
;           EGA registers configured for XOR operation
ega_cursor_flip proc near private

    push cx
    push dx
    push si

    ; DX <- x_res/8
    mov dx, cs:Screen_Mode.x_res[di]
    shr dx, 1
    shr dx, 1
    shr dx, 1

    mov ch, 0
    @flip:

        ; Flip line
        cmp ch, ah
        jb @F
        cmp ch, al
        ja @F
            mov bh, es:[si] ; Fill the latch register
            mov byte ptr es:[si], 0FFh
        @@:
        inc ch

        ; Advance to next line
        add si, dx

    cmp ch, bl
    jb @flip

    pop si
    pop dx
    pop cx
    ret

ega_cursor_flip endp

; Set up configured colors
; On entry: BL != 0 if parameter 1 specified
;           BH contains parameter 1
;           CL != 0 if parameter 2 specified
;           CH contains parameter 1
;           DL != 0 if parameter 3 specified
;           DH contains parameter 3
; Returns: C set if error
ega_SETCLR proc near

    or bl, bl
    je @F
        ; Can't use COLOR 0 because the on-screen editor doesn't work
        and bh, 0Fh
        je @error

        mov foreground_color, bh
        mov text_attr, bh
    @@:

    or cl, cl
    je @F
        ; Background color
        push ax
        push bx
        mov bl, ch
        mov bh, 0
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    clc
    ret

    @error:
    stc
    ret

ega_SETCLR endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
ega_SWIDTH_80 proc near

    cmp AL, 80
    je @80_columns
    cmp AL, 40
    je @40_columns
        ; anything else is an error
        stc
        ret

    @40_columns:
        clc
        ret

    @80_columns:
        ; Switch to mode 8
        mov al, 8
        call set_screen_mode
        ret

ega_SWIDTH_80 endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
ega_SWIDTH_40 proc near

    cmp AL, 80
    je @80_columns
    cmp AL, 40
    je @40_columns
        ; anything else is an error
        stc
        ret

    @40_columns:
        ; Switch to mode 7
        mov al, 7
        call set_screen_mode
        ret

    @80_columns:
        clc
        ret

ega_SWIDTH_40 endp

; Set foreground and background colors
; On entry: foreground_color and background_color set
; Returns: none
ega_SETFBC proc near private

    ret

ega_SETFBC endp

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
ega_STOREC proc near

    push ax
    push dx

    mov video_pos, bx
    mov video_bitmask, al

    ; Recover the X coordinate
    mov ax, bx
    mov bx, cs:Screen_Mode.x_res[di]
    shr bx, 1
    shr bx, 1
    shr bx, 1
    xor dx, dx
    div bx        ; remainder to DX
    shl dx, 1     ; convert to pixel coordinate
    shl dx, 1
    shl dx, 1
    mov al, video_bitmask
    test al, 0Fh  ; get the lower three bits
    je @F
        or dx, 4
    @@:
    test al, 33h
    je @F
        or dx, 2
    @@:
    test al, 55h
    je @F
        or dx, 1
    @@:

    mov x_coordinate, dx

    pop dx
    pop ax
    ret

ega_STOREC endp

; Move one pixel up
; On entry: none
; Returns   none
ega_UPC proc near

    push ax
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1
    shr ax, 1
    shr ax, 1
    sub video_pos, ax
    pop ax
    ret

ega_UPC endp

; Move one pixel down
; On entry: none
; Returns   none
ega_DOWNC proc near

    push ax
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1
    shr ax, 1
    shr ax, 1
    add video_pos, ax
    pop ax
    ret

ega_DOWNC endp

; Move one pixel left
; On entry: none
; Returns   none
ega_LEFTC proc near

    ; Shift the bit mask left
    rol video_bitmask, 1

    ; Carry into the address
    jnc @F
        dec video_pos
    @@:

    ret

ega_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
ega_RIGHTC proc near

    ; Shift the bit mask right
    ror video_bitmask, 1

    ; Carry into the address
    jnc @F
        inc video_pos
    @@:

    ret

ega_RIGHTC endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
ega_MAPXYC proc near
 
    push ax
    push bx
    push cx
    push dx ; MUL clobbers DX

    mov x_coordinate, cx

    ; Separate the bit mask from the byte position
    mov bx, cx
    and cl, 07h
    mov al, 80h
    shr al, cl
    mov video_bitmask, al
    mov cl, 3
    shr bx, cl

    ; AX <- x_res/8
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, cl

    ; video_pos <- AX*y + x/8
    mul dx
    add ax, bx
    mov video_pos, ax

    pop dx
    pop cx
    pop bx
    pop ax
    ret
 
ega_MAPXYC endp

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  C set if error
ega_SETATR proc near private

    mov graph_attr, al
    mov ega_set_pixel, 1
    clc
    ret

ega_SETATR endp

ega_set_attr proc near private

    cmp ega_set_pixel, 0
    je @end
        ; SETC will use the bit mask register 03CE[8] to select the pixels to write.
        ; Any write will then replace the pixels with the configured values.
        write_ega_reg 03CEh, 0, graph_attr ; Set/reset the selected attribute
        write_ega_reg 03CEh, 1, 0Fh        ; Set/reset all planes
        write_ega_reg 03CEh, 3, 000o       ; Replace operation, 0 bits rotation
        write_ega_reg 03C4h, 2, 0Fh        ; Write all planes
        mov ega_set_pixel, 0
    @end:
    ret

ega_set_attr endp

; Read pixel at current position
; Returns: AL = pixel attribute
ega_READC proc near

    push bx
    push cx
    push dx ; write_ega_reg uses DX
    push si
    push es

    les si, video_addr
    mov ch, video_bitmask
    xor bl, bl              ; Build the pixel here

    ; Read the bit from each plane
    mov cl, 3
    @read:
        write_ega_reg 03CEh, 4, cl  ; Read select register
        mov ah, es:[si]             ; Read the plane
        and ah, ch                  ; Isolate the bit
        or bl, ah                   ; Assemble with other planes
        rol bl, 1
    dec cl
    jns @read

    ; Shift the bits into place
    mov al, bl
    mov cx, x_coordinate
    and cl, 07h
    rol al, cl

    pop es
    pop si
    pop dx
    pop cx
    pop bx
    ret

ega_READC endp

; Write pixel at current location, using current attribute
; Returns: none
ega_SETC proc near

    push ax
    push dx
    push di
    push es

    ; Set up color registers if needed
    call ega_set_attr

    ; Set bit mask register
    write_ega_reg 03CEh, 8, video_bitmask

    ; Write the pixel
    les di, video_addr
    mov al, es:[di]
    mov es:[di], al

    pop es
    pop di
    pop dx
    pop ax
    ret

ega_SETC endp

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
ega_NSETCX proc near

    push ax
    push bx
    push cx
    push dx ; write_ega_reg uses DX
    push bp
    push si
    push di
    push es

    cld

    ; Set up color registers if needed
    call ega_set_attr

    ; Start and end of draw
    mov bp, x_coordinate
    add bx, bp
    cmp bx, cs:Screen_Mode.x_res[di]
    jb @F
        mov bx, cs:Screen_Mode.x_res[di]
    @@:
    dec bx

    ; Does the draw cross a byte boundary?
    mov ax, bx
    xor ax, bp
    and ax, 0FFF8h
    jnz @multibyte

        ; Draw lies within a single byte
        mov si, bp
        and si, 07h
        mov cl, left_mask_1[si]
        mov si, bx
        and si, 07h
        and cl, right_mask_1[si] ; CL has 1 where bits will be replaced
        write_ega_reg 03CEh, 8, cl
        les di, video_addr
        mov al, es:[di]         ; Set the latch register
        stosb                   ; Write the pixels

    jmp @end
    @multibyte:

        ; Draw spans two or more bytes
        ; Draw the first byte
        mov ch, graph_attr
        les di, video_addr
        mov si, bp
        and si, 07h
        write_ega_reg 03CEh, 8, left_mask_1[si]
        mov al, es:[di]         ; Set the latch register
        stosb                   ; Write the pixels

        ; Draw zero or more whole bytes
        write_ega_reg 03CEh, 8, 0FFh
        mov si, bx      ; Save right partial byte for the end
        and si, 07h
        mov cl, 3
        shr bx, cl
        shr bp, cl
        sub bx, bp
        dec bx
        je @end_whole
            mov cx, bx
            ; Because we're writing all pixels, we don't need to set the
            ; latch register
            rep stosb
        @end_whole:

        ; Draw the last byte
        write_ega_reg 03CEh, 8, right_mask_1[si]
        mov al, es:[di]         ; Set the latch register
        stosb                   ; Write the pixels

    @end:

    pop es
    pop di
    pop si
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ega_NSETCX endp

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
ega_PGINIT proc near

    mov blit_addr, bx
    mov blit_bits, cx
    jnc @F
        push ax
        xor ah, ah
        shl ax, 1
        mov bx, ax
        mov ax, word ptr blit_op[bx]
        mov blit_mixer, ax
        mov bx, blit_addr
        pop ax
    @@:
    add cx, 7
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov blit_bytes, cx
    mov cx, blit_bits
    shr cx, 1
    shr cx, 1
    mov ega_blit_pixels, cx
    mov cx, blit_bits
    ret

; Table of mix operations
; First byte selects the operation in the EGA registers
; Second byte inverts the data
blit_op:
    db 020o, 0      ; OR
    db 010o, 0      ; AND
    db 000o, 0FFh   ; PRESET
    db 000o, 0      ; PSET
    db 030o, 0      ; XOR

ega_PGINIT endp

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
ega_NREAD proc near

    push ax
    push bx
    push cx
    push dx
    push bp
    push si
    push di
    push es

    cld

    ; AX <- number of pixels to read
    mov ax, blit_bits
    shr ax, 1
    shr ax, 1

    ; BP <- number of bytes to read from each plane
    mov bp, x_coordinate
    and bp, 7               ; extra pixels on left
    add bp, ax              ; total pixels to read
    add bp, 7               ; round up to byte
    shr bp, 1
    shr bp, 1
    shr bp, 1

    ; The assumption here is that memory is fast and I/O ports are slow.
    ; We read each plane into a separate byte array, which may be slightly
    ; larger than the one provided by the caller, then shift and repack
    ; them to fit into the caller's array.
    mov ega_blit_plane, 3
    mov ega_blit_bits, 0
    @plane:
        ; Select plane to read
        write_ega_reg 03CEh, 4, ega_blit_plane

        ; Read a plane into ega_repack
        mov cx, bp
        mov ax, ds
        mov es, ax
        mov di, offset ega_repack
        lds si, video_addr
        rep movsb
        mov ds, ax

        ; Shift so that no extra pixels appear to the left
        mov cx, x_coordinate
        and cx, 7       ; also CH <- 0; this is the carry byte
        je @end_lshift  ; Skip if no shift needed
            mov si, offset ega_repack
            add si, bp
            mov dx, bp
            @lshift:
                dec si
                mov al, [si]
                xor ah, ah
                shl ax, cl
                or al, ch       ; Carry in
                mov ch, ah      ; Carry out
                mov [si], al
            dec dx
            jnz @lshift
        @end_lshift:

        ; Shift to align with the caller's array
        mov si, offset ega_repack   ; Copy from here
        mov di, ega_blit_bits
        shr di, 1
        shr di, 1
        shr di, 1
        add di, blit_addr           ; to here
        mov cx, ega_blit_bits
        and cx, 7                   ; Shift count
        mov dx, ega_blit_pixels
        add dx, cx                  ; Number of bits to write
        add dx, 7                   ; Convert to bytes
        shr dx, 1
        shr dx, 1
        shr dx, 1
        or cl, cl
        je @no_rshift  ; Just copy if no shift needed
            ; Shift right and append to caller's buffer
            mov bl, cl              ; Mask off extra bits
            xor bh, bh
            mov ch, [di]            ; Initial carry in
            mov ah, left_mask_1[bx]
            not ah
            and ch, ah
            ; Shift
            @rshift:
                mov ah, [si]
                xor al, al
                shr ax, cl
                or ah, ch       ; Carry in
                mov ch, al      ; Carry out
                mov [di], ah
                inc si
                inc di
            dec dx
            jnz @rshift
        jmp @end_rshift
        @no_rshift:
            ; Shift is zero; just copy
            mov cx, dx
            rep movsb
        @end_rshift:
        ; Add to the number of bits copied
        mov ax, ega_blit_pixels
        add ega_blit_bits, ax
    dec ega_blit_plane
    jns @plane

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ega_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
ega_NWRITE proc near

    push ax
    push bx
    push cx
    push dx
    push bp
    push si
    push di
    push es

    cld

    write_ega_reg 03CEh, 1, 0                     ; No set/reset
    write_ega_reg 03CEh, 3, byte ptr blit_mixer+0 ; Selected operation, 0 bits rotation

    ; AX <- number of pixels to read
    mov ax, blit_bits
    shr ax, 1
    shr ax, 1

    ; BP <- number of bytes to read from each plane
    mov bp, x_coordinate
    and bp, 7               ; extra pixels on left
    add bp, ax              ; total pixels to read
    add bp, 7               ; round up to byte
    shr bp, 1
    shr bp, 1
    shr bp, 1

    mov ega_blit_plane, 08h
    mov ega_blit_bits, 0
    @plane:
        ; Copy bytes from caller's buffer to ega_repack
        mov cl, 3
        mov si, ega_blit_bits
        mov dx, si
        shr si, cl
        add si, blit_addr
        and dx, 07h
        add dx, ega_blit_pixels
        add dx, 7
        shr dx, cl
        mov cx, dx
        mov ax, ds
        mov es, ax
        mov di, offset ega_repack
        rep movsb

        ; Shift left so that the first pixel is in the first bit
        mov cx, ega_blit_bits
        and cx, 07h     ; Also CH <- 0, initial carry in
        je @end_lshift  ; Skip if shift count is zero
            mov si, offset ega_repack
            add si, bp
            mov dx, bp
            @lshift:
                dec si
                mov al, [si]
                xor ah, ah
                shl ax, cl
                or al, ch       ; Carry in
                mov ch, ah      ; Carry out
                mov [si], al
            dec dx
            jnz @lshift
        @end_lshift:

        ; Shift right to align with the requested position
        mov cx, x_coordinate
        and cx, 07h     ; Also CH <- 0, initial carry in
        je @end_rshift  ; Skip if shift count is zero
            mov si, offset ega_repack
            mov dx, bp
            @rshift:
                mov ah, [si]
                xor al, al
                shr ax, cl
                or ah, ch       ; Carry in
                mov ch, al      ; Carry out
                mov [si], ah
                inc si
            dec dx
            jnz @rshift
        @end_rshift:

        ; Transfer to the frame buffer
        write_ega_reg 03C4h, 2, ega_blit_plane
        cmp bp, 1
        jne @multibyte
            ; Transfer one byte
            mov si, x_coordinate
            and si, 07h
            mov di, si
            add di, ega_blit_pixels
            dec di
            and di, 07h
            mov ah, left_mask_1[si]
            and ah, right_mask_1[si]
            write_ega_reg 03CEh, 8, ah ; Bits to transfer
            les di, video_addr
            mov al, es:[di]             ; Set the latch register
            mov al, ega_repack[0]
            mov es:[di], al
        jmp @end_write
        @multibyte:
            ; Transfer two or more bytes
            mov si, offset ega_repack
            les di, video_addr
            ; Transfer the first byte
            mov bx, x_coordinate
            and bx, 07h
            write_ega_reg 03CEh, 8, left_mask_1[bx] ; Bits to transfer
            mov ah, byte ptr blit_mixer+1
            mov al, es:[di]             ; Set the latch register
            lodsb
            xor al, ah
            stosb
            ; Transfer zero or more whole bytes
            mov cx, bp
            sub cx, 2
            jcxz @end_whole
            write_ega_reg 03CEh, 8, 0FFh ; Bits to transfer
            @whole:
                mov al, es:[di]         ; Set the latch register
                lodsb
                xor al, ah
                stosb
            loop @whole
            @end_whole:
            ; Transfer the last byte
            mov bx, x_coordinate
            add bx, ega_blit_pixels
            dec bx
            and bx, 07h
            write_ega_reg 03CEh, 8, right_mask_1[bx] ; Bits to transfer
            mov al, es:[di]             ; Set the latch register
            lodsb
            xor al, ah
            stosb
        @end_write:

        ; Add to the number of bits copied
        mov ax, ega_blit_pixels
        add ega_blit_bits, ax
    shr ega_blit_plane, 1
    jnz @plane

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    mov ega_set_pixel, 1
    write_ega_reg 03CEh, 3, 0

    pop es
    pop bp
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ega_NWRITE endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
ega_PNTINI proc near

    mov border_attr, al
    clc
    ret

ega_PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
ega_TDOWNC proc near

    push ax
    push bx
    push dx

    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1                       ; AX <- x_res/8
    shr ax, 1
    shr ax, 1
    mov bx, ax                      ; BX <- x_res/8
    mul cs:Screen_Mode.y_res[di]    ; AX <- (x_res/8)*y_res
    add bx, video_pos               ; BX <- new position
    cmp ax, bx                      ; Set C if off the bottom edge
    jc @F
        mov video_pos, bx
    @@:

    pop dx
    pop bx
    pop ax
    ret

ega_TDOWNC endp

; Move current position up with boundary check
; Returns: C set if moving up would pass the top of the screen;
;          the current position is unchanged in that case
; This differs from UPC only in the boundary check
ega_TUPC proc near

    push ax
    push bx
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1
    shr ax, 1
    shr ax, 1
    mov bx, video_pos
    sub bx, ax
    jc @F
        mov video_pos, bx
    @@:
    pop bx
    pop ax
    ret

ega_TUPC endp

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
ega_SCANR proc near

    push ax
    push bp
    push si
    push di
    push es

    mov ega_set_pixel, 1

    ; Set up read mode 1 and border color as target
    push dx
    write_ega_reg 03CEh, 5, 08h         ; Read mode 1
    write_ega_reg 03CEh, 2, border_attr ; Color compare register
    write_ega_reg 03CEh, 7, 0Fh         ; All bits of color are significant
    pop dx

    ; Right boundary
    mov bp, cs:Screen_Mode.x_res[di]
    dec bp

    ; Graphics position
    les di, video_addr
    mov ah, video_bitmask
    mov si, x_coordinate

    ; Search right for non-border pixel
    or dx, dx
    jz @paint_not_found
    @border_scan:
        ; End loop if a non-border pixel is found
        ; With read mode 1 set, a read of the frame buffer returns 1 where the
        ; pixel matches the border color
        mov al, es:[di]
        and al, ah          ; video_bitmask
        jz @paint_found
        ; Go to next pixel
        cmp si, bp
        jae @paint_not_found
        inc si              ; x_coordinate
        ror ah, 1
        jnc @F
            inc di
        @@:
    dec dx
    jnz @border_scan
    @paint_not_found:
        ; No matching pixel found
        mov video_bitmask, ah
        mov video_pos, di
        mov x_coordinate, si
        xor bx, bx ; Nothing painted
        xor cl, cl
        xor dx, dx ; No border pixels remain
        push dx
        jmp @end

    @paint_found:

    ; Set position where drawing begins
    mov CSAVEA, di
    mov CSAVEM, ah
    push dx             ; remaining border pixel count
    mov bx, si          ; starting X coordinate

    ; Scan pixels until the border color is found again
    @count_pixels:
        mov al, es:[di]
        and al, ah
        jnz @end_count_pixels   ; Border color is found again
        ; Go to next pixel
        inc si                  ; x_coordinate
        ror ah, 1
        jnc @F
            inc di
        @@:
    cmp si, bp
    jbe @count_pixels           ; Right edge of screen is reached
    @end_count_pixels:

    ; BX <- number of pixels to paint (SI - BX)
    neg bx
    add bx, si

    ; Back up one pixel
    rol ah, 1
    jnc @F
        dec di
    @@:
    dec si

    ; Set this to the last painted pixel
    mov video_pos, di
    mov video_bitmask, ah
    mov x_coordinate, si

    ; CSAVEA:CSAVEM points to the pixel where we begin painting.
    ; video_pos:video_bitmask pointrs to the pixel where we end painting.

    ; While painting, we need to match the paint color
    write_ega_reg 03CEh, 2, graph_attr ; Color compare register

    ; Set the write options
    write_ega_reg 03CEh, 0, graph_attr ; Set/reset the selected attribute
    write_ega_reg 03CEh, 1, 0Fh        ; Set/reset all planes
    write_ega_reg 03CEh, 3, 000o       ; Replace operation, 0 bits rotation
    write_ega_reg 03C4h, 2, 0Fh        ; Write all planes

    ; Get the start position
    mov si, CSAVEA
    mov ah, CSAVEM

    ; Make AH into the left mask; e.g. 0x40 -> 0x7F
    dec ah
    stc
    rcl ah, 1

    ; Do we paint within a single byte?
    cmp si, video_pos
    jne @multibyte

        ; Apply the right mask
        mov al, video_bitmask
        dec al
        not al
        and ah, al

        ; Set the bit mask register
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov cl, es:[si]
        not cl
        and cl, ah

        ; Paint the pixels
        mov es:[si], al

    jmp @end_paint
    @multibyte:

        ; Write the first byte:
        ; Set the bit mask register
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov cl, es:[si]
        not cl
        and cl, ah

        ; Paint the pixels
        mov es:[si], al
        inc si

        ; Write zero or more whole bytes
        write_ega_reg 03CEh, 8, 0FFh
        mov di, video_pos
        jmp @end_whole
        @whole:
            ; Read color compare bits
            mov al, es:[si]
            not al
            or cl, al

            ; Paint the pixels
            mov es:[si], al
            inc si
        @end_whole:
        cmp si, di
        jb @whole

        ; Write the last byte

        ; Apply the right mask
        mov ah, video_bitmask
        dec ah
        not ah
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov al, es:[si]
        not al
        and al, ah
        or cl, al

        ; Paint the pixels
        mov es:[si], al

    @end_paint:

    ; Advance one point right
    ror video_bitmask, 1
    jnc @F
        inc video_pos
    @@:
    inc x_coordinate

@end:
    write_ega_reg 03CEh, 2, 0
    write_ega_reg 03CEh, 5, 0           ; Read mode 0
    pop dx  ; Number of remaining border pixels
    pop es
    pop di
    pop si
    pop bp
    pop ax

    ret

ega_SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
ega_SCANL proc near

    push ax
    push dx
    push si
    push di
    push es

    mov ega_set_pixel, 1

    ; Set up read mode 1 and border color as target
    write_ega_reg 03CEh, 5, 08h         ; Read mode 1
    write_ega_reg 03CEh, 2, border_attr ; Color compare register
    write_ega_reg 03CEh, 7, 0Fh         ; All bits of color are significant

    ; Graphics position
    les di, video_addr
    mov ah, video_bitmask
    mov si, x_coordinate

    ; Scan left until border color found
    mov bx, 0
    @scan:
        inc bx

        ; Go to next pixel
        rol ah, 1
        jnc @F
            dec di
        @@:
        dec si          ; x_coordinate
        js @end_scan

        ; Reading the frame buffer gets 1 for any pixel that matches the border
        mov al, es:[di]
        and al, ah
    jz @scan
    @end_scan:
    ; Move back one pixel to the right
    inc si
    ror ah, 1
    jnc @F
        inc di
    @@:
    dec bx

    ; Update the graphics position and retrieve the starting position
    xchg x_coordinate, si
    mov si, di
    xchg video_pos, di
    xchg video_bitmask, ah

    ; While painting, match the paint color
    write_ega_reg 03CEh, 2, graph_attr ; Color compare register

    ; Set the write options
    write_ega_reg 03CEh, 0, graph_attr ; Set/reset the selected attribute
    write_ega_reg 03CEh, 1, 0Fh        ; Set/reset all planes
    write_ega_reg 03CEh, 3, 000o       ; Replace operation, 0 bits rotation
    write_ega_reg 03C4h, 2, 0Fh        ; Write all planes

    ; Make AH into the right mask, e.g. 0x40 -> 0xC0
    dec ah
    not ah

    ; Do we paint within a single byte?
    cmp di, video_pos
    jne @multibyte

        ; Apply the left mask
        mov al, video_bitmask
        dec al
        stc
        rcl al, 1
        and ah, al

        ; Set the bit mask register
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov cl, es:[di]
        not cl
        and cl, ah

        ; Paint the pixels
        mov es:[di], al

    jmp @end_paint
    @multibyte:

        ; Write the first byte:
        ; Set the bit mask register
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov cl, es:[di]
        not cl
        and cl, ah

        ; Paint the pixels
        mov es:[di], al
        dec di

        ; Write zero or more whole bytes
        ; SI has the address of the last byte
        write_ega_reg 03CEh, 8, 0FFh
        jmp @end_whole
        @whole:
            ; Read color compare bits
            mov al, es:[di]
            not al
            or cl, al

            ; Paint the pixels
            mov es:[di], al
            dec di

        @end_whole:
        cmp di, si
        ja @whole

        ; Write the last byte

        ; Apply the left mask
        mov ah, video_bitmask
        dec ah
        stc
        rcl ah, 1
        write_ega_reg 03CEh, 8, ah

        ; Read color compare bits; this also sets the latch register
        mov al, es:[di]
        not al
        and al, ah
        or cl, al

        ; Paint the pixels
        mov es:[di], al

    @end_paint:

    write_ega_reg 03CEh, 2, 0
    write_ega_reg 03CEh, 5, 0           ; Read mode 0
    pop es
    pop di
    pop si
    pop dx
    pop ax
    ret

ega_SCANL endp

;-----------------------------------------------------------------------------
; Screen mode 13: VGA 320x200, 256 colors
;-----------------------------------------------------------------------------

; Set an EGA/VGA planar mode
vga_SCRSTT_init proc near private

    ; Set the correct BIOS mode
    mov al, cs:Screen_Mode.bios_mode[di]
    mov ah, 00h
    int 10h

    ; Did it work?
    push bx
    mov ah, 0Fh
    int 10h
    pop bx
    cmp al, cs:Screen_Mode.bios_mode[di]
    jne @error

    ; We're good.
    mov al, cs:Screen_Mode.text_columns[di]
    mov text_width, al
    mov al, cs:Screen_Mode.text_rows[di]
    mov text_height, al
    mov cursor_pos, 0FFFFh
    mov video_seg, 0A000h
    mov text_attr, 15
    clc
    ret

    ; Oops.
    @error:
    stc
    ret

vga_SCRSTT_init endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
vga_CLRSCN proc near private

    cmp al, 0
    stc
    jne @end

        push ax
        push cx
        push di
        push es

        mov es, video_seg
        xor di, di
        mov cx, 32000
        xor ax, ax
        cld
        rep stosw

        mov cursor_pos, 0FFFFh

        call SCNCLR
        call GRPINI

        pop es
        pop dx
        pop cx
        pop ax

        clc

    @end:
    ret

vga_CLRSCN endp

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
; Returns: none
vga_CSRDSP proc near private

    push ax
    push bx
    push dx
    push si
    push es

    ; Develop the address of the text cell
    push ax
    xchg dh, dl
    dec dl  ; 0-based column
    dec dh  ; 0-based row
    mov al, dh          ; AX <- row
    xor ah, ah
    xor dh, dh
    mov si, dx          ; SI <- column
    mov bx, 320
    mul bx              ; AX <- address of first column
    add si, ax          ; SI <- address
    shl si, 1
    shl si, 1
    shl si, 1
    mov es, video_seg

    ; Remove any existing cursor
    call vga_cursor_off

    pop ax
    cmp al, 0
    je @end

        ; Set the cursor shape
        cmp al, 1
        jne @two
            mov ax, 0007h
            jmp @on
        @two:
        cmp al, 2
        jne @three
            mov ax, 0607h
            jmp @on
        @three:
            mov ax, cursor_shape
        @on:

        ; Display the new cursor
        call vga_cursor_flip
        mov cursor_pos, si
        mov cursor_state, ax

    @end:

    pop es
    pop si
    pop dx
    pop bx
    pop ax
    ret

vga_CSRDSP endp

; Turn off the cursor
vga_cursor_off proc near private

    cmp cursor_pos, 0FFFFh
    je @end ; it isn't already off
        push ax
        push si
        push es

        mov es, video_seg
        mov si, cursor_pos
        mov ax, cursor_state
        call vga_cursor_flip

        pop es
        pop si
        pop ax
        mov cursor_pos, 0FFFFh
    @end:

    ret

vga_cursor_off endp

; Reverse the area of text that the cursor occupies
; On entry: AH = start line; AL = end_line
;           ES:SI = address of line 0 of the text cell
vga_cursor_flip proc near private

    push bx
    push cx
    push dx
    push si

    mov ch, 0
    @flip:

        ; Flip line
        cmp ch, ah
        jb @no_flip
        cmp ch, al
        ja @no_flip
            mov bx, 7
            @pixel:
                xor byte ptr es:[bx+si], 0Fh
            dec bx
            jns @pixel
        @no_flip:
        inc ch

        ; Advance to next line
        add si, 320

    cmp ch, 8
    jb @flip

    pop si
    pop dx
    pop cx
    pop bx
    ret

vga_cursor_flip endp

; Set up configured colors
; On entry: BL != 0 if parameter 1 specified
;           BH contains parameter 1
;           CL != 0 if parameter 2 specified
;           CH contains parameter 1
;           DL != 0 if parameter 3 specified
;           DH contains parameter 3
; Returns: C set if error
vga_SETCLR proc near

    or bl, bl
    je @F
        ; Can't use COLOR 0 because the on-screen editor doesn't work
        or bh, bh
        je @error

        mov foreground_color, bh
        mov text_attr, bh
    @@:

    or cl, cl
    je @F
        ; Background color
        push ax
        push bx
        mov bl, ch
        mov bh, 0
        mov ah, 0Bh
        int 10h
        pop bx
        pop ax
    @@:

    clc
    ret

    @error:
    stc
    ret

vga_SETCLR endp

; Set foreground and background colors
; On entry: foreground_color and background_color set
; Returns: none
vga_SETFBC proc near private

    ret

vga_SETFBC endp

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
vga_STOREC proc near

    push ax
    push bx
    push dx

    mov video_pos, bx
    mov video_bitmask, al

    ; Recover the X coordinate
    mov ax, video_pos
    xor dx, dx
    mov bx, 320
    div bx
    mov x_coordinate, dx

    pop dx
    pop bx
    pop ax
    ret

vga_STOREC endp

; Move one pixel up
; On entry: none
; Returns   none
vga_UPC proc near

    sub video_pos, 320
    ret

vga_UPC endp

; Move one pixel down
; On entry: none
; Returns   none
vga_DOWNC proc near

    add video_pos, 320
    ret

vga_DOWNC endp

; Move one pixel left
; On entry: none
; Returns   none
vga_LEFTC proc near

    dec video_pos
    ret

vga_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
vga_RIGHTC proc near

    inc video_pos
    ret

vga_RIGHTC endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
vga_MAPXYC proc near

    push ax
    push dx

    mov ax, 320
    mul dx
    add ax, cx
    mov video_pos, ax
    mov video_bitmask, 0

    pop dx
    pop ax
    ret

vga_MAPXYC endp

; Set the pixel attribute to be drawn
; On entry: AL = attribute
; Returns:  C set if error
vga_SETATR proc near private

    mov graph_attr, al
    clc
    ret

vga_SETATR endp

; Read pixel at current position
; Returns: AL = pixel attribute
vga_READC proc near

    push bx
    push es
    les bx, video_addr
    mov al, es:[bx]
    pop es
    pop bx
    ret

vga_READC endp

; Write pixel at current location, using current attribute
; Returns: none
vga_SETC proc near

    push ax
    push bx
    push es
    les bx, video_addr
    mov al, graph_attr
    mov es:[bx], al
    pop es
    pop bx
    pop ax
    ret

vga_SETC endp

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
vga_NSETCX proc near

    push ax
    push cx
    push di
    push es

    cld
    les di, video_addr
    mov cx, bx
    mov al, graph_attr
    rep stosb

    pop es
    pop di
    pop cx
    pop ax
    ret

vga_NSETCX endp

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
vga_PGINIT proc near

    mov blit_addr, bx
    mov blit_bits, cx
    jnc @F
        xor ah, ah
        mov blit_mixer, ax
    @@:
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov blit_bytes, cx
    mov cx, blit_bits
    ret

vga_PGINIT endp

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
vga_NREAD proc near

    push ax
    push cx
    push si
    push di
    push es

    cld
    mov cx, blit_bytes
    mov ax, ds
    mov es, ax
    mov di, blit_addr
    lds si, video_addr
    rep movsb
    mov ds, ax

    ; Advnce the main memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop di
    pop si
    pop cx
    pop ax
    ret

vga_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
vga_NWRITE proc near

    push ax
    push cx
    push si
    push di
    push es

    mov cx, blit_bytes
    or cx, cx
    je @mix_end         ; Skip if 0 bytes to copy
    mov si, blit_addr
    les di, video_addr
    cld

    mov ax, blit_mixer
    cmp ax, 0
    jne @mix_1
        ; OR
        @@:
            lodsb
            or al, es:[di]
            stosb
        loop @B
    jmp @mix_end
    @mix_1:
    cmp ax, 1
    jne @mix_2
        ; AND
        @@:
            lodsb
            and al, es:[di]
            stosb
        loop @b
    jmp @mix_end
    @mix_2:
    cmp ax, 2
    jne @mix_3
        ; PRESET
        @@:
            lodsb
            not al
            stosb
        loop @b
    jmp @mix_end
    @mix_3:
    cmp ax, 3
    jne @mix_4
        ; PSET
        rep movsb
    jmp @mix_end
    @mix_4:
        ; XOR
        @@:
            lodsb
            xor al, es:[di]
            stosb
        loop @b
    @mix_end:

    ; Advnce the main memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop di
    pop si
    pop cx
    pop ax
    ret

vga_NWRITE endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
vga_PNTINI proc near

    mov border_attr, al
    clc
    ret

vga_PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
vga_TDOWNC proc near

    cmp video_pos, 64000 - 320
    jae @bounds
        add video_pos, 320
        clc
        ret
    @bounds:
        stc
        ret

vga_TDOWNC endp

; Move current position up with boundary check
; Returns: C set if moving up would pass the top of the screen;
;          the current position is unchanged in that case
; This differs from UPC only in the boundary check
vga_TUPC proc near

    cmp video_pos, 320
    jb @bounds
        sub video_pos, 320
        clc
        ret
    @bounds:
        stc
        ret

vga_TUPC endp

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
vga_SCANR proc near

    push ax
    push si
    push di
    push es

    les di, video_addr
    mov si, x_coordinate
    mov ah, border_attr

    ; Search right for non-border pixel
    or dx, dx
    jz @paint_not_found
    @border_scan:
        ; End loop if a non-border pixel is found
        mov al, es:[di]
        cmp al, ah
        jne @paint_found
        ; Go to next pixel
        cmp si, 319
        jae @paint_not_found
        inc si                  ; x_coordinate
        inc di
    dec dx
    jnz @border_scan
    @paint_not_found:
        ; No matching pixel found
        mov video_pos, di
        mov x_coordinate, si
        xor bx, bx ; Nothing painted
        xor cl, cl
        xor dx, dx ; No border pixels remain
        jmp @end

    @paint_found:

    ; Set position where drawing begins
    mov CSAVEA, di
    mov CSAVEM, 0
    xor cl, cl
    xor bx, bx
    mov ch, graph_attr

    @paint:
        ; End loop if a border pixel is found
        mov al, es:[di]
        cmp al, ah
        je @end_paint
        ; Update the change flag
        xor al, ch  ; graph_attr
        or cl, al
        ; Paint the pixel
        mov es:[di], ch
        inc di      ; video_pos
        inc si      ; x_coordinate
        inc bx      ; pixel count
    jmp @paint
    @end_paint:

    ; Set the graphics position to the border pixel
    mov video_pos, di
    mov x_coordinate, si

    @end:
    pop es
    pop di
    pop si
    pop ax
    ret

vga_SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
vga_SCANL proc near

    push ax
    push si
    push di
    push es

    ; Graphics position
    les di, video_addr
    mov ah, border_attr
    mov si, x_coordinate
    xor bx, bx
    xor cl, cl
    mov ch, graph_attr

    ; Scan left until border color found
    @paint:
        ; Go to next pixel
        dec di      ; video_pos
        dec si      ; x_coordinate
        js @end_paint
        ; End loop if a border pixel is found
        mov al, es:[di]
        cmp al, ah  ; border_attr
        je @end_paint
        ; Update the change flag
        xor al, ch  ; graph_attr
        or cl, al
        ; Paint the pixel
        mov es:[di], ch ; graph_attr
        inc bx      ; pixel count
    jmp @paint
    @end_paint:

    ; Move back one pixel to the right
    inc di
    inc si

    ; Set the graphics position to the last pixel painted
    mov video_pos, di
    mov x_coordinate, si

    pop es
    pop di
    pop si
    pop ax
    ret

vga_SCANL endp

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
; Height of text screen in rows
text_height db ?

; Variables used for scrolling
scroll_r1 db ?
scroll_c1 db ?
scroll_r2 db ?
scroll_c2 db ?
scroll_width db ?
scroll_height db ?
scroll_from dw ?
scroll_to dw ?

; Shape of text cursor set by CSRDSP
cursor_shape dw 0B0Dh ; Initially in overwrite mode
; For graphical modes
cursor_state dw ?
cursor_pos dw ?

; Colors set by SETFBC and SETCLR and returned by GETFBC
foreground_color db 7
background_color db 0
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

; EGA: Pixel attribute needs to be configured before pixel operation
ega_set_pixel db ?

; EGA: Bit-blits use this area to repack transferred bytes
; The size is one eighth the maximum X resolution of an EGA/VGA planar mode,
; plus one
ega_repack db 101 dup (?)
; EGA: Current plane while bit-blitting
ega_blit_plane db ?
; EGA: Bits per plane while bit-blitting
ega_blit_pixels dw ?
; EGA: Bits written to the caller's array
ega_blit_bits dw ?

DSEG ends

END
