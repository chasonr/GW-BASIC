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
extrn CHKKYB:near

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
GWINI proc near

    push ax
    push bx
    push cx
    push es

    call disp_check

    ; If we have an EGA or VGA, set blink mode
    test disp_installed, disp_ega
    je @F
        mov bx, 0040h
        mov es, bx
        mov al, es:[0065h]
        mov blink_flag, al ; to restore on exit
        mov bl, 1
        mov ax, 1003h
        int 10h
    @@:

    ; Set up screen mode 0
    xor al, al
    call set_screen_mode

    ; Set INT 1Bh
    mov ax, 351Bh
    int 21h
    mov word ptr old_int1B+0, bx
    mov word ptr old_int1B+2, es
    push ds
    mov dx, cs
    mov ds, dx
    mov dx, offset int1B_handler
    mov ax, 251Bh
    int 21h
    pop ds

    ; Set INT 1Ch
    mov ax, 351Ch
    int 21h
    mov word ptr old_int1C+0, bx
    mov word ptr old_int1C+2, es
    push ds
    mov dx, cs
    mov ds, dx
    mov dx, offset int1C_handler
    mov ax, 251Ch
    int 21h
    pop ds

    ; Set COMx interrupt handlers
    mov ax, 350Bh
    int 21h
    mov word ptr old_int0B+0, bx
    mov word ptr old_int0B+2, es
    push ds
    mov dx, cs
    mov ds, dx
    mov dx, offset comx_irq3_handler
    mov ax, 250Bh
    int 21h
    pop ds

    mov ax, 350Ch
    int 21h
    mov word ptr old_int0C+0, bx
    mov word ptr old_int0C+2, es
    push ds
    mov dx, cs
    mov ds, dx
    mov dx, offset comx_irq4_handler
    mov ax, 250Ch
    int 21h
    pop ds

    pop es
    pop cx
    pop bx
    pop ax
    ret

GWINI endp

; Identify the display adapter
disp_check proc near private

    push ax
    push bx
    push dx
    push es

    ; Clear the installation flags
    mov disp_installed, 0

    ; Look for the sequencer registers at port 03C4
    mov dx, 03C4h
    mov al, 05h
    out dx, al
    in al, dx
    cmp al, 05h
    jne @no_ega
    mov al, 0Ah
    out dx, al
    in al, dx
    cmp al, 0Ah
    jne @no_ega
        ; Check for installed VGA
        mov ax, 1A00h
        int 10h
        cmp al, 1Ah
        jne @ega
        cmp bl, 7
        jb @ega
        cmp bl, 8
        ja @ega
            ; VGA adapter installed
            mov disp_installed, disp_vga or disp_ega or disp_cga
        jmp @end_ega
        @ega:
            ; EGA adapter installed
            mov disp_installed, disp_ega or disp_cga
        @end_ega:
    jmp @end
    @no_ega:
        ; Check for a CRTC at 03D4
        mov dx, 03D4h
        call check_crtc
        jnc @end_cga
            mov disp_installed, disp_cga
        @end_cga:
        ; Check for a CRTC at 03B4
        ; A CGA may be installed alongside an MDA or 32K Hercules
        mov dx, 03B4h
        call check_crtc
        jnc @end_mda
            or disp_installed, disp_mda
        @end_mda:
        ; Allow Hercules if a driver is installed
        mov ah, 0EFh
        mov dx, 0FFFFh
        int 10h
        cmp dl, 0FFh
        je @end_hercules
            or disp_installed, disp_herc
            inc dh
            ; Adjust the maximum number of pages
            mov screen_mode_3[Screen_Mode.num_pages], dh
        @end_hercules:
    @end:

    pop es
    pop dx
    pop bx
    pop ax
    ret

disp_check endp

; Look for a CRTC at the port at DX
; Return C set if found
check_crtc proc near private

    ; Check for the presence of the index register; this will not disturb the
    ; operation of the device
    mov al, 05h
    out dx, al
    jmp short $+2
    in al, dx
    jmp short $+2
    cmp al, 05h
    jne @no_crtc

    mov al, 0Ah
    out dx, al
    jmp short $+2
    in al, dx
    jmp short $+2
    cmp al, 0Ah
    jne @no_crtc

    ; Check is good
    stc
    ret

    ; Something didn't match
@no_crtc:
    clc
    ret

check_crtc endp

; Check that the frame buffer at segment ES extends at least to the address
; in BX
; BX is a power of two, so any mirroring will show up at address 0
check_framebuf proc near private

    ; Save bytes at 0 and BX
    mov dh, es:[0]
    mov dl, es:[bx]

    ; Change the byte at BX
    mov al, dl
    not al
    mov es:[bx], al
    cmp es:[bx], al
    jne @no_mem     ; What we wrote didn't come back

    ; Invert the byte at 0 and write to BX.
    ; If the byte appears at 0, the frame buffer is mirrored.
    mov al, dh
    not al
    mov es:[bx], al
    cmp es:[0], al
    je @no_mem      ; Mirroring found

    ; Frame buffer is good
    mov es:[bx], dl
    clc
    ret

    ; No memory, or mirroring
@no_mem:
    mov es:[bx], dl
    stc
    ret

check_framebuf endp

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

    push ax
    push bx
    push es

    ; Save the DS for interrupt handlers
    mov BASIC_DS, ds

    ; Free memory beyond what the DS needs
    ; Allow the DS to be 64K if available memory permits

    ; BX <- current PSP
    mov ah, 51h
    int 21h

    ; DS minus PSP is the size of everything before the DS; add this to the
    ; block size
    mov ax, ds
    sub ax, bx
    add ax, 65536/16

    ; Arrange as needed to resize the block
    mov es, bx  ; PSP
    mov bx, ax  ; size in paragraphs
    mov ah, 4Ah
    int 21h

    pop es
    pop bx
    pop ax
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
    push dx

    ; Remove INT 1Bh handler
    push ds
    lds dx, old_int1B
    mov ax, 251Bh
    int 21h
    pop ds

    ; Remove INT 1Ch handler
    push ds
    lds dx, old_int1C
    mov ax, 251Ch
    int 21h
    pop ds
    call speaker_off

    ; Remove COMx interrupt handlers
    push ds
    lds dx, old_int0B
    mov ax, 250Bh
    int 21h
    pop ds

    push ds
    lds dx, old_int0C
    mov ax, 250Ch
    int 21h
    pop ds

    ; Set blink mode back to starting state
    test disp_installed, disp_ega
    je @F
        mov bl, blink_flag
        mov cl, 5
        shr bl, cl
        and bl, 1
        mov ax, 1003h
        int 10h
    @@:

    ; Return to 80 column text mode
    mov ax, 0007h
    test disp_installed, disp_cga
    je @F
        mov ax, 0003h
    @@:
    int 10h

    pop dx
    pop cx
    pop bx
    pop ax
    ret

GWTERM endp

; INT 1Bh handler
int1B_handler proc near private

    push ds
    mov ds, cs:BASIC_DS
    or event_flag, event_ctrlbreak
    pop ds
    jmp cs:old_int1B

int1B_handler endp
old_int1B dd ?

; INT 1Ch handler
int1C_handler proc near private

    push ds
    mov ds, cs:BASIC_DS
    call sound_timer
    pop ds
    jmp cs:old_int1C

int1C_handler endp
old_int1C dd ?

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

    ; Check for Ctrl-Break
    test event_flag, event_ctrlbreak
    je @F
        ; Return CTRL-C
        and event_flag, not event_ctrlbreak
        mov ax, 0FF03h
        or ax, ax
        stc
        ret
    @@:

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
            cmp al, 03h
            jne @end_ctrl_c
                ; Signal shift-ctrl-C as ctrl-break
                mov ah, 02h
                int 16h
                test al, 03h
                je @ctrl_c
                    mov ax, 0FF03h
                    or ax, ax
                    stc
                    ret
                @ctrl_c:
                mov ax, 0003h
            @end_ctrl_c:
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
    page_size      db ? ; Size of a page is 2 to this power
    num_pages      db ? ; Number of supported pages
    pixel_size     db ? ; Number of bits per pixel
    bios_mode      db ? ; BIOS video mode
    display        db ? ; Display type required
Screen_Mode ends

; Table of supported screen modes
mode_table label word
    dw offset screen_mode_0  ; 40x25 or 80x25 text
    dw offset screen_mode_1  ; CGA 320x200, 4 colors
    dw offset screen_mode_2  ; CGA 640x200, 2 colors
    dw offset screen_mode_3  ; Hercules 720x348, 2 colors
    dw 0                     ; Olivetti; properties unknown
    dw 0                     ; undefined
    dw 0                     ; undefined
    dw offset screen_mode_7  ; EGA 320x200, 16 colors
    dw offset screen_mode_8  ; EGA 640x200, 16 colors
    dw offset screen_mode_9  ; EGA 640x350, 16 colors
    dw offset screen_mode_10 ; EGA 640x350, monochrome
    dw offset screen_mode_11 ; VGA 640x480, monochrome
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
    db 80                     ; text_columns (actually may be 40)
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 400                    ; y_res
    dw 0100h                  ; width_height
    dw 0100h                  ; height_width
    db 12                     ; page_size (actually may be 2**11)
    db 8                      ; num_pages
    db 0                      ; pixel_size
    db 3                      ; bios_mode (actually 0-3 or 7)
    db disp_mda or disp_cga   ; display

screen_mode_1 label word
    dw mode_1_SCRSTT_init     ; SCRSTT_init
    dw mode_1_SCRSTT_color    ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw cga_SCROLL             ; SCROLL_handler
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
    dw mono_LEFTC             ; LEFTC_handler
    dw mono_RIGHTC            ; RIGHTC_handler
    dw cga_MAPXYC             ; MAPXYC_handler
    dw mode_1_SETATR          ; SETATR_handler
    dw mono_READC             ; READC_handler
    dw mono_SETC              ; SETC_handler
    dw mono_NSETCX            ; NSETCX_handler
    dw mono_PGINIT            ; PGINIT_handler
    dw mono_NREAD             ; NREAD_handler
    dw mono_NWRITE            ; NWRITE_handler
    dw mode_1_PNTINI          ; PNTINI_handler
    dw cga_TDOWNC             ; TDOWNC_handler
    dw cga_TUPC               ; TUPC_handler
    dw mono_SCANR             ; SCANR_handler
    dw mono_SCANL             ; SCANL_handler
    db 40                     ; text_columns
    db 25                     ; text_rows
    dw 320                    ; x_res
    dw 200                    ; y_res
    dw 00D5h                  ; width_height (0.833)
    dw 0133h                  ; height_width (1.200)
    db 14                     ; page_size
    db 1                      ; num_pages
    db 2                      ; pixel_size
    db 5                      ; bios_mode (actually 4 or 5)
    db disp_cga               ; display

screen_mode_2 label word
    dw mode_2_SCRSTT_init     ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw cga_SCROLL             ; SCROLL_handler
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
    dw mono_LEFTC             ; LEFTC_handler
    dw mono_RIGHTC            ; RIGHTC_handler
    dw cga_MAPXYC             ; MAPXYC_handler
    dw mono_SETATR            ; SETATR_handler
    dw mono_READC             ; READC_handler
    dw mono_SETC              ; SETC_handler
    dw mono_NSETCX            ; NSETCX_handler
    dw mono_PGINIT            ; PGINIT_handler
    dw mono_NREAD             ; NREAD_handler
    dw mono_NWRITE            ; NWRITE_handler
    dw mono_PNTINI            ; PNTINI_handler
    dw cga_TDOWNC             ; TDOWNC_handler
    dw cga_TUPC               ; TUPC_handler
    dw mono_SCANR             ; SCANR_handler
    dw mono_SCANL             ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 640                    ; x_res
    dw 200                    ; y_res
    dw 006Bh                  ; width_height (0.4167)
    dw 0266h                  ; height_width (2.4000)
    db 14                     ; page_size
    db 1                      ; num_pages
    db 1                      ; pixel_size
    db 6                      ; bios_mode
    db disp_cga               ; display

screen_mode_3 label word
    dw herc_SCRSTT_init       ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw herc_SCRSTT_actpage    ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw herc_SCROLL            ; SCROLL_handler
    dw herc_CLRSCN            ; CLRSCN_handler
    dw generic_CLREOL         ; CLREOL_handler
    dw generic_CSRATR         ; CSRATR_handler
    dw herc_CSRDSP            ; CSRDSP_handler
    dw generic_LCPY           ; LCPY_handler
    dw graphics_stub          ; SCRATR_handler
    dw graphics_stub          ; SETCLR_handler
    dw herc_SWIDTH            ; SWIDTH_handler
    dw generic_GETFBC         ; GETFBC_handler
    dw graphics_stub          ; SETFBC_handler
    dw generic_FKYFMT         ; FKYFMT_handler
    dw generic_FKYADV         ; FKYADV_handler
    dw generic_GRPSIZ         ; GRPSIZ_handler
    dw herc_STOREC            ; STOREC_handler
    dw generic_FETCHC         ; FETCHC_handler
    dw herc_UPC               ; UPC_handler
    dw herc_DOWNC             ; DOWNC_handler
    dw mono_LEFTC             ; LEFTC_handler
    dw mono_RIGHTC            ; RIGHTC_handler
    dw herc_MAPXYC            ; MAPXYC_handler
    dw mono_SETATR            ; SETATR_handler
    dw mono_READC             ; READC_handler
    dw mono_SETC              ; SETC_handler
    dw mono_NSETCX            ; NSETCX_handler
    dw mono_PGINIT            ; PGINIT_handler
    dw mono_NREAD             ; NREAD_handler
    dw mono_NWRITE            ; NWRITE_handler
    dw mono_PNTINI            ; PNTINI_handler
    dw herc_TDOWNC            ; TDOWNC_handler
    dw herc_TUPC              ; TUPC_handler
    dw mono_SCANR             ; SCANR_handler
    dw mono_SCANL             ; SCANL_handler
    db 80                     ; text_columns
    db 25                     ; text_rows
    dw 720                    ; x_res
    dw 348                    ; y_res
    dw 00A5h                  ; width_height (0.644)
    dw 018Dh                  ; height_width (1.552)
    db 15                     ; page_size
    db 2                      ; num_pages
    db 1                      ; pixel_size
    db 8                      ; bios_mode
    db disp_herc              ; display

screen_mode_7 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    db 13                     ; page_size
    db 8                      ; num_pages
    db 4                      ; pixel_size
    db 0Dh                    ; bios_mode
    db disp_ega               ; display

screen_mode_8 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    db 14                     ; page_size
    db 4                      ; num_pages
    db 4                      ; pixel_size
    db 0Eh                    ; bios_mode
    db disp_ega               ; display

screen_mode_9 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    db 15                     ; page_size
    db 2                      ; num_pages
    db 4                      ; pixel_size
    db 10h                    ; bios_mode
    db disp_ega               ; display

screen_mode_10 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    dw ega_mono_NREAD         ; NREAD_handler
    dw ega_mono_NWRITE        ; NWRITE_handler
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
    db 15                     ; page_size
    db 2                      ; num_pages
    db 1                      ; pixel_size
    db 0Fh                    ; bios_mode
    db disp_ega               ; display

screen_mode_11 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    dw ega_mono_NREAD         ; NREAD_handler
    dw ega_mono_NWRITE        ; NWRITE_handler
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
    db 0                      ; page_size
    db 1                      ; num_pages
    db 1                      ; pixel_size
    db 11h                    ; bios_mode
    db disp_vga               ; display

screen_mode_12 label word
    dw ega_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw ega_SCROUT             ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw ega_SCROLL             ; SCROLL_handler
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
    db 0                      ; page_size
    db 1                      ; num_pages
    db 4                      ; pixel_size
    db 12h                    ; bios_mode
    db disp_vga               ; display

screen_mode_13 label word
    dw vga_SCRSTT_init        ; SCRSTT_init
    dw graphics_stub          ; SCRSTT_color
    dw generic_SCRSTT_actpage ; SCRSTT_actpage
    dw generic_SCRSTT_vispage ; SCRSTT_vispage
    dw generic_SCROUT         ; SCROUT_handler
    dw generic_SCRINP         ; SCRINP_handler
    dw vga_SCROLL             ; SCROLL_handler
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
    db 0                      ; page_size
    db 1                      ; num_pages
    db 8                      ; pixel_size
    db 13h                    ; bios_mode
    db disp_vga               ; display

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

    push bx ; GRPINI clobbers it
    push cx
    push di

    ; Determine if the mode is valid
    cmp al, mode_table_size
    jae @error              ; invalid if outside of mode_table
    xor ah, ah
    mov di, ax
    shl di, 1
    mov di, mode_table[di]
    or di, di
    jz @error               ; or if mode_table has 0

    ; Check for required hardware
    mov al, cs:Screen_Mode.display[di]
    and al, disp_installed
    jz @error

    ; Set up the mode
    call cs:Screen_Mode.SCRSTT_init[di]
    jc @error
    mov mode_ptr, di
    mov  al,text_width
    mov  cl,text_height
    call SCNSWI
    call GRPINI
    call SCNCLR

    ; Set the active page to 0
    xor ax, ax
    mov active_page, al
    call cs:Screen_Mode.SCRSTT_actpage[di]

    clc
    pop cx ; Discard saved DI
    pop cx
    pop bx
    ret

@error:
    pop di
    pop cx
    pop bx
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

    push ax
    push cx

    mov cl, cs:Screen_Mode.page_size[di]
    or cl, cl
    je @no_pages
        shl ax, cl
        mov page_offset, ax
        mov ax, 1
        shl ax, cl
        dec ax
        mov page_bitmask, ax
    jmp @end
    @no_pages:
        mov page_offset, 0
        mov page_bitmask, 0FFFFh
    @end:

    pop cx
    pop ax
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
    mov bh, active_page
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

    ; Determine lines per character
    mov ah, disp_installed
    test ah, disp_vga
    je @check_ega
        ; VGA: 16 line character
        mov ah, 16
    jmp @got_lines
    @check_ega:
    test ah, disp_ega
    je @check_cga
        ; EGA has 14 line characters, but scales the cursor height
        mov ah, 8
    jmp @got_lines
    @check_cga:
    test ah, disp_cga
    je @set_mda
        ; CGA: 8 line character
        mov ah, 8
    jmp @got_lines
    @set_mda:
        ; MDA or Hercules: 14 line character
        mov ah, 14
    @got_lines:

    ; Determine the shape of the cursor
    cmp al, 0
    jne @F
        mov cx, 1F1Fh ; start line 31, end line 31 (no cursor)
        jmp @set_cursor
    @@:
    cmp ax, 8*256 + 1
    jne @F
        mov cx, 0007h ; start line 0, end line 7
        jmp @set_cursor
    @@:
    cmp ax, 8*256 + 2
    jne @F
        mov cx, 0607h ; start line 6, end line 7
        jmp @set_cursor
    @@:
    cmp ax, 14*256 + 1
    jne @F
        mov cx, 000Dh ; start line 0, end line 13
        jmp @set_cursor
    @@:
    cmp ax, 14*256 + 2
    jne @F
        mov cx, 0B0Dh ; start line 11, end line 13
        jmp @set_cursor
    @@:
    cmp ax, 16*256 + 1
    jne @F
        mov cx, 000Dh ; start line 0, end line 13
        jmp @set_cursor
    @@:
    cmp ax, 16*256 + 2
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

    test disp_installed, disp_cga
    je @monochrome
        ; Query the current mode and width
        mov ah, 0Fh
        int 10h
        mov al, 3     ; BIOS mode for 80 columns
        mov bl, 80
        cmp ah, 40
        jg @F
            mov al, 1 ; BIOS mode for 40 columns
            mov bl, 40
        @@:
        mov text_width, bl
        mov ah, 00h
        int 10h       ; Set the BIOS mode
        mov video_seg, 0B800h
    jmp @end
    @monochrome:
        ; Not necessary with MDA. With Hercules, this returns to text mode.
        mov ax, 0007h
        int 10h
        mov text_width, 80
        mov video_seg, 0B000h
    @end:

    mov text_height, 25
    mov foreground_color, 7
    mov background_color, 0
    call mode_0_set_text_attr

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

    ; Set up the scrolling:
    ; Save the registers
    mov scroll_width, ch
    mov scroll_height, cl

    ; Convert coordinates to addresses:
    mov dx, ax
    call mode_0_xy_to_address
    jc @end
    mov scroll_from, di
    mov dx, bx
    call mode_0_xy_to_address
    jc @end
    mov scroll_to, di

    ; Address the frame buffer
    mov es, video_seg

    ; Which way are we scrolling?
    cmp di, scroll_from
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

@end:
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
    shl ah, 1           ; shift left three, for page * 2048
    shl ah, 1
    shl ah, 1
    cmp text_width, 80
    jb @F
        shl ah, 1       ; or four, for page * 4096
    @@:
    mov al, 0           ; Load into high byte
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

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
cga_SCROLL proc near private

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
    shl cl, 1
    shl cl, 1
    mov scroll_height, cl ; as line count per bank

    ; Convert coordinates to addresses
    mov cl, 160
    mov al, scroll_r1
    mul cl
    shl ax, 1
    mov scroll_from, ax
    mov al, scroll_r2
    mul cl
    shl ax, 1
    mov scroll_to, ax
    mov al, scroll_c1
    xor ah, ah
    mov bl, scroll_c2
    xor bh, bh
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 2
        shl ax, 1
        shl bx, 1
        shl scroll_width, 1 ; as byte count
    @@:
    add scroll_from, ax
    add scroll_to, bx

    ; Address the frame buffer
    mov es, video_seg

    ; Which way are we scrolling?
    mov ax, scroll_to
    cmp ax, scroll_from
    jae @scroll_higher
        ; Scroll from higher to lower address
        cld
        mov dl, scroll_height
        @scroll_1:
            ; Scroll an even line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Scroll an odd line
            mov si, scroll_from
            mov di, scroll_to
            add si, 2000h
            add di, 2000h
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Go to next line
            mov ax, 80
            add scroll_from, ax
            add scroll_to, ax
        dec dl
        jnz @scroll_1
    jmp @scroll_end
    @scroll_higher:
        ; Scroll from lower to higher address
        ; Point starting addresses to last cell to scroll
        mov al, scroll_height   ; AX <- ((scroll_height-1)*80)+(scroll_width-1)
        dec al
        mov cl, 80
        mul cl
        add al, scroll_width
        adc ah, 0
        dec ax
        add scroll_from, ax
        add scroll_to, ax
        ; Begin loop
        std
        mov dl, scroll_height
        @scroll_2:
            ; Scroll an even line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Scroll an odd line
            mov si, scroll_from
            mov di, scroll_to
            add si, 2000h
            add di, 2000h
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Go to next line
            mov ax, 80
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

cga_SCROLL endp

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
; Modes 1, 2 and 3 all use this
mono_LEFTC proc near private

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

mono_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
; Modes 1, 2 and 3 all use this
mono_RIGHTC proc near private

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

mono_RIGHTC endp

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
; Modes 1, 2 and 3 all use this
mono_READC proc near private

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

mono_READC endp

; Write pixel at current location, using current attribute
; Returns: none
; Modes 1, 2 and 3 all use this
mono_SETC proc near private

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

mono_SETC endp

; Write multiple pixels starting at current position and proceeding right
; On entry: BX = pixel count
; Returns:  none
; Modes 1, 2 and 3 all use this
mono_NSETCX proc near private

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
    cmp bx, cs:Screen_Mode.x_res[di]
    jb @F
        mov bx, cs:Screen_Mode.x_res[di]
    @@:
    cmp cs:Screen_Mode.pixel_size[di], 1
    je @F
        ; Mode 1
        shl bx, 1
        shl dx, 1
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

mono_NSETCX endp

; Set up for bit-blit via NREAD or NWRITE
; On entry: BX = pixel array
;           CX = number of bits
;           If C set:
;           AL = index to a drawing routine (0-4)
;                choices are 0 (OR), 1 (AND), 2 (PRESET), 3 (PSET), 4 (XOR)
; Modes 1, 2 and 3 all use this
mono_PGINIT proc near private

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

mono_PGINIT endp

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
; Modes 1, 2 and 3 all use this
mono_NREAD proc near private

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

mono_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
; Modes 1, 2 and 3 all use this
mono_NWRITE proc near private

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
    jae @multibyte

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

mono_NWRITE endp

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
; Modes 1, 2 and 3 all use this
mono_SCANR proc near private

    push ax
    push si
    push di
    push bp
    push es

    ; Skip right
    mov cl, cs:Screen_Mode.pixel_size[di]
    mov bp, cs:Screen_Mode.x_res[di]
    dec bp
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

mono_SCANR endp

; Fill pixels to the left until the border color is found
; On entry: Setup done with PNTINI
; Returns:  Start painting one pixel left of current position
;           BX = number of pixels painted
;           CL != 0 if at least one pixel changed
;           Current position updated
; Modes 1, 2 and 3 all use this
mono_SCANL proc near private

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

mono_SCANL endp

;-----------------------------------------------------------------------------
; Screen mode 3: Hercules 720x348, 2 colors
;-----------------------------------------------------------------------------

; Set up mode 3
herc_SCRSTT_init proc near private

    ; Set BIOS mode 8
    mov ax, 0008h
    int 10h

    ; Did it work?
    push bx
    mov ah, 0Fh
    int 10h
    pop bx
    cmp al, 8
    jne @error

    ; We're good.
    mov text_width, 80
    mov text_height, 25
    mov text_attr, 3
    mov cursor_pos, 0FFFFh
    mov video_seg, 0B000h
    clc
    ret

    ; Oops.
    @error:
    stc
    ret

herc_SCRSTT_init endp

; Set the active page
herc_SCRSTT_actpage proc near private

    push ax

    ; Use video_seg to select the page
    mov page_offset, 0
    mov page_bitmask, 0FFFFh
    shr al, 1
    mov ax, 0B000h
    jnc @F
        mov ax, 0B800h
    @@:
    mov video_seg, ax

    pop ax
    clc
    ret

herc_SCRSTT_actpage endp

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
herc_SCROLL proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push bp

    ; Convert coordinates to 0-based
    dec ah
    dec al
    dec bh
    dec bl

    ; Save the registers
    mov scroll_c1, ah
    mov scroll_r1, al
    mov scroll_c2, bh
    mov scroll_r2, bl
    mov scroll_width, ch
    mov scroll_height, cl

    ; Number of lines to scroll
    mov ah, 14
    mov al, scroll_height
    mul ah
    mov si, ax

    ; Number of pixels in each line to scroll
    mov ah, 9
    mov al, scroll_width
    mul ah
    mov bp, ax

    ; Which way should we copy?
    mov ah, scroll_r1
    mov al, scroll_c1
    mov bh, scroll_r2
    mov bl, scroll_c2
    cmp ax, bx
    jb @copy_backwards

        ; Scrolling upward or leftward

        ; Convert the coordinates to addresses:
        ; "From" location
        mov ah, 9
        mov al, scroll_c1
        mul ah
        mov cx, ax
        mov ah, 14
        mov al, scroll_r1
        mul ah
        mov dx, ax
        call herc_xy_to_pixel
        mov scroll_from, bx
        mov scroll_from_pixel, cl
        ; "To" location
        mov ah, 9
        mov al, scroll_c2
        mul ah
        mov cx, ax
        mov ah, 14
        mov al, scroll_r2
        mul ah
        mov dx, ax
        call herc_xy_to_pixel
        mov scroll_to, bx
        mov scroll_to_pixel, cl

        ; Line count to DX
        mov dx, si

        ; Copy lines
        @fwd_copy:
            call herc_copy_line
            mov bx, scroll_from
            call herc_next_line
            mov scroll_from, bx
            mov bx, scroll_to
            call herc_next_line
            mov scroll_to, bx
        dec dx
        jnz @fwd_copy

    jmp @end_scroll
    @copy_backwards:

        ; Scrolling downward or rightward

        ; Convert the coordinates to addresses:
        ; "From" location
        mov ah, 9
        mov al, scroll_c1
        mul ah
        mov cx, ax
        mov ah, 14
        mov al, scroll_r1
        add al, scroll_height
        mul ah
        mov dx, ax
        call herc_xy_to_pixel
        mov scroll_from, bx
        mov scroll_from_pixel, cl
        ; "To" location
        mov ah, 9
        mov al, scroll_c2
        mul ah
        mov cx, ax
        mov ah, 14
        mov al, scroll_r2
        add al, scroll_height
        mul ah
        mov dx, ax
        call herc_xy_to_pixel
        mov scroll_to, bx
        mov scroll_to_pixel, cl

        ; Line count to DX
        mov dx, si

        ; Copy lines
        @back_copy:
            mov bx, scroll_from
            call herc_prev_line
            mov scroll_from, bx
            mov bx, scroll_to
            call herc_prev_line
            mov scroll_to, bx
            call herc_copy_line
        dec dx
        jnz @back_copy

    @end_scroll:

    pop bp
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

herc_SCROLL endp

; Copy a line while scrolling
; On entry: scroll_from, scroll_from_pixel, scroll_to and scroll_to_pixel set
;           BP = number of pixels to copy
herc_copy_line proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    cld

    ; Copy directly if the columns are equal
    mov al, scroll_c1
    cmp al, scroll_c2
    jne @shifting_copy

        ; Set up addresses
        mov bl, scroll_from_pixel
        mov si, scroll_from
        mov di, scroll_to
        mov es, video_seg

        ; Copy the left byte
        xor bh, bh
        mov ah, left_mask_1[bx]
        lodsb es:[si]
        xor al, es:[di]
        and al, ah
        xor al, es:[di]
        stosb

        ; Copy whole bytes
        add bx, bp
        dec bx
        mov cx, bx
        shr cx, 1
        shr cx, 1
        shr cx, 1
        dec cx
        rep movsb es:[di], es:[si]

        ; Copy the right byte
        and bx, 7
        mov ah, right_mask_1[bx]
        lodsb es:[si]
        xor al, es:[di]
        and al, ah
        xor al, es:[di]
        stosb

    jmp @end
    @shifting_copy:

        ; Copy whole bytes to scroll_buf
        mov bl, scroll_from_pixel
        xor bh, bh
        add bx, bp
        add bx, 7
        mov cl, 3
        shr bx, cl
        mov cx, bx
        mov si, scroll_from
        mov di, offset scroll_buf
        mov ax, ds
        mov es, ax
        mov ds, video_seg
        assume es:DSEG, ds:nothing
        rep movsb
        mov ax, es
        mov ds, ax
        assume ds:DSEG, es:nothing

        ; Shift to align with the destination
        ; BX is the number of bytes just copied
        mov cl, scroll_from_pixel
        sub cl, scroll_to_pixel
        je @end_shift
        jc @shift_right
            ; Shift left
            ; CL is the shift count
            ; DI points to the byte after the copied bytes in scroll_buf
            dec di
            std
            xor ch, ch ; Initial carry
            @lshift:
                mov al, [di]
                xor ah, ah
                shl ax, cl
                or al, ch       ; Carry in
                mov ch, ah      ; Carry out
                stosb
            dec bx
            jnz @lshift
            cld
        jmp @end_shift
        @shift_right:
            ; Shift right
            neg cl
            xor ch, ch ; Initial carry
            mov di, offset scroll_buf
            @rshift:
                mov ah, [di]
                xor al, al
                shr ax, cl
                xchg al, ah
                or al, ch       ; Carry in
                mov ch, ah      ; Carry out
                stosb
            dec bx
            jnz @rshift
            mov [di], bl
        @end_shift:

        ; Set up addresses
        mov si, offset scroll_buf
        mov di, scroll_to
        mov es, video_seg

        ; Copy the left byte
        mov bl, scroll_to_pixel
        xor bh, bh
        mov ah, left_mask_1[bx]
        lodsb
        xor al, es:[di]
        and al, ah
        xor al, es:[di]
        stosb

        ; Copy whole bytes
        add bx, bp
        dec bx
        mov cx, bx
        shr cx, 1
        shr cx, 1
        shr cx, 1
        dec cx
        rep movsb

        ; Copy the right byte
        and bx, 7
        mov ah, right_mask_1[bx]
        lodsb
        xor al, es:[di]
        and al, ah
        xor al, es:[di]
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

herc_copy_line endp

; Clear the screen or the text window
; On entry: If the CLS statement has a parameter, C is set and the parameter
;           is in AL.
;           Otherwise, C is clear and AL is 0.
; Returns:  C clear if screen cleared
;           SCNCLR and GRPINI called
herc_CLRSCN proc near private

    cmp al, 0
    stc
    jne @end

        push ax
        push bx
        push cx
        push dx
        push di
        push es

        cld

        ; Number of raster lines to clear
        mov al, max_line
        inc al
        mov ah, 14
        mul ah
        mov dx, ax

        ; BX <- limit of addresses to clear
        xor cx, cx
        call herc_xy_to_pixel ; address to BX

        mov dl, 4
        xor ax, ax
        mov es, video_seg
        @clear_bank:

            mov di, bx
            and di, 6000h
            mov cx, bx
            and cx, 1FFFh
            shr cx, 1
            rep stosw

        call herc_next_line
        dec dl
        jnz @clear_bank

        mov cursor_pos, 0FFFFh

        call SCNCLR
        call GRPINI

        pop es
        pop di
        pop dx
        pop cx
        pop bx
        pop ax

        clc

    @end:
    ret

herc_CLRSCN endp

; On entry: AL = cursor type
;                0 = off
;                1 = insert mode (larger)
;                2 = overwrite mode (smaller)
;                3 = user mode
;           DX = cursor position: 1-based (column, row)
; Returns: none
herc_CSRDSP proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push es

    ; Develop the address of the text cell
    push ax
    mov al, dh
    dec al
    mov ah, 9
    mul ah
    mov cx, ax
    mov al, dl
    dec al
    mov ah, 14
    mul ah
    mov dx, ax
    call herc_xy_to_pixel ; address to BX; pixel offset to CL
    mov si, bx
    mov bx, 0FF80h
    shr bx, cl            ; bits to flip
    xchg bl, bh
    pop ax
    ; SI has the address of the first line; BX has the bit mask to flip

    ; Remove any existing cursor
    call herc_cursor_off

    cmp al, 0
    je @end

        ; Set the cursor shape
        cmp al, 1
        jne @two
            mov ax, 000Dh
            jmp @on
        @two:
        cmp al, 2
        jne @three
            mov ax, 0B0Dh
            jmp @on
        @three:
            mov ax, cursor_shape
        @on:

        ; Display the new cursor
        mov es, video_seg
        call herc_cursor_flip
        mov cursor_pos, si
        mov cursor_state, ax
        mov cursor_mask, bx

    @end:

    pop es
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

herc_CSRDSP endp

; Turn off the cursor
herc_cursor_off proc near private

    cmp cursor_pos, 0FFFFh
    je @end ; it isn't already off
        push ax
        push bx
        push si
        push es

        mov es, video_seg
        mov si, cursor_pos
        mov ax, cursor_state
        mov bx, cursor_mask
        call herc_cursor_flip

        pop es
        pop si
        pop bx
        pop ax
        mov cursor_pos, 0FFFFh
    @end:

    ret

herc_cursor_off endp

; Reverse the area of text that the cursor occupies
; On entry: AH = start line; AL = end_line
;           ES:SI = address of line 0 of the text cell
;           BX = FFFF for mode 1, 00FF for mode 2
herc_cursor_flip proc near private

    push cx
    push si

    mov cl, 0
    xchg bx, si
    @flip:

        cmp cl, ah
        jb @F
        cmp cl, al
        ja @F
            xor word ptr es:[bx], si
        @@:
        inc cl
        call herc_next_line

    cmp cl, 14
    jb @flip
    mov bx, si

    pop si
    pop cx
    ret

herc_cursor_flip endp

; Set screen width in columns
; On entry: AL = number of requested columns
; Returns C set if error
herc_SWIDTH proc near private

    ; Width is fixed at 80 columns
    cmp al, 80
    jne @error
        clc
        ret
    @error:
        stc
        ret

herc_SWIDTH endp

; Set a cursor position retrieved from FETCHC
; On entry: AL:BX = cursor position
; Returns:  none
herc_STOREC proc near private

    push ax
    push dx

    mov video_pos, bx
    mov video_bitmask, al

    ; Recover the X coordinate
    mov ax, bx
    and ax, 1FFFh   ; exclude the bank bits
    mov dl, 90
    div dl          ; AH <- byte offset within line
    mov dl, ah      ; to 16 bit in DX
    xor dh, dh
    shl dx, 1       ; convert to pixel coordinate
    shl dx, 1
    shl dx, 1
    mov al, video_bitmask ; get the lower three bits
    test al, 0Fh
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

herc_STOREC endp

; Move one pixel up
; On entry: none
; Returns   none
herc_UPC proc near private

    xchg bx, video_pos
    call herc_prev_line
    xchg bx, video_pos
    ret

herc_UPC endp

; Move BX to the previous line
herc_prev_line proc near private

    sub bx, 2000h
    jns @F
        add bx, 8000h - 90
    @@:
    ret

herc_prev_line endp

; Move one pixel down
; On entry: none
; Returns   none
herc_DOWNC proc near private

    xchg bx, video_pos
    call herc_next_line
    xchg bx, video_pos
    ret

herc_DOWNC endp

; Move BX to the next line
herc_next_line proc near private

    add bx, 2000h
    jns @F
        sub bx, 8000h - 90
    @@:
    ret

herc_next_line endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
herc_MAPXYC proc near private

    push ax
    push bx
    push cx

    mov x_coordinate, cx

    call herc_xy_to_pixel
    mov video_pos, bx
    mov al, 80h
    shr al, cl
    mov video_bitmask, al

    pop cx
    pop bx
    pop ax
    ret

herc_MAPXYC endp

; Map pixel coordinates to an address and a shift count
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: BX = offset into page
;          CL = rightward shift count from bit 7
herc_xy_to_pixel proc near private

    push ax
    push dx

    ; Right two bits of Y to the bank number
    ror dx, 1
    ror dx, 1
    mov bh, dh
    shr bh, 1
    and bx, 6000h

    ; Derive offset from the rest of the Y coordinate
    mov al, 90
    mul dl
    add bx, ax

    ; Byte offset into line
    mov ax, cx
    shr ax, 1
    shr ax, 1
    shr ax, 1
    add bx, ax

    ; Pixel shift count
    and cl, 7

    pop dx
    pop ax
    ret

herc_xy_to_pixel endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
herc_TDOWNC proc near private

    xchg bx, video_pos
    cmp bx, 7E3Ch
    jae @error
    call herc_next_line
    xchg bx, video_pos

    clc
    ret

@error:
    xchg bx, video_pos
    stc
    ret

herc_TDOWNC endp

herc_TUPC proc near private

    xchg bx, video_pos
    cmp bx, 90
    jb @error
    call herc_prev_line
    xchg bx, video_pos

    clc
    ret

@error:
    xchg bx, video_pos
    stc
    ret

herc_TUPC endp

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
; Modes 2 and 3 both use this
mono_SETATR proc near private

    ; Duplicate lower bit through the entire byte
    and al, 1
    neg al
    mov graph_attr, al
    clc
    ret

mono_SETATR endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
; Modes 2 and 3 both use this
mono_PNTINI proc near private

    ; Duplicate lower bit through the entire byte
    and al, 1
    neg al
    mov border_attr, al
    clc
    ret

mono_PNTINI endp

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

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
ega_SCROLL proc near private

    push ax
    push bx
    push cx
    push dx
    push bp
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

    ; Select read mode 0 and write mode 1
    write_ega_reg 03CEh, 5, 01h
    ; Write all planes
    write_ega_reg 03C4h, 2, 0Fh

    ; Get the font height
    mov ax, cs:Screen_Mode.y_res[di]
    div cs:Screen_Mode.text_rows[di]
    mov bh, al
    ; Get the number of bytes per line
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1
    shr ax, 1
    shr ax, 1
    mov bl, al

    ; Get the number of bytes per text row
    mul bh
    mov cx, ax

    ; Convert coordinates to addresses
    mov al, scroll_r1
    xor ah, ah
    mul cx              ; bytes per text row
    add al, scroll_c1
    adc ah, 0
    add ax, page_offset
    mov scroll_from, ax

    mov al, scroll_r2
    xor ah, ah
    mul cx              ; bytes per text row
    add al, scroll_c2
    adc ah, 0
    add ax, page_offset
    mov scroll_to, ax

    ; Address the frame buffer
    mov es, video_seg

    ; Which way are we scrolling?
    mov ax, scroll_to
    cmp ax, scroll_from
    jae @scroll_higher
        ; Scroll from higher to lower address
        cld
        mov al, scroll_height
        mul bh                  ; lines per text row
        mov dx, ax
        @scroll_1:
            ; Scroll one line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Go to next line
            mov al, bl
            xor ah, ah
            add scroll_from, ax
            add scroll_to, ax
        dec dx
        jnz @scroll_1
    jmp @scroll_end
    @scroll_higher:
        ; Scroll from lower to higher address
        ; Point starting addresses to last cell to scroll
        mov al, scroll_height
        mul bh                  ; AX <- lines to scroll
        push ax
        dec ax                  ; AX <- lines to scroll - 1
        mov dl, bl
        xor dh, dh
        mul dx                  ; AX <- displacement to last scrolled line
        add al, scroll_width
        adc ah, 0
        dec ax
        add scroll_from, ax
        add scroll_to, ax
        ; Begin loop
        std
        pop dx                  ; DX <- lines to scroll
        @scroll_2:
            ; Scroll one line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsb es:[di], es:[si]
            ; Go to next line
            mov al, bl
            xor ah, ah
            sub scroll_from, ax
            sub scroll_to, ax
        dec dx
        jnz @scroll_2
    @scroll_end:

    ; Select read mode 0 and write mode 0
    write_ega_reg 03CEh, 5, 00h

    pop es
    pop di
    pop si
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ega_SCROLL endp

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
        mov di, page_offset
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
    add si, page_offset
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
ega_SETCLR proc near private

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
ega_SWIDTH_80 proc near private

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
ega_SWIDTH_40 proc near private

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
ega_STOREC proc near private

    push ax
    push bx
    push dx

    and bx, page_bitmask
    add bx, page_offset
    mov video_pos, bx
    mov video_bitmask, al

    ; Recover the X coordinate
    mov ax, bx
    and ax, page_bitmask
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
    pop bx
    pop ax
    ret

ega_STOREC endp

; Move one pixel up
; On entry: none
; Returns   none
ega_UPC proc near private

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
ega_DOWNC proc near private

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
ega_LEFTC proc near private

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
ega_RIGHTC proc near private

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
ega_MAPXYC proc near private
 
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
    add ax, page_offset
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
ega_READC proc near private

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
ega_SETC proc near private

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
ega_NSETCX proc near private

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
ega_PGINIT proc near private

    mov blit_addr, bx
    mov blit_bits, cx
    jnc @F
        ; Set blit_mixer
        push ax
        xor ah, ah
        shl ax, 1
        mov bx, ax
        mov ax, word ptr blit_op[bx]
        mov blit_mixer, ax
        pop ax
    @@:
    ; Set blit_bytes
    add cx, 7
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov blit_bytes, cx
    ; Set ega_blit_pixels
    mov cx, blit_bits
    shr cx, 1
    shr cx, 1
    mov ega_blit_pixels, cx
    ; Set ega_blit_whole_bytes
    and cx, 07h
    inc cx
    shr cx, 1
    mov bx, blit_bytes
    sub bx, cx
    shr bx, 1
    shr bx, 1
    mov ega_blit_whole_bytes, bx
    ; Preserve BX and CX through this function
    mov bx, blit_addr
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
ega_NREAD proc near private

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    cld

    ; The assumption here is that memory is fast and I/O ports are slow.
    ; We read each plane into a separate byte array, which may be slightly
    ; larger than the one provided by the caller. Whole bytes are lined up
    ; in the caller's array, and the odd bits are packed at the end.
    mov ega_blit_plane, 3
    mov ega_blit_bits, 0
    mov ega_odd_bits+0, 0
    @plane:
        ; Select plane to read
        write_ega_reg 03CEh, 4, ega_blit_plane

        ; Read a plane into the caller's buffer
        mov dx, ega_blit_pixels
        add dx, 7
        shr dx, 1
        shr dx, 1
        shr dx, 1
        mov cx, x_coordinate
        mov di, blit_addr
        les si, video_addr
        and cl, 7
        je @no_lshift
            mov ch, es:[si]     ; Initial carry
            inc si
            shl ch, cl
            @lshift:
                mov al, es:[si] ; from frame buffer
                inc si
                xor ah, ah
                shl ax, cl
                or  ah, ch      ; Carry in
                mov ch, al      ; Carry out
                mov [di], ah
                inc di
            dec dx
            jnz @lshift
        jmp @end_lshift
        @no_lshift:
            ; Shift is zero; just copy
            @copy:
                mov al, es:[si] ; from frame buffer
                inc si
                mov [di], al
                inc di
            dec dx
            jnz @copy
        @end_lshift:

        ; If there are odd bits, retrieve them from the last byte copied
        mov bx, ega_blit_pixels
        and bx, 07h
        je @end_odd_bits
            dec di
            mov ah, [di]            ; AH <- the odd bits at the left
            xor al, al
            and ah, (right_mask_1-1)[bx] ; Ignore bits beyond the odd bits
            mov si, ega_blit_bits
            mov cx, si
            shr si, 1
            shr si, 1
            shr si, 1               ; SI <- offset in ega_odd_bits
            and cx, 07h             ; CL <- number of bits to shift the odd bits
            shr ax, cl
            or  ega_odd_bits[si+0], ah ; Add the odd bits to ega_odd_bits
            mov ega_odd_bits[si+1], al
        @end_odd_bits:

        mov blit_addr, di

        ; Add to the number of odd bits packed
        add ega_blit_bits, bx
    dec ega_blit_plane
    jns @plane

    ; Append the odd bits to the caller's array
    mov cx, ega_blit_bits
    add cx, 7
    shr cx, 1
    shr cx, 1
    shr cx, 1
    mov si, offset ega_odd_bits
    mov di, blit_addr
    mov ax, ds
    mov es, ax
    rep movsb
    mov blit_addr, di

    pop es
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
ega_NWRITE proc near private

    push ax
    push bx
    push cx
    push dx
    push bp
    push si
    push di
    push es

    cld

    mov bx, x_coordinate
    and bx, 07h ; this is the shift count
    mov ah, byte ptr blit_mixer+0
    or ah, bl
    write_ega_reg 03CEh, 1, 0  ; No set/reset
    write_ega_reg 03CEh, 3, ah ; Selected operation, rotate by the shift count

    ; Copy the odd bits to ega_odd_bits
    mov cx, ega_blit_pixels
    and cx, 07h             ; CL <-- odd bits per plane
    je @end_odd_bits_1
        mov si, ega_blit_whole_bytes
        shl si, 1
        shl si, 1
        mov ax, blit_bytes
        sub ax, si          ; AX <- number of bytes containing odd bits
        add si, blit_addr   ; SI <- location of odd bits in caller's buffer
        mov bh, 0[si]       ; BH <- first byte of odd bits
        cmp al, 1
        jbe @F
            mov bl, 1[si]   ; BL <- second byte of odd bits
        @@:
        cmp al, 2
        jbe @F
            mov dh, 2[si]   ; DH <- third byte of odd bits
        @@:
        cmp al, 3
        jbe @F
            mov dl, 3[si]   ; DL <- third byte of odd bits
        @@:

        mov ch, 8
        sub ch, cl          ; CH <- right shift count for carry

        mov di, 3           ; Current plane
        @odd_bits:
            mov ega_odd_bits[di], bh ; Set odd bits
            shl bx, cl              ; Shift 32 bits by CL
            mov al, dh
            shl dx, cl
            xchg cl, ch             ; Carry
            shr al, cl
            xchg cl, ch
            or bl, al
        dec di
        jns @odd_bits
    @end_odd_bits_1:

    mov ega_blit_plane, 3
    @plane:
        ; Enable write to the current plane
        mov cl, ega_blit_plane
        mov ah, 1
        shl ah, cl
        write_ega_reg 03C4h, 2, ah

        ; If there are odd bits, swap them into the caller's buffer
        test ega_blit_pixels, 07h
        je @end_odd_bits_2
            mov bl, ega_blit_plane
            xor bh, bh
            mov si, ega_blit_whole_bytes
            add si, blit_addr
            mov al, [si]
            mov ah, ega_odd_bits[bx]
            mov [si], ah
            mov ega_odd_bits[bx], al
        @end_odd_bits_2:

        ; Does the transfer cross a byte boundary?
        mov bx, x_coordinate
        and bx, 07h
        mov bp, bx
        add bp, ega_blit_pixels
        dec bp
        cmp bp, 8
        jae @multibyte

            ; Transfer lies within a single byte
            ; Build bit mask in AH
            mov ah, left_mask_1[bx]
            and ah, right_mask_1[bp]
            write_ega_reg 03CEh, 8, ah ; Bits to transfer

            ; Byte to transfer in AL
            mov si, blit_addr
            lodsb

            ; Invert if the configured operation calls for it
            xor al, byte ptr blit_mixer+1

            ; Transfer to frame buffer
            les di, video_addr
            mov dl, es:[di]
            stosb

        jmp @end
        @multibyte:

            ; Left and right masks
            mov bh, left_mask_1[bx]
            push bp
            and bp, 07h
            mov bl, right_mask_1[bp]
            pop bp

            ; Number of whole bytes to copy
            shr bp, 1
            shr bp, 1
            shr bp, 1

            ; Invert if the configured operation calls for it
            mov ah, byte ptr blit_mixer+1

            ; Transfer left portion of bytes
            mov si, blit_addr
            les di, video_addr
            mov cx, bp        
            write_ega_reg 03CEh, 8, bh
            @left_copy:
                lodsb
                xor al, ah
                mov dl, es:[di]
                stosb
            loop @left_copy
            ; Transfer last byte
            mov cl, bh
            and cl, bl
            jz @F
                write_ega_reg 03CEh, 8, cl
                lodsb
                xor al, ah
                mov dl, es:[di]
                stosb
            @@:

            ; Transfer right portion of bytes
            not bh
            mov si, blit_addr
            les di, video_addr
            inc di
            mov cx, bp        
            dec cx
            jz @end_right_copy
            write_ega_reg 03CEh, 8, bh
            @right_copy:
                lodsb
                xor al, ah
                mov dl, es:[di]
                stosb
            loop @right_copy
            @end_right_copy:
            ; Transfer last byte
            mov cl, bh
            and cl, bl
            jz @F
                write_ega_reg 03CEh, 8, cl
                lodsb
                xor al, ah
                mov dl, es:[di]
                stosb
            @@:

        @end:

        ; Advance to the next set of whole bytes
        mov si, blit_addr
        add si, ega_blit_whole_bytes
        mov blit_addr, si

        ; If there are odd bits, swap them into the caller's buffer
        test ega_blit_pixels, 07h
        je @end_odd_bits_3
            mov bl, ega_blit_plane
            xor bh, bh
            mov al, [si]
            mov ah, ega_odd_bits[bx]
            mov [si], ah
            mov ega_odd_bits[bx], al
        @end_odd_bits_3:

    dec ega_blit_plane
    jns @plane

    ; Advance the local memory address
    mov cx, ega_blit_pixels
    and cx, 07h
    inc cx
    shr cx, 1           ; CX <- number of bytes worth of odd bits
    add blit_addr, cx

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

; Read a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          main memory address advanced to next line
;          pixels read in packed form into main memory
;
; Used instead of ega_NREAD in monochrome modes (10 and 11)
ega_mono_NREAD proc near private

    push ax
    push cx
    push dx
    push si
    push di
    push es

    ; Only one plane to read, so read directly into the caller's array

    ; Select plane to read
    write_ega_reg 03CEh, 4, 0

    mov dx, blit_bytes  ; Number of bytes
    les si, video_addr  ; Copy from
    mov di, blit_addr   ; Copy to

    ; Shift so that no extra pixels appear to the left
    mov cx, x_coordinate
    and cx, 7       ; also CH <- 0; this is the carry byte
    je @no_lshift   ; Skip if no shift needed
        mov al, es:[si]
        inc si
        xor ah, ah
        shl ax, cl
        mov ch, ah      ; Initial carry
        @lshift:
            mov al, es:[si]
            inc si
            xor ah, ah
            shl ax, cl
            or al, ch   ; Carry in
            mov ch, ah  ; Carry out
            mov [di], al
            inc di
        dec dx
        jnz @lshift
    jmp @end_lshift
    @no_lshift:
        ; Shift is zero; just copy
        @copy:
            mov al, es:[si]
            inc si
            mov [di], al
            inc di
        dec dx
        jnz @copy
    @end_lshift:

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop ax
    ret

ega_mono_NREAD endp

; Write a line of pixels
; On entry: PGINIT complete
; Returns: none in registers
;          local memory address advanced to the next line
;
; Used instead of ega_NWRITE in monochrome modes (10 and 11)
ega_mono_NWRITE proc near private

    push ax
    push bx
    push cx
    push dx
    push bp
    push si
    push di
    push es

    cld

    mov bx, x_coordinate
    and bx, 07h ; this is the shift count
    mov ah, byte ptr blit_mixer+0
    or ah, bl
    write_ega_reg 03CEh, 1, 0  ; No set/reset
    write_ega_reg 03CEh, 3, ah ; Selected operation, rotate by the shift count

    ; Does the transfer cross a byte boundary?
    mov bp, bx
    add bp, blit_bits
    dec bp
    cmp bp, 8
    jae @multibyte

        ; Transfer lies within a single byte
        ; Build bit mask in AH
        mov ah, left_mask_1[bx]
        and ah, right_mask_1[bp]
        write_ega_reg 03CEh, 8, ah ; Bits to transfer

        ; Byte to transfer in AL
        mov si, blit_addr
        lodsb

        ; Invert if the configured operation calls for it
        xor al, byte ptr blit_mixer+1

        ; Transfer to frame buffer
        les di, video_addr
        mov dl, es:[di]
        stosb

    jmp @end
    @multibyte:

        ; Left and right masks
        mov bh, left_mask_1[bx]
        push bp
        and bp, 07h
        mov bl, right_mask_1[bp]
        pop bp

        ; Number of whole bytes to copy
        shr bp, 1
        shr bp, 1
        shr bp, 1

        ; Invert if the configured operation calls for it
        mov ah, byte ptr blit_mixer+1

        ; Transfer left portion of bytes
        mov si, blit_addr
        les di, video_addr
        mov cx, bp        
        write_ega_reg 03CEh, 8, bh
        @left_copy:
            lodsb
            xor al, ah
            mov dl, es:[di]
            stosb
        loop @left_copy
        ; Transfer odd bits
        mov cl, bh
        and cl, bl
        jz @F
            write_ega_reg 03CEh, 8, cl
            lodsb
            xor al, ah
            mov dl, es:[di]
            stosb
        @@:

        ; Transfer right portion of bytes
        not bh
        mov si, blit_addr
        les di, video_addr
        inc di
        mov cx, bp        
        dec cx
        jz @end_right_copy
        write_ega_reg 03CEh, 8, bh
        @right_copy:
            lodsb
            xor al, ah
            mov dl, es:[di]
            stosb
        loop @right_copy
        @end_right_copy:
        ; Transfer odd bits
        mov cl, bh
        and cl, bl
        jz @F
            write_ega_reg 03CEh, 8, cl
            lodsb
            xor al, ah
            mov dl, es:[di]
            stosb
        @@:

    @end:

    ; Advance the local memory address
    mov ax, blit_bytes
    add blit_addr, ax

    mov ega_set_pixel, 1
    write_ega_reg 03CEh, 3, 0 ; Replace operation, no rotation

    pop es
    pop di
    pop si
    pop bp
    pop dx
    pop cx
    pop bx
    pop ax
    ret

ega_mono_NWRITE endp

; Set up flood fill algorithm
; On entry: AL = border attribute
; Returns: C set if error
ega_PNTINI proc near private

    mov border_attr, al
    clc
    ret

ega_PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
ega_TDOWNC proc near private

    push ax
    push bx
    push dx

    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1                       ; AX <- x_res/8
    shr ax, 1
    shr ax, 1
    mov bx, ax                      ; BX <- x_res/8
    mul cs:Screen_Mode.y_res[di]    ; AX <- (x_res/8)*y_res
    mov dx, video_pos
    and dx, page_bitmask
    add bx, dx                      ; BX <- new position
    cmp ax, bx                      ; Set C if off the bottom edge
    jc @F
        add bx, page_offset
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
ega_TUPC proc near private

    push ax
    push bx
    mov ax, cs:Screen_Mode.x_res[di]
    shr ax, 1
    shr ax, 1
    shr ax, 1
    mov bx, video_pos
    and bx, page_bitmask
    sub bx, ax
    jc @F
        add bx, page_offset
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
ega_SCANR proc near private

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
    mov cl, cs:Screen_Mode.pixel_size[di]
    mov ah, 1
    shl ah, cl
    dec ah
    write_ega_reg 03CEh, 7, ah          ; Significant bits of the color
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
ega_SCANL proc near private

    push ax
    push dx
    push si
    push di
    push es

    mov ega_set_pixel, 1

    ; Set up read mode 1 and border color as target
    write_ega_reg 03CEh, 5, 08h         ; Read mode 1
    write_ega_reg 03CEh, 2, border_attr ; Color compare register
    mov cl, cs:Screen_Mode.pixel_size[di]
    mov ah, 1
    shl ah, cl
    dec ah
    write_ega_reg 03CEh, 7, ah          ; Significant bits of the color

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

; Scroll window beginning at column AH, row AL,
; extending through CH columns and CL rows,
; to column BH, row BL
; Rows and columns begin at 1
; Returns: None
vga_SCROLL proc near private

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
    shl ch, 1
    shl ch, 1
    mov scroll_width, ch  ; as word count per line
    shl cl, 1
    shl cl, 1
    shl cl, 1
    mov scroll_height, cl ; as line count

    ; Convert coordinates to addresses
    ; addr = row*320*8 + col*8
    mov bl, 160
    mov al, scroll_r1
    mul bl
    shl ax, 1
    add al, scroll_c1
    adc ah, 0
    shl ax, 1
    shl ax, 1
    shl ax, 1
    mov scroll_from, ax
    mov al, scroll_r2
    mul bl
    shl ax, 1
    add al, scroll_c2
    adc ah, 0
    shl ax, 1
    shl ax, 1
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
            ; Scroll a line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsw es:[di], es:[si]
            ; Go to next line
            mov ax, 320
            add scroll_from, ax
            add scroll_to, ax
        dec dl
        jnz @scroll_1
    jmp @scroll_end
    @scroll_higher:
        ; Scroll from lower to higher address
        ; Point starting addresses to last cell to scroll
        mov al, scroll_height   ; AX <- ((scroll_height-1)*320)+(scroll_width-1)*2
        dec al
        mov cl, 160
        mul cl
        shl ax, 1
        mov cl, scroll_width
        xor ch, ch
        dec cx
        shl cx, 1
        add ax, cx
        add scroll_from, ax
        add scroll_to, ax
        ; Begin loop
        std
        mov dl, scroll_height
        @scroll_2:
            ; Scroll a line
            mov si, scroll_from
            mov di, scroll_to
            mov cl, scroll_width
            xor ch, ch
            rep movsw es:[di], es:[si]
            ; Go to next line
            mov ax, 320
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

vga_SCROLL endp

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
vga_SETCLR proc near private

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
vga_STOREC proc near private

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
vga_UPC proc near private

    sub video_pos, 320
    ret

vga_UPC endp

; Move one pixel down
; On entry: none
; Returns   none
vga_DOWNC proc near private

    add video_pos, 320
    ret

vga_DOWNC endp

; Move one pixel left
; On entry: none
; Returns   none
vga_LEFTC proc near private

    dec video_pos
    ret

vga_LEFTC endp

; Move one pixel right
; On entry: none
; Returns   none
vga_RIGHTC proc near private

    inc video_pos
    ret

vga_RIGHTC endp

; Map pixel coordinates to a cursor position as returned by FETCHC
; On entry: CX = X coordinate
;           DX = Y coordinate
; Returns: none
vga_MAPXYC proc near private

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
vga_READC proc near private

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
vga_SETC proc near private

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
vga_NSETCX proc near private

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
vga_PGINIT proc near private

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
vga_NREAD proc near private

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
vga_NWRITE proc near private

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
vga_PNTINI proc near private

    mov border_attr, al
    clc
    ret

vga_PNTINI endp

; Move current position down with boundary check
; Returns: C set if moving down would pass the bottom of the screen;
;          the current position is unchanged in that case
; This differs from DOWNC only in the boundary check
vga_TDOWNC proc near private

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
vga_TUPC proc near private

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
vga_SCANR proc near private

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
vga_SCANL proc near private

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
;               DX = duration in 400ths of a second
; Returns: C set on error
PUBLIC DONOTE
DONOTE proc near

    cmp al, 0FFh
    jne @F
        call clear_sound_queue
        clc
        ret
    @@:

    ; Ensure that the sound queue exists
    call create_sound_queue
    jnc @F
        ret
    @@:

    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    ; Move parameters so we have DX:AX for the divide
    mov di, dx  ; duration
    mov bl, al  ; wait flag

    ; Divide
    or cx, cx
    je @end_divide  ; Frequency of 0 is a rest
        mov ax, 2386364 shr 16
        xor dx, dx
        div cx
        mov si, ax
        mov ax, 2386364 and 0FFFFh
        div cx
        ; SI:AX = 2386364/CX
        ; Halve and round to nearest
        add ax, 1
        adc si, 0
        shr si, 1
        rcr ax, 1
        ; SI:AX = 1193182/CX
        ; Bound the quotient to 1..65535
        test si, si
        je @low_bound
            mov ax, 0FFFFh
        jmp @end_bound
        @low_bound:
            test ax, ax
            jne @end_bound
                inc ax
        @end_bound:
    @end_divide:

    ; Wait until there is space in the sound queue
    call wait_sound_queue_space

    ; Address the sound queue
    mov es, sound_queue
    mov dl, sound_queue_tail
    xor dh, dh
    mov si, dx
    shl si, 1
    shl si, 1

    ; Insert the new sound
    mov es:[si+0], ax   ; frequency
    mov es:[si+2], di   ; duration
    inc sound_queue_tail

    ; Wait if requested
    test bl, bl
    jne @F
        call wait_sound_queue_empty
    @@:

    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    clc
    ret

DONOTE endp

; Create the sound queue if it does not already exist
; Return C clear on success, set on error (memory not available)
create_sound_queue proc near private

    cmp sound_queue, 0
    je @F
        clc
        ret
    @@:

    push ax
    push bx
    push es

    ; Allocate memory for the sound queue
    mov bx, 1024/16 ; for 256 sounds
    mov ah, 48h
    int 21h
    jc @error

    mov sound_queue, ax
    call clear_sound_queue

    pop es
    pop bx
    pop ax
    clc
    ret

@error:
    pop es
    pop bx
    pop ax
    stc
    ret

create_sound_queue endp

; Clear the sound queue and turn the speaker off
clear_sound_queue proc near private

    pushf
    cli
    mov sound_queue_head, 0
    mov sound_queue_tail, 0
    mov sound_time, 0
    call speaker_off
    popf
    ret

clear_sound_queue endp

; Wait until the sound queue is empty
wait_sound_queue_empty proc near private

    @wait:
        cmp sound_time, 0
        je @end_wait
        hlt
        call CHKKYB ; Poll for Ctrl-Break
    jmp @wait
    @end_wait:
    call speaker_off
    ret

wait_sound_queue_empty endp

; Wait until the sound queue is not full
wait_sound_queue_space proc near private

    push ax
    @wait:
        mov al, sound_queue_tail
        sub al, sound_queue_head
        cmp al, 255
        jb @end_wait
        hlt
        call CHKKYB ; Poll for Ctrl-Break
    jmp @wait
    @end_wait:
    pop ax
    ret

wait_sound_queue_space endp

; Sound timer tick
; This is called from an interrupt handler; it MUST preserve all registers!
sound_timer proc near private

    push ax

    ; If sound_time was not 0, and it becomes 0, and the queue is empty,
    ; turn the speaker off
    cmp sound_time, 0
    je @no_sound
        ; Sound is in progress
        ; Duration is in 400ths of a second; subtract 22 each time to match
        ; the timer frequency
        sub sound_time, 22
        ja @end
        mov sound_time, 0

        ; End of note
        mov al, sound_queue_head
        cmp al, sound_queue_tail
        je @stop_sound
            ; Set up the next note
            call play_note
        jmp @end
        @stop_sound:
            ; Queue is empty
            call speaker_off
    jmp @end
    @no_sound:
        ; No sound is in progress
        mov al, sound_queue_head
        cmp al, sound_queue_tail
        je @end
            call play_note
    @end:

    pop ax
    ret

sound_timer endp

; Play the next note in the queue
; On entry, the queue is not empty. If the last sound leaves a duration
; of 0, it will play until cancelled.
play_note proc near private

    push ax
    push si
    push es

    @next:
    mov al, sound_queue_head
    cmp al, sound_queue_tail
    je @end

        ; Get a note from the queue
        xor ah, ah
        mov si, ax
        shl si, 1
        shl si, 1
        mov es, sound_queue
        mov ax, es:[si]
        or ax, ax
        je @rest
            ; Note
            call speaker_on
        jmp @end_note
        @rest:
            ; Rest
            call speaker_off
        @end_note:
        mov ax, es:[si+2]
        mov sound_time, ax
        inc sound_queue_head
        or ax, ax
        je @next    ; Duration is 0; queue the next note if there is one

    @end:

    pop es
    pop si
    pop ax
    ret

play_note endp

; Turn the speaker on, with the frequency divisor in AX
speaker_on proc near private

    pushf
    cli
    push ax
    mov al, 10110110b ; Counter 2; write both low and high bytes; square wave
    out 43h, al       ; generator; binary count
    pop ax
    out 42h, al
    xchg al, ah
    out 42h, al
    xchg al, ah
    push ax
    in al, 61h
    or al, 03h        ; Enable the gate line
    out 61h, al
    pop ax
    popf
    ret

speaker_on endp

; Turn the speaker off
speaker_off proc near private

    push ax
    pushf
    cli
    in al, 61h
    and al, 0FCh        ; Disable the gate line
    out 61h, al
    popf
    pop ax
    ret

speaker_off endp

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

    push ax
    push bx
    push cx
    push si

    ; We allocate nothing here. Transmit and receive queues are allocated
    ; when a device is opened for the first time.

    ; Set default receive buffer size
    jnz @F
        mov dx, COM_rx_size
    @@:

    mov comx_rx_qsize, dx

    ; Set up fixed part of device blocks
    mov si, offset comx_params_list
    mov bx, offset comx_devices
    mov cx, NMCOMT
    @@:
        mov ax, cs:COMx_Params.io_addr[si]
        mov COMx_Device.io_addr[bx], ax
        mov al, cs:COMx_Params.irq[si]
        mov COMx_Device.irq[bx], al
    add si, COMx_Params
    add bx, COMx_Device
    loop @B

    ; No allocation here
    xor dx, dx
    pop si
    pop cx
    pop bx
    pop ax
    clc
    ret

COMx_Params struc
    io_addr dw ?
    irq     db ?
COMx_Params ends
comx_params_list COMx_Params <03F8h, 4>
                 COMx_Params <02F8h, 3>
                 COMx_Params <03E8h, 4>
                 COMx_Params <02E8h, 3>

SETCBF endp

; Set up COMx port
; On entry: DS:BX = address of DCB
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

; This describes the device control block provided by the caller
COMx_DCB struc

    _DEVID db ?             ;RS232 Channel ID (0..n)
    _BAUDR dw ?             ;baud rate (least significant byte 1st)
                            ;(0=disable device, 9600=9600 baud etc.)
    _BYTSZ db ?             ;bits per byte (4..8)
    _PARIT db ?             ;parity (0..4)=...(NONE, ODD, EVEN, MARK, SPACE)
        P_NONE  = 0
        P_ODD   = 1
        P_EVEN  = 2
        P_MARK  = 3
        P_SPACE = 4
    _STOPB db ?             ;(0..2)= (1, 1.5, 2) stop bits
        ST_1   = 0
        ST_1_5 = 1
        ST_2   = 2
    _RLSTO dw ?             ;RLSD (rec. line signal detect) timeout
    _CTSTO dw ?             ;CTS (clear to send) timeout
    _DSRTO dw ?             ;DSR (data set ready) timeout
                            ;All timeout values are in milliseconds.
                            ;0=infinite, LSB is always 1st.
                            ;Support of Timeout Flags by BIOS is
                            ;optional.
    _CMFLG db ?             ;Boolean attributes mask for this device
        _CMBIN=1O               ;(0/1)=ASCII/BINARY (ASC option not in filename)
        _CMRTS=2O               ;non-zero=Suppress Request-To-Send (RS option)
        _CMCOD=20O              ;non-zero=user specified ASC or BIN in filename
        _CMCTS=40O              ;non-zero=CTS parm not defaulted
        _CMCLF=100O             ;non-zero=Send line feed after CR
        _CMCRF=200O             ;non-zero=last char sent was Carriage Return

COMx_DCB ends

PUBLIC INICOM
INICOM proc near

    push bx
    push cx
    push dx
    push si
    push di

    ; Local device structure in DI
    mov ah, COMx_DCB._DEVID[bx]
    call comx_get_device
    jc @no_device
    cmp COMx_Device.open[di], 0
    jnz @no_device

    ; Allocate queues if not already allocated
    cmp COMx_Device.tx_queue[di], 0
    jnz @F
        push bx
        mov bx, (COM_tx_size+15)/16
        mov ah, 48h
        int 21h
        pop bx
        jc @no_device
        mov COMx_Device.tx_queue[di], ax
    @@:
    cmp COMx_Device.rx_queue[di], 0
    jnz @F
        push bx
        mov bx, comx_rx_qsize
        add bx, 15
        mov cl, 4
        shr bx, cl
        mov ah, 48h
        int 21h
        pop bx
        jc @no_device
        mov COMx_Device.rx_queue[di], ax
    @@:

    ; Empty the queues
    xor ax, ax
    mov COMx_Device.tx_head[di], ax
    mov COMx_Device.tx_tail[di], ax
    mov COMx_Device.rx_head[di], ax
    mov COMx_Device.rx_tail[di], ax

    ; Set the device flags
    mov al, COMx_DCB._CMFLG[bx]
    mov COMx_Device.flags[di], al

    ; Set the bit rate:
    ; Access the divisor
    mov dx, COMx_Device.io_addr[di]
    add dx, 3
    mov al, 80h
    out dx, al
    ; Get the bit rate
    mov cx, COMx_DCB._BAUDR[bx]
    cmp cx, 4
    jb @no_device   ; Avoid overflow when dividing
    ; As a special case, if the bit rate is 65535, set the divisor to 1.
    ; This will configure 115200 bps, the fastest that a COMx port can go.
    cmp cx, 65535
    jne @divide
        mov ax, 1
    jmp @set_divisor
    @divide:
        ; AX <- 115200/_BAUDR rounded to nearest
        mov dx, (115200*2) shr 16
        mov ax, (115200*2) and 0FFFFh
        div cx
        inc ax
        shr ax, 1
    @set_divisor:
    ; Set the divisor
    mov dx, COMx_Device.io_addr[di]
    out dx, al
    inc dx
    xchg al, ah
    out dx, al
    xchg al, ah
    ; Check that the divisor was written successfully
    mov cx, ax
    in al, dx
    mov ah, al
    dec dx
    in al, dx
    cmp ax, cx
    jne @no_device

    ; Line control register:
    ; bits 0-1: word size
    mov ah, COMx_DCB._BYTSZ[bx]
    cmp ah, 8
    ja @no_device
    sub ah, 5
    jc @no_device
    ; bit 2: number of stop bits
    mov al, COMx_DCB._STOPB[bx]
    ; If we set a 5 bit word, the 8250 generates 1.5 stop bits when bit 2 is
    ; set; if we set a longer word, the 8250 generates two stop bits.
    ; If the caller requests 1.5 stop bits and a 6 bit word or more, set two
    ; stop bits; if two stop bits and a 5 bit word, we can't guarantee two
    ; stop bits, so bail out
    cmp al, ST_2
    ja @no_device   ; error if setting is invalid
    jb @stop_ok     ; OK if 1 or 1.5 stop bits
    cmp ah, 0
    je @no_device   ; error if 5 bit word and 2 stop bits
    @stop_ok:
    cmp al, ST_1
    je @F
        or ah, 04h
    @@:
    ; bits 3-5: parity setting
    mov dl, COMx_DCB._PARIT[bx]
    cmp dl, P_SPACE
    ja @no_device   ; error if setting is invalid
    xor dh, dh
    mov si, dx
    or ah, parity_table[si]
    ; set the register
    ; This also switches io_addr+0 and 1 back to the normal registers
    mov dx, COMx_Device.io_addr[di]
    add dx, 3
    mov al, ah
    out dx, al

    ; Set modem control register
    inc dx              ; DX <- io_addr+4
    mov al, 00001011b   ; Set Out 2, RTS and DTR; clear Loop and Out 1
    test COMx_DCB._CMFLG[bx], _CMRTS
    jz @F
        and al, not 00000010b ; Clear RTS
    @@:
    out dx, al

    ; Read the status registers
    inc dx              ; DX <- io_addr+5, line status register
    in al, dx
    inc dx              ; DX <- io_addr+6, modem status register
    in al, dx

    ; Mark as open and Tx inactive, and clear errors
    mov COMx_Device.open[di], 1
    mov COMx_Device.tx_active[di], 0
    mov COMx_Device.errors[di], 0

    ; Set the interrupt enable register
    mov dx, COMx_Device.io_addr[di]
    add dx, 1
    mov al, 00000111b   ; Enable Tx, Rx, line status
    out dx, al

    ; Enable the interrupt at the interrupt controller
    mov cl, COMx_Device.irq[di]
    mov ah, 0FEh
    rol ah, cl
    in al, 21h
    and al, ah
    out 21h, al

    mov ah, 0
@end:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    ret

@no_device:
    mov ah, 0FFh
    jmp @end

parity_table db 00000000b ; P_NONE  = 0: enable parity = 0
             db 00001000b ; P_ODD   = 1: enable parity = 1, even parity = 0, stick parity = 0
             db 00011000b ; P_EVEN  = 2: enable parity = 1, even parity = 1, stick parity = 0
             db 00101000b ; P_MARK  = 3: enable parity = 1, even parity = 0, stick parity = 1
             db 00111000b ; P_SPACE = 4: enable parity = 1, even parity = 1, stick parity = 1

INICOM endp

; Receive a byte from a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2, etc.
; On return:  AH = 0
;             Z clear if byte received
;             AL = byte received
; On error:   AH = nonzero error code, as for STACOM
PUBLIC RECCOM
RECCOM proc near

    push bx
    push di
    push es

    ; Local device structure in DI
    call comx_get_device
    jc @no_device
    cmp COMx_Device.open[di], 0
    jz @no_device

    ; Check for received errors
    xor al, al
    xchg COMx_Device.errors[di], al
    test al, 02h ; overrun
    jnz @overrun
    test al, 04h ; parity
    jnz @parity
    test al, 08h ; framing
    jnz @io_error

    xor ah, ah  ; No error if we return by this path

    ; Is there a byte available?
    mov bx, COMx_Device.rx_head[di]
    cmp bx, COMx_Device.rx_tail[di]
    je @end     ; return with Z set for no character

    ; A byte is available
    mov es, COMx_Device.rx_queue[di]
    mov al, es:[bx]
    inc bx
    cmp bx, comx_rx_qsize
    jb @F
        xor bx, bx
    @@:
    mov COMx_Device.rx_head[di], bx

    ; Clear Z before returning by this path
    mov bx, 1
    or bx, bx
    
@end:
    pop es
    pop di
    pop bx
    ret

@no_device:
    mov ah, 0FFh
    jmp @end

@overrun:
    mov ah, 1
    jmp @end

@parity:
    mov ah, 2
    jmp @end

@io_error:
    mov ah, 0FFh
    jmp @end

RECCOM endp

; Query status of COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2, etc.
; On return:  AH = 0
;             CX = free bytes in queue
;             DX = total bytes in queue
; On error:   AH = nonzero error code
;                  1 = buffer overflow
;                  2 = parity error
;                  3, 4, 5 = device timeout
;                  others are mapped to "I/O error"
PUBLIC STACOM
STACOM proc near

    push di

    ; Local device structure in DI
    call comx_get_device
    jc @no_device
    cmp COMx_Device.open[di], 0
    jz @no_device

    ; Receive queue status
    mov dx, comx_rx_qsize
    mov ax, COMx_Device.rx_tail[di]
    sub ax, COMx_Device.rx_head[di]
    jnc @F
        add ax, dx
    @@:
    ; AX is the number of bytes in the queue
    dec dx ; Capacity is rx_qsize - 1
    mov cx, dx
    sub cx, ax

    ; Check for received errors
    xor al, al
    xchg COMx_Device.errors[di], al
    test al, 02h ; overrun
    jnz @overrun
    test al, 04h ; parity
    jnz @parity
    test al, 08h ; framing
    jnz @io_error

    xor ah, ah

@end:
    pop di
    ret

@no_device:
    mov ah, 0FFh
    jmp @end

@overrun:
    mov ah, 1
    jmp @end

@parity:
    mov ah, 2
    jmp @end

@io_error:
    mov ah, 0FFh
    jmp @end

STACOM endp

; Send a byte to a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2, etc.
;             AL = byte to send
; On return:  AH = 0
; On error:   AH = nonzero error code, as for STACOM
PUBLIC SNDCOM
SNDCOM proc near

    push bx
    push cx
    push di
    push es

    ; Local device structure in DI
    call comx_get_device
    jc @no_device
    cmp COMx_Device.open[di], 0
    jz @no_device

    ; BX <- current tail of transmit queue
    mov bx, COMx_Device.tx_tail[di]
    ; CX <- tail after update
    mov cx, bx
    inc cx
    cmp cx, COM_tx_size
    jb @F
        xor cx, cx
    @@:
    ; Is there space in the queue?
    @tx_wait:
        cmp cx, COMx_Device.tx_head[di]
        jne @end_tx_wait
        call CHKKYB
    jmp @tx_wait
    @end_tx_wait:

    ; Place the byte in the queue
    cli
    mov es, COMx_Device.tx_queue[di]
    mov es:[bx], al
    mov COMx_Device.tx_tail[di], cx

    ; Mark the transmitter as active; and if it wasn't, pass the byte to
    ; the hardware
    xor ah, ah
    xchg COMx_Device.tx_active[di], ah
    or ah, ah
    jne @F
        call comx_fill_tx
    @@:
    sti

    ; No error
    xor ah, ah
@end:
    pop es
    pop di
    pop cx
    pop bx
    ret

@no_device:
    mov ah, 0FFh
    jmp @end

SNDCOM endp

; End access to a COMx port
; On entry:   AH = unit number; 0 for COM1, 1 for COM2, etc.
; On return:  AH = 0
; On error:   AH = nonzero error code, as for STACOM
PUBLIC TRMCOM
TRMCOM proc near

    push bx
    push cx
    push dx
    push di
    push es

    ; Local device structure in DI
    call comx_get_device
    jc @no_device
    cmp COMx_Device.open[di], 0
    jz @no_device

    cli
    ; Mark the device as closed
    mov COMx_Device.open[di], 0

    ; Disable interrupts at the device
    mov dx, COMx_Device.io_addr[di]
    inc dx
    xor al, al
    out dx, al
    sti

    ; Free the transmit and the receive queues
    mov es, COMx_Device.tx_queue[di]
    mov ah, 49h
    int 21h
    mov es, COMx_Device.rx_queue[di]
    mov ah, 49h
    int 21h
    xor ax, ax
    mov COMx_Device.tx_queue[di], ax
    mov COMx_Device.rx_queue[di], ax

    ; If this was the last open device on its interrupt line, disable the
    ; interrupt at the controller
    mov cx, NMCOMT
    mov bx, offset comx_devices
    mov dl, COMx_Device.irq[di]
    @irq_check:
        cmp COMx_Device.open[bx], 0
        jz @next_irq_check
        ; Device is open
        cmp COMx_Device.irq[bx], dl
        je @end_irq_check   ; Another device is open on the same IRQ line
    @next_irq_check:
    add bx, COMx_Device
    loop @irq_check
        ; Disable the interrupt if we exit by this path
        mov cl, dl
        mov ah, 01h
        shl ah, cl
        in al, 21h
        or al, ah
        out 21h, al
    @end_irq_check:

    ; Return no error
    xor ah, ah

@end:
    pop es
    pop di
    pop dx
    pop cx
    pop bx
    ret

@no_device:
    mov ah, 0FFh
    jmp @end

TRMCOM endp

; Given the device number in AH, get the address of the element of comx_devices
; in DI
; Return C if device does not exist
comx_get_device proc near private

    cmp ah, NMCOMT
    jae @no_device

    push ax
    mov al, COMx_Device
    mul ah
    add ax, offset comx_devices
    mov di, ax
    pop ax
    clc
    ret

@no_device:
    stc
    ret

comx_get_device endp

; Interrupt handlers for COMx ports
comx_irq3_handler proc near private

    push ax
    mov al, 3
    call comx_irq_handler
    pop ax
    iret

comx_irq3_handler endp
old_int0B dd ?

comx_irq4_handler proc near private

    push ax
    mov al, 4
    call comx_irq_handler
    pop ax
    iret

comx_irq4_handler endp
old_int0C dd ?

comx_irq_handler proc near private

    ; The caller saves AX and passes the IRQ number; this procedure must save
    ; all other registers

    push bx
    push cx
    push dx
    push si
    push di
    push ds
    push es

    mov ds, BASIC_DS

    mov bl, al
    mov cx, NMCOMT
    mov di, offset comx_devices
    @dev_loop:

        cmp COMx_Device.open[di], 0
        jz @next_device     ; Device is not open
        cmp COMx_Device.irq[di], bl
        jne @next_device    ; Device uses another interrupt

        @get_interrupt:

            ; Check the device's interrupt flags
            mov dx, COMx_Device.io_addr[di]
            add dx, 2           ; interrupt identification register
            in al, dx
            shr al, 1
            jc @next_device     ; no interrupt from this device

            ; Got an interrupt
            mov bh, al
            and bh, 03h
            dec bh
            jns @check_1
                ; Modem status interrupt
                ; This shouldn't happen, because it isn't enabled
                mov dx, COMx_Device.io_addr[di]
                add dx, 6
                in al, dx
            jmp @get_interrupt
            @check_1:
            dec bh
            jns @check_2
                ; Tx ready
                call comx_fill_tx
            jmp @get_interrupt
            @check_2:
            dec bh
            jns @check_3
                ; Rx ready
                mov dx, COMx_Device.io_addr[di]
                in al, dx

                mov es, COMx_Device.rx_queue[di]

                ; SI <- current tail of queue
                ; DX <- tail after update
                mov si, COMx_Device.rx_tail[di]
                mov dx, si
                inc dx
                cmp dx, comx_rx_qsize
                jb @F
                    xor dx, dx
                @@:
                ; If the updated tail would equal the head, the queue is full.
                ; Discard the byte and set the overrun flag
                cmp dx, COMx_Device.rx_head[di]
                jne @F
                    or COMx_Device.errors[di], 02h
                    or event_flag, event_rxerror
                    jmp @get_interrupt
                @@:

                ; Add the byte to the queue
                mov es:[si], al
                mov COMx_Device.rx_tail[di], dx
            jmp @get_interrupt
            @check_3:
                ; Line status interrupt
                mov dx, COMx_Device.io_addr[di]
                add dx, 5       ; line status register
                in al, dx
                and al, 1Fh
                or COMx_Device.errors[di], al
                or event_flag, event_rxerror
        jmp @get_interrupt

    @next_device:
    add di, COMx_Device
    dec cx
    jnz @dev_loop

    ; Acknowledge the interrupt
    mov al, 60h
    or al, bl
    out 20h, al

    pop es
    pop ds
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    ret

comx_irq_handler endp

; Fill the transmitter from its queue
; On entry: DI points to the relevant block in comx_devices
;           Interrupts are disabled, whether called from SNDCOM or the
;           interrupt handler
comx_fill_tx proc near private

    push ax
    push bx
    push dx
    push es

    ; BX <- current head of transmit queue
    mov bx, COMx_Device.tx_head[di]
    cmp bx, COMx_Device.tx_tail[di]
    jne @tx_available
        ; Mark the Tx inactive, so SNDCOM will start it again next time
        mov COMx_Device.tx_active[di], 0
    jmp @end
    @tx_available:
        ; Get byte from transmit queue
        mov es, COMx_Device.tx_queue[di]
        mov al, es:[bx]
        ; Remove it from the queue
        inc bx
        cmp bx, COM_tx_size
        jb @F
            xor bx, bx
        @@:
        mov COMx_Device.tx_head[di], bx
        ; Pass it to the device
        mov dx, COMx_Device.io_addr[di]
        out dx, al
        ; Mark the Tx active
        mov COMx_Device.tx_active[di], 1
    @end:

    pop es
    pop dx
    pop bx
    pop ax
    ret

comx_fill_tx endp

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
SNDLPT proc near

    ; Ralf Brown Interrupt List: "Some print spoolers trash the BX register
    ; on return."
    push bx
    push dx

    mov dl, ah  ; Set device number
    xor dh, dh
    xor ah, ah
    int 17h

    ; Convert returned status to error in BASIC
    xor al, al
    test ah, 80h
    je @F
        mov al, 1   ; Device unavailable
        jmp @end
    @@:
    test ah, 20h
    je @F
        mov al, 3   ; Out of paper
        jmp @end
    @@:
    test ah, 08h
    je @F
        mov al, 4   ; I/O error
        jmp @end
    @@:
    test ah, 01h
    je @F
        mov al, 2   ; Timeout
        jmp @end
    @@:

@end:
    mov ah, al
    pop dx
    pop bx
    ret

SNDLPT endp

;-----------------------------------------------------------------------------
; Support for a light pen
;-----------------------------------------------------------------------------

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
RDPEN proc near

    ; TODO: Stub; this gets called if POLLEV returns Z clear
    xor bx, bx
    ret

RDPEN endp

;-----------------------------------------------------------------------------
; Joystick support
;-----------------------------------------------------------------------------

; Read a joystick button
; On entry: AH = 0 to query whether the button is now pressed
;                1 to query whether the button was pressed since the last call
;           AL = index of button to read
; Returns:  AL = 1 if the button is or was pressed, else 0
PUBLIC RDTRIG
RDTRIG proc near

    ; TODO: Stub; joysticks not yet implemented
    xor al, al
    ret

RDTRIG endp

; Read a joystick axis
; On entry: AL = joystick axis to read
; Returns:  C clear if success
;           BX = joystick axis as a signed integer
PUBLIC RDSTIK
RDSTIK proc near

    ; TODO: Stub; joysticks not yet implemented
    stc
    ret

RDSTIK endp

;-----------------------------------------------------------------------------
; Event support
;-----------------------------------------------------------------------------

; Test for trappable events
; Return: Z clear if any event occurred
PUBLIC POLLEV
POLLEV proc near

    push ax
    mov ax, event_flag
    or ax, ax
    pop ax
    ret

POLLEV endp

CSEG ENDS

DSEG segment public 'DATASG'

extrn TOPMEM:word
extrn CSAVEA:word
extrn CSAVEM:byte

; Installed video equipment
disp_installed db 0
disp_vga = 08h      ; VGA features available
disp_ega = 04h      ; EGA features available
disp_cga = 02h      ; CGA features available
disp_mda = 01h      ; MDA or Hercules installed
disp_herc = 10h     ; Hercules installed

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
scroll_from_pixel db ?
scroll_to_pixel db ?
; Used only by Hercules, and sized for that mode
scroll_buf db 91 dup (?)

; Shape of text cursor set by CSRDSP
cursor_shape dw 0B0Dh ; Initially in overwrite mode
; For graphical modes
cursor_state dw ?
cursor_pos dw ?
cursor_mask dw ?

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
page_bitmask dw 0
page_offset dw 0

; Bit-blit parameters
blit_addr dw ?
blit_bits dw ?
blit_bytes dw ?
blit_mixer dw ?

; EGA: Pixel attribute needs to be configured before pixel operation
ega_set_pixel db ?

; EGA: Odd bits at the end of a bit-blitted line
; One extra byte to simplify the packing algorithm a bit
ega_odd_bits db 5 dup (?)
; EGA: Current plane while bit-blitting
ega_blit_plane db ?
; EGA: Bits per plane while bit-blitting
ega_blit_pixels dw ?
; EGA: Bits written to the caller's array
ega_blit_bits dw ?
; EGA: Whole bytes per plane transferred in bit-blit
ega_blit_whole_bytes dw ?

; Event flag
event_flag dw 0
event_ctrlbreak = 0001h
event_rxready   = 0002h
event_rxerror   = 0004h

; Sound support
sound_queue dw 0 ; Segment for queue data
sound_queue_head db 0 ; Head of queue
sound_queue_tail db 0 ; Head of queue
sound_time dw 0  ; Ticks until current sound ends

; COMx device support
; This is the control structure for a single COMx port
COMx_Device struc

open      db ? ; True if device is open
tx_active db ? ; True if we're awaiting a transmit interrupt
tx_queue  dw ? ; Segment for transmit queue
tx_head   dw ? ; Head of transmit queue
tx_tail   dw ? ; Tail of transmit queue
rx_queue  dw ? ; Segment for receive queue
rx_head   dw ? ; Head of receive queue
rx_tail   dw ? ; Tail of receive queue
io_addr   dw ? ; I/O address
irq       db ? ; Interrupt number
flags     db ? ; Flags set on open
errors    db ? ; Errors received

COMx_Device ends

comx_devices db (NMCOMT*COMx_Device) dup (?)

; Configured size of receive queue
comx_rx_qsize dw ?

DSEG ends

END
