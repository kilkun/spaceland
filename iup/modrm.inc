; ********
; Analyse of the modRM byte
; Entry:  ES:SI points to the modRM
;         NbPrefix contains the number of prefixes
;         Prefix is the opcaode of the last prefix
;
; Return: RelocSeg:RelocOfs contains the destination adress
;         SizeModRM contient le nombre d'octets pris par le modR/M

proc AnalyseModRM
      ; verify that the destination operand is not a register
      mov al, [es:si]          ; modR/M
      and al, 11000000b        ; mod only
      cmp al, 11000000b
      je @@DestIsReg

      mov [SizeModRM], 1

      ; search the prefix (eg the segment) and put it in fs
      cmp [NbPrefix], 0
      jne @@3                  ; if no prefix, it is DS by default
      mov [Prefix], 3eh        ; value of prefix DS:

@@3:
      mov bx, [Prefix]
      and bx, 00011000b        ; get the number of the register (ES:,CS:,...)
      shr bl, 2                ; segment register number * 2
      mov dx, [word RegSeg+bx] ; dx is now the value of the register

      ; save the value found
      mov [RelocSeg], dx

      ; what registers are implied:
      ;  [BX+SI], [BX+DI], [BP+SI], [BP+DI], [BX], [SI], [BP], [DI] or [dep16]
      ; $$$: without prefix, the segment register by default of BP is SS,
      ;      not DS (not handled)
      mov al, [es:si]          ; modR/M in al
      mov bl, al               ; bh is already 0
      xor dx, dx

      and bx, 11000111b
      cmp bx, 00000110b        ; dep 16 only
      je @@dep16

      and bx, 111b             ; get R/M only

      ; $$$ not efficient. Could be optimized
      cmp bx, 0
      je short @@RelocBXSI
      cmp bx, 1
      je  short @@RelocBXDI
      cmp bx, 2
      je  short @@RelocBPSI
      cmp bx, 3
      je  short @@RelocBPDI
      cmp bx, 4
      je  short @@RelocSI
      cmp bx, 5
      je  short @@RelocDI
      cmp bx, 6
      je  short @@RelocBP

@@RelocBX:                     ; BX=7
      add dx, [SaveBX]
      jmp @@1
@@RelocBXSI:
      mov dx, [SaveBX]
      add dx, [SaveSI]
      jmp @@1
@@RelocBXDI:
      mov dx, [SaveBX]
      add dx, [SaveDI]
      jmp @@1
@@RelocBPSI:
      mov dx, [SaveBP]
      add dx, [SaveSI]
      jmp @@1
@@RelocBPDI:
      mov dx, [SaveBP]
      add dx, [SaveDI]
      jmp @@1
@@RelocSI:
      mov dx, [SaveSI]
      jmp @@1
@@RelocDI:
      mov dx, [SaveDI]
      jmp @@1
@@RelocBP:
      mov dx, [SaveBP]
;      jmp @@1

@@1:
      ; seek for an offset, and its size (8 or 16 bits)
      ; al countains always the modR/M
      and al, 11000000b        ; get the mod only
      cmp al, 01000000b
      je short @@dep8
      cmp al, 10000000b
      je short @@dep16
      jmp short @@fin

@@dep8:
      inc si
      inc [SizeModRM]
      mov al, [es:si]          ; load 8 bits offset value
      xor ah, ah
      add dx, ax
      jmp short @@fin

@@dep16:
      inc si
      add [SizeModRM], 2
      mov ax, [es:si]          ; load 16 bits offset value
      add dx, ax

@@fin:
      mov [RelocOfs], dx
      xor ax, ax               ; all is well
      ret

@@DestIsReg:
      mov ax, 0ffffh           ; not an interresting prefix
      ret

endp AnalyseModRM
