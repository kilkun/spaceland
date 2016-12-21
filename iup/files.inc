; File management

; Write an executable (COM or EXE) file without execute it
; File name is in ProgName
; Give also the parameters
proc ReadExeFile
      mov ax, 4B01h            ; load without execute
      lea dx, [ProgName]       ; file name
      lea bx, [param_block]    ; parameters block ptr
      mov [word es:bx], 0      ; segment's son environment
      mov [word es:bx+2], offset OptionLine        ; adr ofs paramˆtres
      mov [es:bx+4], ds                            ; adr seg paramˆtres
      mov [word es:bx+6], offset ChaineVide        ; adr ofs FCB1
      mov [es:bx+8], ds                            ; adr seg FCB1
      mov [word es:bx+0ah], offset ChaineVide      ; adr ofs FCB2
      mov [es:bx+0ch], ds                          ; adr seg FCB2
      int 21h
      jnc ReadOk
      ERROR TxtLoadError, 0
ReadOk:
      ret
endp ReadExeFile

; Change termination address
proc ChangeTermination
      mov ax, 6200h
      int 21h                  ; get son's PSP -> BX
      mov [ParPSP], bx         ; save it for later
      mov es, bx
      push cs
      pop ax                   ; current segment
      mov [es:0ch], ax         ; PSP int 22h seg.
      lea ax, [RetourProg]
      mov [es:0ah], ax         ; PSP int 22h ofs.
      ret
endp ChangeTermination


; Free unused memory
proc FreeMem
      mov ax, 04A00h
      mov bx, offset TSREnd    ; compute the number of paragraphs
      shr bx, 4                ; IUP really needs
      inc bx
      int 21h
      ret
endp FreeMem

; Create the new file
proc CreateFile
      mov ax, 3C00h
      xor cx, cx
      lea dx, [TempFile]
      int 21h
      mov [NumHandle], ax
      ret
endp CreateFile

; Write the body code
proc WriteBody
      mov bx, [NumHandle]
      movzx edx, [ParCode]
      mov ecx, [AdrSup]
      shl edx, 4               ; transforms paragraph in bytes
      sub ecx, edx             ; PSP adress
      inc ecx
      mov [TailleCodeEcrit], ecx

      push ds

      mov dx, [ParCode]
      mov ds, dx               ; DS:DX is the begining of the
      xor dx, dx               ; program in memory

      ; write 64KB blocs
@@1:
      cmp ecx, 0fff0h
      jbe short @@10           ; if less than 64KB to write
      sub ecx, 0fff0h          ; paragraphs to write
      push ecx
      mov cx, 0fff0h
      mov ax, 4000h            ; write in file
      int 21h
      mov dx, ds               ; inc DS by the
      add dx, 0fffh            ; number of paragraphs
      mov ds, dx               ; written
      xor dx, dx
      pop ecx

      jmp @@1

      ; write the residue
@@10:
      mov ax, 4000h
      int 21h

      pop ds

      ret
endp WriteBody

proc WriteHeader
      ; seek at the begining of the file
      mov ax, 4200h
      mov bx, [NumHandle]
      xor cx, cx
      xor dx, dx
      int 21h

      ; write the header
      mov ax, 04000h
      mov bx, [NumHandle]
      mov cx, 32
      lea dx, [EntNouvFichier]
      int 21h
      ret
endp WriteHeader

; Copy the overlay table
; We use the 512 bytes jump table as buffer. It avoids to allocate some
; buffers elsewhere.
proc WriteOverlay
      ; go to the end of the new file
      mov ax, 4202h
      mov bx, [NumHandle]      ; BX = new file handle
      xor cx, cx
      xor dx, dx
      int 21h

      ; open the old file
      mov ax, 3d00h
      lea dx, [ProgName]
      int 21h                  ; should be correct
      mov [OldHandle], ax

      ; go to the begining of the overlay in the old file
      mov eax, [HeaderSize]
      add eax, [CodeSize]

      mov dx, ax
      shr eax, 16
      mov cx, ax
      mov ax, 4200h
      mov bx, [OldHandle]
      int 21h

      lea dx, [JtOpcode]

@@1:
      mov ax, 3f00h
      mov bx, [OldHandle]
      mov cx, 512              ;  our buffer size
      int 21h

      cmp ax, 0
      je @@2

      mov cx, ax               ; copy only the size read
      mov ax, 4000h
      mov bx, [NumHandle]
      int 21h

      jmp @@1

@@2:
      ; close old file
      mov ax, 3e00h
      mov bx, [OldHandle]
      int 21h

      ret
endp WriteOverlay

proc CloseFile
      mov ax, 3E00h
      mov bx, [NumHandle]
      int 21h

      not [FichierCree]        ; new value should be 0ffffh

      ret
endp CloseFile

proc CreateTempFile
      mov ax, 5a00h
      xor cx, cx
      lea dx, [TempFile]
      int 21h
      jnc CreateOK

      ERROR TxtCantCreate, 1
CreateOK:
      ret
endp CreateTempFile
