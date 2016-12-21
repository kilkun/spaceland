; Terminate with a return code
macro EXIT ReturnCode
      mov AX, 4Ch*256+ReturnCode
      int 21h
      endm

; Write a string on stdout with int 21h
; Warning: - DS must be correct
;          - AX and DX may change
macro PRINT String
      mov ax, 0900h
      lea dx, [String]
      int 21h
      endm

; Write a character on stdout with int 21h
; Warning: - AX and DX may change
macro PRINTCAR Character
      mov ax, 0200h
      mov dl, Character
      int 21h
      endm

; Save the traced program's context
macro ContextSave
      mov [cs:SaveSS], ss
      mov [cs:SaveSP], sp
      mov [cs:SaveBP], bp

      mov [cs:SaveDS], ds
      mov [cs:SaveAX], ax
      mov ax, cs
      mov ds, ax               ; DS = CS

      mov bp, sp
;      mov ax, ss:[bp+4]      ; flags (they are already on stack)
;      mov cs:SaveFG, ax
      mov ax, [ss:bp+2]       ; CS
      mov [SaveCS], ax
      mov ax, [ss:bp]         ; IP
      mov [SaveIP], ax

      mov ax, cs
      mov ss, ax
      lea sp, [TSREnd]

      mov [SaveBX], bx
      mov [SaveCX], cx
      mov [SaveDX], dx
      mov [SaveES], es
      mov [SaveDI], di
      mov [SaveSI], si

      endm

; Restore traced program's context
macro ContextLoad
      mov ax, [cs:SaveAX]
      mov bx, [cs:SaveBX]
      mov cx, [cs:SaveCX]
      mov dx, [cs:SaveDX]
      mov es, [cs:SaveES]
      mov di, [cs:SaveDI]
      mov si, [cs:SaveSI]

      mov ss, [cs:SaveSS]
      mov sp, [cs:SaveSP]
      mov bp, [cs:SaveBP]

      mov ds, [cs:SaveDS]
      endm

; Print an error text and exit
macro ERROR Chaine, ErrorCode
      PRINT TxtError
      PRINT Chaine
      EXIT ErrorCode
      endm

TxtError db 0dh, 0ah, "IUP: $"
