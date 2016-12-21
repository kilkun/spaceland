; File analysis
; *********
; Analysis of the entry file
; Return=0 on error (an error message is printed)
;        1 if a COM file
;        2 if an EXE file
; *********

proc AnalyseFichier
      ; open the file
      mov ax, 3d00h            ; open in read only
      lea dx, [ProgName]       ; parameters offset adress
      int 21h
      jnc @@2

      ERROR TxtLoadError, 1    ; can't load file

@@2:  mov bx, ax               ; handle

      ; get file's size
      mov ax, 4202h
      xor cx, cx
      xor dx, dx
      int 21h
      jnc @@3

      ERROR TxtInternalError, 1

@@3:  xchg eax, edx
      shl eax, 16
      xchg ax, dx
      mov [FileSize], eax

      ; read the first 28 bytes of the file
      ; (the header if an exe file)
      mov ax, 4200h
      xor cx, cx
      xor dx, dx
      int 21h                  ; position 0

      mov ax, 3f00h
      mov cx, 28
      lea dx, [EntAncFichier.Identificateur]
      int 21h

      ; close the file
      mov cx, ax               ; store the number of bytes read
      mov ax, 3e00h
      int 21h

      PRINT FileSizeTxt
      mov eax, [FileSize]
      call WriteDecimal
      PRINT CarReturn

      PRINT TxtOldFileStruct   ; "Structure : "

      ; search for an exe identificator
      mov ax, [EntAncFichier.Identificateur]
      cmp ax, 'ZM'       ; MZ
      je short AncEXE
      cmp ax, 'MZ'       ; ZM
      je short AncEXE

      ; it's not an exe, so we check for file size
      mov eax, [FileSize]
      cmp eax, 0ff00h
      ja short AncDonnees      ; file is over ff00h bytes

      PRINT TxtCOMFileStruct
      mov ax, 1                ; COM file
      ret

AncDonnees:
      ; so it is some datas
      PRINT StructureDonneesTxt
      xor ax, ax               ; not an executable
      ret

AncEXE:
      ; so it is an exe file
      PRINT TxtEXEFileStruct

      mov eax, [FileSize]
      cmp eax, 28              ; header size
      jae short @@1

      ; size is not good, so it's a data file with an exe file signature
      PRINT CarReturn
      PRINT TxtInvalidFormat
      xor ax, ax
      ret

@@1:  ; write infos on standard output

      PRINTCAR '('
      PRINT TailleHeaderTxt
      movzx eax, [EntAncFichier.TailleHeader]
      shl eax, 4               ; eax=eax*16
      mov ebx, eax
      mov [HeaderSize], eax
      call WriteDecimal

      PRINT TailleCodeTxt
      movzx eax, [EntAncFichier.LongFicDiv]
      shl eax, 9               ; EAX=EAX*512
      movzx ecx, [EntAncFichier.LongFicMod]
      cmp ecx, 0
      je @@4
      add eax, ecx
      sub eax, 512
@@4:
      mov ecx, eax             ; store HeaderSize+CodeSize
      sub eax, ebx

      mov [CodeSize], eax
      call WriteDecimal

      PRINT TailleOverlayTxt
      mov eax, [FileSize]
      sub eax, ecx
      mov [OverlaySize], eax
      call WriteDecimal

      PRINTCAR ')'
      PRINT CarReturn

      mov ax, 2                ; EXE file

      ret

endp AnalyseFichier
