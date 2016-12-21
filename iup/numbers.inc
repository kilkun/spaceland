; Write numbers to stdout

; WriteHexa4   : Write a 4 bits hexadecimal number (in AL)
; WriteHexa8   : Write a 8 bits hexadecimal number (in AL)
; WriteHexa16  : Write a 16 bits hexadecimal number (in AX)
; WriteDecimal : Write a 32 bits decimal number (in EAX)

proc WriteHexa4
      push ax
      push dx

      mov ah, 02h              ; write a character
      xchg al, dl
      cmp dl, 10               ; number or letter ?
      jae @@1
      add dl, '0'
      jmp @@2
@@1:  add dl, 'A'-10
@@2:  int 21h

      pop dx
      pop ax
      ret
endp WriteHexa4

proc WriteHexa8
      push ax

      push ax
      shr al, 4                ; 4 higher bits
      call WriteHexa4
      pop ax
      and al, 00fh             ; 4 lower bits
      call WriteHexa4

      pop ax
      ret
endp WriteHexa8


proc WriteHexa16
      push ax

      push ax                  ; get a copy of ax
      xchg al, ah              ; higher byte
      call WriteHexa8
      pop ax
      call WriteHexa8          ; lower byte

      pop ax
      ret
endp WriteHexa16

proc WriteDecimal
      push eax
      push ebx
      push ecx
      push edx

      test eax, 10000000000000000000000000000000b   ; negative
      je @@3
      neg eax          ; reverse the number
      push eax
      mov ah, 02h
      mov dl, '-'      ; print a '-'
      int 21h
      pop eax

@@3:
      mov ebx, 10
      xor cx, cx

@@1:  xor edx, edx
      div ebx        ; (exd:eax) / 10
      push dx        ; the lowest decimal number
      inc cx
      or eax, eax    ; eax=0 ?
      jne @@1

      mov ah, 02h
@@2:  pop dx         ; number in dans dl
      add dl, '0'
      int 21h
      dec cx
      or cx, cx
      jne @@2

      pop edx
      pop ecx
      pop ebx
      pop eax

      ret
endp WriteDecimal
