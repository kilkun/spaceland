;*******
; array of options on command line
;*******
struc OptEntry
  OptLen dw 0                  ; length of option
  OptName db 5 dup (0)         ; name of option (length can be increased)
  OptHandler dw 0              ; handler to call
ends OptEntry

OptArray OptEntry <5, "mish1", install_mish1>
         OptEntry <5, "mish2", install_mish2>
         OptEntry <0, 0, 0>             ; no more options

; *******
; Analysis of command line parameters
; Program name is in NomProg
; and its parameters are in ParamsProg
; *******
proc ParseOptions
      push bx
      push si
      push di

      cld

      mov si, 81h              ; ds:si points to the commmand line

      ; search for options
@@1:  lodsb
      cmp al, ' '
      je @@1
      cmp al, 09h
      je @@1
      cmp al, '-'
      jne @@4
      call AnOption
      jmp @@1
@@4:  cmp al, '/'
      jne @@5
      call AnOption
      jmp @@1
@@5:  cmp al, 0dh
      je EndOptions

      ; get the file name
      lea di, [ProgName]
@@2:  stosb                    ; store
      lodsb
      cmp al, 0dh
      je EndOptions
      cmp al, ' '
      je @@3
      cmp al, 09h
      jne @@2

      ; get options for program to load
      lea di, [OptionLine]
@@3:  stosb
      lodsb
      cmp al, 0dh
      jne @@3

EndOptions:
      pop di
      pop si
      pop bx
      ret

endp ParseOptions

; search for the option and call the handler
proc AnOption
      lea bx, [OptArray]
      push ds
      pop es
      cld

@@1:  mov cx, [(OptEntry bx).OptLen]
      or cx, cx
      je UnknowOpt

      push si
      lea di, [(OptEntry bx).OptName]
      repe cmpsb
      je FoundOPt

      pop si
      add bx, size OptEntry
      jmp @@1

FoundOpt:
      pop ax                   ; restore stack (si was pushed)
      mov di, [(OptEntry bx).OptHandler]
      call di
      ret

UnknowOpt:
      ERROR TxtInvalidOption, 1          ; no option yet
endp AnOption

TxtInvalidOption db "Invalid option$"

