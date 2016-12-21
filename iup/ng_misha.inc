; IUP
; code not generic

; ===================
; Misha unpacker #1 (option -mish1)
; ===================
; Misha's routine use int 01 to uncrypt the program's code.
;
; install_mish1 initialize the no-generic code.
; It install a trap for instruction (CF-AD)-E2-FD (??? / loop $-1)
; This trap is executed several time (once by crypted byte).
; Unfortunatly, we can't remove that installed trap.

proc install_mish1
      push ax
      push bx
      lea bx, [jtOpcode+2*0e2h]
      mov ax, [bx]             ; get old vector
      mov [old_e2], ax
      lea ax, [mish1_1]
      mov [bx], ax
      pop bx
      pop ax
      ret
endp install_mish1

proc mish1_1
      ; verify that code is E2-FD and CF-AD before (should be long enough)
      cmp ax, 0fde2h
      jne @@3
      mov ax, [es:si-2]        ; bug if SI is 0, but should not
      cmp ax, 0adcfh
      mov ax, 0fde2h
      jne @@3

@@1:  ; do the thing that int 01 should have done
      mov ax, [SaveAX]
      add [SaveBX], ax
      mov ax, [SaveBX]         ; BX:=BX+AX
      mov [SaveAX], ax         ; AX:=BX

@@3:  jmp [old_e2]

endp mish1_1

old_e2 dw 0                    ; old entry in opcode jump table for 0e2h
old_e9 dw 0                    ; old entry in opcode jump table for 0e2h

; ===================
; End of Misha unpacker #1
; ===================


; ===================
; Misha unpacker #1 (option -mish1)
; ===================
; Misha's routine use int 01 to uncrypt the program's code.
;
; install_mish1 initialize the no-generic code.
; It install a trap for instruction (CF-AD)-E2-FD (??? / loop $-1)
; This trap is executed several time (once by crypted byte).
; Unfortunatly, we can't remove that installed trap.
; There a second trap against the first routine: it jumps to 100h and
; then jumps to a second routine.

proc install_mish2
      push ax
      push bx
      lea bx, [jtOpcode+2*0e2h]
      mov ax, [bx]             ; get old vector
      mov [old_e2], ax
      lea ax, [mish2_1]
      mov [bx], ax             ; first trap
      lea bx, [jtOpcode+2*0e9h]
      mov ax, [bx]             ; get old vector
      mov [old_e9], ax
      lea ax, [mish2_2]
      mov [bx], ax             ; first trap

      pop bx
      pop ax
      ret
endp install_mish2

proc mish2_1
      ; verify that code is E2-FD and CF-AD before (should be long enough)
      cmp ax, 0fde2h
      jne @@3
      mov ax, [es:si-2]        ; bug if SI is 0, but should not
      cmp ax, 0adcfh
      mov ax, 0fde2h
      jne @@3

@@1:  ; do the thing that int 01 should have done
      mov ax, [SaveAX]
      sub [SaveBX], ax
      mov ax, [SaveBX]         ; BX:=BX+AX
      mov [SaveAX], ax         ; AX:=BX

@@3:  jmp [old_e2]

endp mish2_1

; search for a jmp 100h
proc mish2_2
      mov bx, [es:si+1]        ; offset to jump
      add bx, [SaveIP]
      add bx, 3                ; size of jmp #
      cmp bx, 100h
      je @@1
      jmp [old_e9]
@@1:
      ; so it is the jmp 100h
      ; remove both handlers
      lea bx, [jtOpcode+2*0e2h]
      mov ax, [old_e2]
      mov [bx], ax

      lea bx, [jtOpcode+2*0e9h]
      mov ax, [old_e9]
      mov [bx], ax

      ; ripost is just to ignore that it is a jump
      jmp ResumeProgram

endp mish2_2
