      ALIGN 8
; Here is the jump table, indexed by opcode
jtOpcode dw ResumeProgram      ; 00h
      dw TraiteReloc           ; 01 = add sreg:[xx], xx (used for relocations)
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 10h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 20h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw SegPrefix             ; 26h = ES: prefix
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw SegPrefix             ; 2eh = CS: prefix
      dw ResumeProgram         ;
      dw ResumeProgram         ; 30h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw SegPrefix             ; 36h = SS: prefix
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw SegPrefix             ; 3Eh = DS: prefix
      dw ResumeProgram         ;
      dw ResumeProgram         ; 40h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 50h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 60h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 70h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 80h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw EcrMem_MODRM          ; 87h = xchg sreg:[xx], xx
      dw ResumeProgram         ;
      dw EcrMem_MODRM          ; 89h = mov sreg:[reg+xx], xx
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw EcrMem_MOV_SREG       ; 8ch = mov sreg:[xx], sreg
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; 90h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw treat_PUSHF           ; 9ch = PUSHF
      dw treat_POPF            ; 9dh = POPF
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; a0h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw EcrMem_MOV_AX         ; a3h = mov sreg:[xx], ax
      dw EcrMem_MOVSB          ; a4h = MOVSB
      dw EcrMem_MOVSW          ; a5h = MOVSW
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw EcrMem_STOSB          ; aah = STOSB
      dw EcrMem_STOSW          ; abh = STOSW
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; b0h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; c0h
      dw ResumeProgram         ;
      dw TreatJump             ; c2h = RET ####
      dw TreatJump             ; c3h = RET
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw EcrMem_MOV_8          ; c6h = mov sreg:[xx], 8
      dw EcrMem_MOV_16         ; c7h = mov sreg:[xx], 16
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw TreatJump             ; cah = RETF ####
      dw TreatJump             ; cbh = RETF
      dw ProcessIntCC          ; cch = INT 03 (in one byte)
      dw ProcessIntCD          ; cdh = INT ##
      dw ResumeProgram         ;
      dw ResumeProgram         ; cfh = IRET
      dw ResumeProgram         ; d0h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ; e0h
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw Skip2bytes            ; e4h = in al, imm8
      dw Skip2bytes            ; e5h = in ax, imm8
      dw Skip2Bytes            ; e6h = out imm8, al
      dw Skip2Bytes            ; e7h = out imm8, ax
      dw ResumeProgram         ;
      dw TreatJump             ; e9h = JMP ####
      dw TreatJump             ; eah = JMP FAR ####:####
      dw ResumeProgram         ;
      dw Skip1byte             ; ech = in al, dx
      dw Skip1byte             ; ech = in ax, dx
      dw Skip1Byte             ; eeh = out dx, al
      dw Skip1Byte             ; efh = out dx, ax
      dw ResumeProgram         ; f0h
      dw ResumeProgram         ;
      dw RepPrefix             ; f2h = REPE
      dw RepPrefix             ; f3h = REPZ
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw ResumeProgram         ;
      dw TreatJump             ; 0ffh = JMP FAR reg
