;ЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫ
; IUP Frank Zago's Intelligent (?) UnPacker v0.6.7 - 1996-
; Intermediate release
;ЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫ

; adjustement parameters

; Minimal length of the jump from which we consider we change of routine
MinSizeJump equ 2000

; choose the level of trace you want (TRACE#)
; a higher level includes the lowers
;   level 0: no traces
;   level 1: a few traces (redirect of interrupts, ...)
;   level 2: a trace per instruction executed
TRACE1 equ

ifdef TRACE2
TRACE1 equ
endif

      .MODEL TINY

IUPSEG segment para

assume cs:IUPSEG, ds:IUPSEG, ss:IUPSEG
      .386
      ideal
      jumps       ; conditionnal jumps automatic adjustment
      locals      ; allows locals symbols (@@...:)
      smart

      org 100h    ; COM file

start:
      jmp IUP     ; jump to the true entry point

include "macro.inc"
include "keyboard.inc"

; *********
; New int 00 handler
; Print an error messsage and exit properly (sometimes :)
; *********
NewInt00:
      ContextSave
      mov bp, sp

      mov ax, cs
      mov ds, ax

      PRINT TxtInt00Error

      jmp EndProg


; *********
; The new int 01
; *********

      ALIGN 16                 ; align cache on 16 bytes boudary
NewInt01:
      ContextSave

      ; turn on/off num lock light keyboard
      dec [InstCount]
      jnz NoJump               ; wait 0
      call Blink               ; switch the light state

NoJump:
      ifdef TRACE2
      mov ax, [SaveCS]
      call WriteHexa16         ; print next CS
      printcar ':'
      mov ax, [SaveIP]
      call WriteHexa16         ; print next IP
      printcar ' '
ifdef 0
      mov ax, SaveSS
      call WriteHexa16         ; print SS
      printcar ':'
      mov ax, SaveSP
      add ax, 6
      call WriteHexa16         ; print true SP
      printcar ' '
      mov ax, SaveDS
      call WriteHexa16         ; print DS
      printcar ':'
      mov ax, SaveSI
      call WriteHexa16         ; print SI
      printcar ' '
      mov ax, SaveES
      call WriteHexa16         ; print ES
      printcar ':'
      mov ax, SaveDI
      call WriteHexa16         ; print DI
      printcar ' '
endif
      mov es, [saveCS]
      mov si, [SaveIP]
      mov ax, [es:si]
      call WriteHexa16         ; print next instruction opcode
      printcar 0dh
      printcar 0ah             ; carriage return
      endif ;TRACE2

      ; was the previous instruction a jump ?
      cmp [IsJump], 0
      jne EndOfJump            ; yes

      ; load the current instruction
      mov es, [SaveCS]
      mov si, [SaveIP]
      mov ax, [es:si]          ; instruction in AX (AL then AH)

      ; no prefix yet
      mov [NbPrefix], 0

SearchOpcode:

      ; Now, ax is the instruction opcode
      ; we search for the good instruction

      ; jump to the correct opcode
      ; if I remember, it can take less intructions with a 586+
      mov bx, ax
      and bx, 0ffh             ; get al in bx
      shl bx, 1                ; al*2
      jmp [jtOpcode+bx]

ResumeProgram:
      ; resume program
      ContextLoad
      iret

TreatJump:
      ; that flags means this instruction is a jump
      not [IsJump]             ; become 0ffffh

      ; remember current adress
      movzx eax, [SaveCS]
      movzx ebx, [SaveIP]
      shl eax, 4
      add eax, ebx
      mov [AddrStartJmp], eax  ; EAX = adress from where the jump starts

      ; resume program
      jmp ResumeProgram

EndOfJump:
      not [IsJump]             ; become 0, for next jump

      ; It might be the end of unpacking procedure
      ; there is 4 different jumps :
      ;   - an internal jump -> do nothing
      ;   - a forward jump   -> usually, it's the copy of the unpacker routine
      ;   - a rear jump      -> usually, it's the beggining of the true code

      ; compute the difference in bytes between the previous and the
      ; new adress
      movzx eax, [SaveCS]
      movzx ebx, [SaveIP]
      shl eax, 4
      add eax, ebx             ; EAX = current adress
      mov ebx, [AddrStartJmp]  ; EBX = previous adress (before the jump)

      ; COND #1
      ; Is it a rear jump ?
      cmp eax, ebx
      jb @@1                   ; yes

      ; so it is forward jump
      sub eax, ebx
      xchg eax, ecx

      ; COND #2
      ; Is it an internal jump to the routine (less than MinSizeJump bytes) ?
      cmp ecx, MinSizeJump
      jb NoJump                ; no, we do nothing
      jmp NotEndUnpack

@@1:
      ; so it is a rear jump

      ; COND #3
      ; the file might be a .COM
      mov cx, [SaveIP]
      cmp cx, 0100h            ; minimal condition for a .COM
      je @@2

      ; distance for the jump
      sub ebx, eax
      cmp ebx, MinSizeJump    ; more than MinSizeJump bytes ?
      jb NoJump               ; no

@@2:
      ; COND 3 : DS = ES
      mov ax, [SaveDS]
      cmp ax, [SaveES]
      jne NotEndUnpack

      ; COND #4 : DS = PSP
      cmp ax, [ParPSP]
      jne NotEndUnpack

      ; Here, we are sure that the unpacking routine is done
      ; We don't know yet if it is a .COM or a .EXE
      ; we save the file

      ; has there been some relocations ?
      cmp [EntNouvFichier.NbReloc], 0
      jne FichierEXE

      ; start in offset 100h
      cmp [SaveIP], 0100h
      jne FichierEXE

      ; start with CS = PSP ?
      mov ax, [SaveCS]
      cmp ax, [ParPSP]
      jne FichierEXE

      ; start with SS = PSP ?
      cmp ax, [SaveSS]
      jne FichierEXE

      ; adjust stack
      ; and test if SP = 0fffch or 0fffeh
      mov ax, [SaveSP]
      add ax, 6
      and ax, 0fff0h           ; SP should be in [fff0..ffff]
      cmp ax, 0fff0h
      jb FichierEXE

FichierCOM:
      ; It is a .COM file
      ; If size is less than the original file size,
      ; it means that the code was a scrambler
      ; by default we set the new size to the old size
      movzx eax, [ParCode]
      mov ebx, [FileSize]
      shl eax, 4
      add eax, ebx
      dec eax                  ; minimal size before uncompress

      cmp [AdrSup], eax
      jnb @@1
      mov [AdrSup], eax        ; fix a new file size

@@1:
      PRINT Stage4Txt

      PRINT DebutCOMTxt
      PRINT EnregistreCodeTxt

      call CreateFile          ; .COM is not created yet
      call WriteBody           ; write code body
      call CloseFile

      jmp EndProg

FichierEXE:
      ; It is an EXE file

      PRINT Stage4Txt

      PRINT DebutEXETxt
      PRINT EnregistreCodeTxt

      ; if the file doesn't exist yet (EXE without relocation),
      ; we create it
      cmp [EntNouvFichier.NbReloc], 0    ; number or relocation
      jne @@12
      call CreateFile          ; create the file
      call WriteHeader         ; write a fake header

@@12:
      ; compute the header size, and align it on a paragraph
      mov ax, [EntNouvFichier.NbReloc]
      shl ax, 2                ; AX:=AX*4, size of relocations
      add ax, 32               ; header size + relocation size
      mov bx, ax
      xor cx, cx
      push bx
      and ax, 1111b
      jz @@15                  ; jump if already aligned
      mov cx, 10h
      sub cx, ax               ; length in CX
      mov ax, 4000h
      mov bx, [NumHandle]
      lea dx, [ChaineVide]
      int 21h                  ; write

@@15: pop bx
      add bx, cx               ; bx is number divisible by 16

      shr bx, 4                ; number of paragraph
      mov [EntNouvFichier.TailleHeader], bx

      call WriteBody           ; write the code body

      ; correct and write the header
      mov ax, [ParCode]

      mov bx, [SaveSS]
      sub bx, ax
      mov [EntNouvFichier.RegSS], bx

      mov bx, [SaveSP]
      add bx, 6                ; adjust stack
      mov [EntNouvFichier.RegSP], bx

      mov bx, [SaveIP]
      mov [EntNouvFichier.RegIP], bx

      mov bx, [SaveCS]
      sub bx, ax
      mov [EntNouvFichier.RegCS], bx

      mov [EntNouvFichier.AdrRelocEXE], 32

      ; compute size of file (MOD and DIV 512)
      movzx eax, [EntNouvFichier.TailleHeader]
      shl eax, 4
      add eax, [TailleCodeEcrit]
      mov ebx, eax
      shr eax, 9               ; eax/512
      and ebx, 111111111b
      mov [EntNouvFichier.LongFicMOD], bx
      cmp bx, 0
      je @@19
      inc eax                  ; there is 1 more 512 byte page
@@19:
      mov [EntNouvFichier.LongFicDIV], ax

      ; compute min required paragraphs
      ; it is an approximate value, and a lot of packers doesn't give
      ; a correct min size.
      ; it is (packed code size/16)+(old min value)-(unpacked code size/16)
      mov eax, [CodeSize]
      shr eax, 4
      movzx ebx, [EntAncFichier.ParMin]
      add eax, ebx
      mov ebx, [TailleCodeEcrit]
      shr ebx, 4
      sub eax, ebx
      jc @@20                  ; ebx > eax
      mov [EntNouvFichier.ParMin], ax
      jmp @@21
@@20: mov [EntNouvFichier.PArMin], 0       ; rare case, but it happened
@@21:
      ; compute max required paragraphs
      movzx ebx, [EntAncFichier.ParMax]
      cmp ebx, 0ffffh
      je @@17                  ; 0ffffh by default
      mov eax, [CodeSize]
      shr eax, 4               ; eax/16
      add eax, ebx
      mov ebx, [TailleCodeEcrit]
      shr ebx, 4
      sub eax, ebx
      mov [EntNouvFichier.ParMax], ax
@@17:
      call WriteHeader         ; write good header now
      call WriteOverlay
      call CloseFile           ; close the file

EndProg:
      ; stop the program by placing
      ; mov ax, 4C00h followed by int 21h
      cld
      mov es, [SaveCS]
      mov di, [SaveIP]
      mov ax, 00B8h            ; mov ax,4C00h
      stosw
      mov al, 04Ch
      stosb
      mov ax, 21CDh            ; int 21h
      stosw

      ; set trap flag to 0
      mov es, [SaveSS]
      mov di, [SaveSP]
      and [word es:di+4], 1111111011111111b

      ; if problem happened, erase the temporary file
      cmp [FichierCree], 0
      jnz @@21

      lea dx, [TempFile]
      mov ax, 4100h
      int 21h
@@21:
      jmp ResumeProgram

NotEndUnpack:
      ; it was a long jump (rear or forward), so we reset some datas
      movzx eax, [SaveCS]
      movzx ebx, [SaveIP]
      shl eax, 4
      add eax, ebx             ; EAX = current adress
      mov [RoutBegin], eax

      xor eax, eax
      mov [AdrSup], eax        ; greater adress found
      mov [EntNouvFichier.NbReloc], ax   ; no longer relocations

      jmp NoJump

; skip a byte
Skip1Byte:
      mov es, [SaveSS]
      mov di, [SaveSP]
      inc [word es:di]         ; next IP
      inc [SaveIP]
      jmp NoJump

; skip 2 bytes
Skip2Bytes:
      mov es, [SaveSS]
      mov di, [SaveSP]
      add [word es:di], 2      ; next IP
      add [SaveIP], 2
      jmp NoJump

; Memory writes with the following instructions : STOS[BW], MOVS[BW]
; The output format is :
;    fs = destination segment
;    bx = destination offset
;    cx = instruction size (include prefix DS:, ES:, ...)
;    ax = the data to store
;    dx = number of bytes to store

; Memory write with STOSW
EcrMem_STOSW:
      cmp [SaveES], 0
      jne ResumeProgram

      mov bx, [SaveDI]
      mov es, [SaveSS]
      mov di, [SaveSP]
      mov ax, [es:di+6]        ; get flags
      and ax, 0000010000000000b            ; direction flag
      or ax, ax                ; is it set ?
      jne @@1
      add [SaveDI], 2
      jmp @@2
@@1:  sub [SaveDI], 2

@@2:  mov fs, [SaveES]
      mov ax, [SaveAX]
      mov cx, [NbPrefix]
      inc cx
      mov dx, 2

      jmp EcrMemRoutage

; Memory write with STOSB
EcrMem_STOSB:
      mov fs, [SaveES]
      mov bx, [SaveDI]
      mov ax, [SaveAX]
      mov cx, [NbPrefix]
      inc cx
      mov dx, 1
      jmp EcrMemRoutage

; Memory write with MOVSW
EcrMem_MOVSW:
      mov ax, [SaveES]
      or ax, ax                ; only if write in 0:xxxx
      jne ResumeProgram

      mov fs, ax
      mov bx, [SaveDI]
      mov es, [SaveDS]
      mov di, [SaveSI]
      mov ax, [es:di]
      mov cx, [NbPrefix]
      inc cx
      mov dx, 2

      jmp EcrMemRoutage

; Memory write with MOVSB
EcrMem_MOVSB:
      mov fs, [SaveES]
      mov bx, [SaveDI]
      mov es, [SaveDS]
      mov di, [SaveSI]
      mov ax, [es:di]
      mov cx, [NbPrefix]
      inc cx
      mov dx, 1
      jmp EcrMemRoutage

; Memory write for instructions that have a Mod R/M
EcrMem_MOV_AX:
      ; search the prefix (the segment adress)
      ; and put it in FS
      cmp [NbPrefix], 0
      jne @@1                  ; if no prefix,
      mov [Prefix], 3eh        ; it is DS: by default

@@1:
      mov bx, [Prefix]
      and bx, 00011000b        ; get prefix register number
      shr bx, 2                ; and computes its entry
      mov dx, [word RegSeg+bx] ; dx contains its value

      or dx,dx                 ; dx = 0 ?
      jne ResumeProgram
      mov fs, dx

      mov cx, [NbPrefix]
      add cx,3                 ; instruction size
      inc si
      mov ax, [SaveAX]         ; value to store
      mov bx, [es:si]
      mov dx, 2

      jmp AntiHack

EcrMem_MOV_16:
      ; search the prefix (the segment adress)
      ; and put it in FS
      cmp [NbPrefix], 0
      jne @@1                  ; if no prefix,
      mov [Prefix], 3eh        ; it is DS: by default

@@1:
      mov bx, [Prefix]
      and bx, 00011000b        ; get prefix register number
      shr bx, 2                ; and computes its entry
      mov dx, [word RegSeg+bx] ; dx contains its value

      or dx,dx                 ; dx = 0 ?
      jne ResumeProgram
      mov fs, dx

      mov cx, [NbPrefix]
      add cx, 6
      inc si                   ; skip instruction opcode
      inc si                   ; and the Mod R/M
      mov bx, [es:si]
      inc si
      inc si
      mov ax, [es:si]
      mov dx,2
      jmp AntiHack

EcrMem_MODRM:
      ; search the prefix (the segment adress)
      ; and put it in FS
      cmp [NbPrefix], 0
      jne @@1                  ; if no prefix,
      mov [Prefix], 3eh        ; it is DS: by default

@@1:
      mov bx, [Prefix]
      and bx, 00011000b        ; get prefix register number
      shr bx, 2                ; and computes its entry
      mov dx, [word RegSeg+bx] ; dx contains its value

      or dx,dx                 ; dx = 0 ?
      jne ResumeProgram
      mov fs, dx

      inc si
      mov ax, [es:si]          ; save the Mod R/M
      push ax
      call AnalyseModRM
      cmp ax, 0ffffh
      je ResumeProgram         ; not a memory adress

      mov cx, [NbPrefix]
      add cx, [SizeModRM]
      inc cx                   ; instruction size

      pop bx                   ; the mod/rm
      and bx, 0000000000111000b          ; mask for register number
      shr bx, 2
      mov ax, [word RegStd+bx] ; value to put
      mov bx, [RelocOfs]       ; offset adress
      mov dx, 2                ; 2 bytes destination
      jmp AntiHack

EcrMem_MOV_8:
      ; search the prefix (the segment adress)
      ; and put it in FS
      cmp [NbPrefix], 0
      jne @@1                  ; if no prefix,
      mov [Prefix], 3eh        ; it is DS: by default

@@1:
      mov bx, [Prefix]
      and bx, 00011000b        ; get prefix register number
      shr bx, 2                ; and computes its entry
      mov dx, [word RegSeg+bx] ; dx contains its value

      or dx,dx                 ; dx = 0 ?
      jne ResumeProgram
      mov fs, dx

      mov cx, [NbPrefix]
      add cx, 5

      inc si                   ; skip instruction
      inc si                   ; and the Mod R/M
      mov bx, [es:si]
      inc si
      inc si
      mov al, [es:si]
      mov dx,1
      jmp AntiHack

EcrMem_MOV_SREG:
      ; search the prefix (the segment adress)
      ; and put it in FS
      cmp [NbPrefix], 0
      jne @@1                  ; if no prefix,
      mov [Prefix], 3eh        ; it is DS: by default

@@1:
      mov bx, [Prefix]
      and bx, 00011000b        ; get prefix register number
      shr bx, 2                ; and computes its entry
      mov dx, [word RegSeg+bx] ; dx contains its value

      or dx,dx                 ; dx = 0 ?
      jne ResumeProgram
      mov fs, dx

      mov cx, [NbPrefix]
      add cx, 4

      inc si                   ; skip instruction
      mov bx, [es:si]          ; Mod R/M
      and bx, 00011000b
      shr bx, 2
      mov ax, [word RegSeg+bx] ; value to put
      inc si                   ; skip the Mod R/M
      mov bx, [es:si]
      jmp AntiHack       ; useless, for now

EcrMemRoutage:
      ; is an interrupt catch, or a write of the program's code ?
      push ax
      mov ax, fs
      or ax, ax
      jne EcrMemNormal         ; aie, where is pop ax ?
      pop ax
      jmp AntiHack

EcrMemNormal:
      ; compute the write adress
      ; and test if it is beyond of the highest adress computed

      mov eax, fs         ; eax is the paragraph number
      and ebx, 0ffffh
      shl eax, 4          ; eax*16
      add eax, ebx        ; eax is now the write adress

      ; compute current adress
      movzx ebx, [SaveCS]
      movzx ecx, [SaveIP]
      shl ebx, 4
      add ebx, ecx        ; ebx is now the current adress

      ; if destination adress is beyond to current adress, do nothing
      cmp eax, ebx
      ja @@1

      ; if destination adress is below the highest adress, do nothing
      cmp eax, [AdrSup]
      jb @@1

      ; so, the destination adress is beyond
      mov [AdrSup], eax       ; paragraph of the superior adress

@@1:
      jmp ResumeProgram

AntiHack:
      ; is it a catch for int 0 to 3 ?
      ; bx contains the offset. For those interrupts, it should be in [0..15]
      cmp bx, 15
      ja ResumeProgram

      ; get the value
      cmp dx, 1                ; destination size
      jne @@1
      mov [byte FakeInt+bx], al
      jmp @@2
@@1:
      mov [word FakeInt+bx], ax
@@2:
      ; we must skip that instruction which will crash
      ; the tracer and may be the computer
      add [SaveIP], cx         ; IP point now to the next instruction
      mov ax, [SaveIP]

      mov es, [SaveSS]
      mov di, [SaveSP]
      mov [es:di], ax          ; return IP on stack is correct

      ifdef TRACE1
      PRINT TxtTrapInt
      shr bx, 2                ; interrupt number
      mov ax, bx
      call WriteHexa8

      PRINT CarReturn
      endif ;TRACE1

      jmp NoJump               ; analysis of the next instruction

ProcessIntCD:
      cmp ah, 4
      jae ResumeProgram        ; superior or equal
      mov cx, 2                ; 2 bytes instruction
      jmp ProcessInt

ProcessIntCC:
      mov cx, 1                ; 1 byte instruction
      mov ah, 03h              ; int 03h
;     jmp ProcessInt

ProcessInt:
      ; simulate an int 0 to 3
      xchg ah, al              ; interrupt number in al

      ifdef TRACE2
      push ax
      PRINT TxtCallInt
      pop ax
      push ax
      call WriteHexa8         ; write interrupt number
      PRINT CarReturn
      pop ax
      endif ;TRACE2

      ; shift the current stack for return to the program
      ; we build the stack as if the interrupt has been executed
      mov es, [SaveSS]
      mov di, [SaveSP]
      sub di, 6                ; decrease SP by 6 (size of SS, SP and flags)
      mov [SaveSP], di

      xor bx, bx
      mov bl, al               ; int number in bx
      shl bx, 2

      ; copy the 6 bytes (S, SP and flags) so IUP can return to
      ; the program traced
      mov eax, [dword FakeInt+bx]        ; SS and SP
      mov [es:di], eax
      mov ax, [es:di+0ah]      ; flags
      mov [es:di+4], ax

      ; copy on the stack the parameters (SS, SP and flags) so the program
      ; will believe it is in an interrupt handler, and can return
      ; correctly to its normal code.
      mov ax, [SaveIP]
      add ax, cx               ; size of CC or CD xx
      mov [es:di+6], ax        ; IP
      mov ax, [SaveCS]
      mov [es:di+8], ax        ; CS

      ; update CS and IP. Flags is unchanged.
      mov ax, [word FakeInt+bx]       ; IP
      mov [SaveIP], ax
      mov ax, [word FakeInt+bx+2]     ; CS
      mov [SaveCS], ax

      jmp NoJump

RepPrefix:
      ; we have a prefix for strings instruction
      ; REPE (f2h) or REPZ (f3h)
      inc si
      inc [NbPrefix]           ; one prefix found
      mov ax, [es:si]          ; read next instruction

      jmp SearchOpcode

SegPrefix:
      ; we have a segment prefix
      ; ES: (26h), CS: (2eh), SS: (36h) or DS: (3eh)
      xor ah, ah
      mov [Prefix], ax         ; store prefix for later use
			       ; only segment prefix are stored
      inc si
      inc [NbPrefix]           ; one prefix more
      mov ax, [es:si]          ; read next instruction

      jmp SearchOpcode


Treat_POPF:
      ; always set the TRAP flag
      mov es, [SaveSS]
      mov di, [SaveSP]
      or [word es:di+6], 0100h
      jmp ResumeProgram

Treat_PUSHF:
      ; always reset the TRAP flag
      ; will be a hole if it is used to prepare an IRET

      ; move down the stack for 2 bytes
      mov es, [SaveSS]
      mov di, [SaveSP]
      mov ax, [es:di]
      mov [es:di-2], ax        ; copy IP
      mov ax, [es:di+2]
      mov [es:di], ax          ; copy CS
      mov ax, [es:di+4]
      mov [es:di+2], ax        ; copy Flags
      and [word es:di+4], 0feffh         ; reset bit T
      sub [SaveSP], 2
      jmp Skip1Byte

TraiteReloc:
      ; It can be a relocation, or an anti-debugging trap.
      inc si                   ; si now points on the modR/M
      call AnalyseModRM

      cmp ax, 0ffffh
      je ResumeProgram         ; not a memory adress

      ; computes the memory adress
      movzx eax, [RelocSeg]
      movzx ebx, [RelocOfs]
      shl eax, 4
      add eax, ebx             ; eax is the target adress

      cmp eax, [AdrSup]
      ja ResumeProgram         ; after the unpacked program
                               ; there's a hole here ! What might the program
                               ; do ?

      cmp eax, [RoutBegin]
      ja ResumeProgram

CalculeReloc:
      ; is it the first relocation ?
      cmp [EntNouvFichier.NbReloc], 0
      jne @@11

      ; yes, so we create the file
      PRINT TxtEndUnpacking
      PRINT Stage3Txt

      ; it IS an exe file
      call CreateFile

      ; write a fake header
      call WriteHeader

@@11:
      ; another relocation
      inc [EntNouvFichier.NbReloc]

      ; compute the relocation
      mov ax, [RelocSeg]       ; memory relocation segment
      sub ax, [ParCode]
      mov [RelocSeg], ax       ; relocation segment stored in file

      ; write the relocation
      mov ax, 04000h
      mov bx, [NumHandle]
      mov cx, 0004h            ; 4 bytes (segment and offset)
      lea dx, [RelocOfs]
      int 21h

      ; we know the relocation, and we dont want to modify the code
      ; because we haven't saved it yet. So we skip the instruction.
      mov es, [SaveSS]
      mov di, [SaveSP]
      mov ax, [SizeModRM]      ; modR/M 's size
      add ax, [NbPrefix]       ; number of prefix
      inc ax                   ; instruction code size
      add [word es:di], ax     ; inc IP with total instruction's size
      jmp ResumeProgram

FakeIntNoWhere:
      ; used for programs that call int 01 and 03 while assuming
      ; they are not used
      iret

RetourProg:
      ; When the program terminates, it returns here.
      mov ax, cs
      mov ds, ax

      cmp [FichierCree], 0
      jne @@19                 ; no decompression code was found

      ; delete temporary file
      mov ax, 4100h
      lea dx, [TempFile]
      int 21h

      ERROR TxtErrorInProg, 1

@@19:
      ; restore old vector 0
      mov ax, 2500h
      lds dx, [cs:int_00h_entry_old]
      int 21h

      ; restore old vector 1
      mov ax, 2501h
      lds dx, [cs:int_01h_entry_old]
      int 21h

      ; restore old vector 6
      mov ax, 2506h
      lds dx, [cs:int_06h_entry_old]
      int 21h

      ; restore environment
      mov ax, cs
      mov ds, ax
      mov es, ax

      ; erase old file
      mov ax, 4100h
      lea dx, [ProgName]
      int 21h
      jnc DeleteOK

      ERROR TxtCantDelete, 1

DeleteOK:
      mov ax, ds
      mov es, ax
      mov ax, 5600h
      lea dx, [TempFile]
      lea di, [ProgName]
      int 21h

      jnc RenameOK

      ERROR TxtCantRename, 1

RenameOK:

      EXIT 00

;ЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫ

include "numbers.inc"
include "modrm.inc"
include "files.inc"
include "ng_misha.inc"         ; no-generic code handler (misha's routines)
include "jmptable.inc"


; *********
; Initialization of IUP
; *********
assume cs:IUPSEG  , ds:IUPSEG , ss:IUPSEG

IUP:
      PRINT DebutProgTxt

      lea sp, [TSREnd]         ; new stack pointer

      ; read parameters
      mov cl, [ds:80h]         ; parameters on command line's length $$$
      xor ch, ch
      test cx, cx              ; empty command line ?
      jz NoOptions

      call ParseOptions        ; parameters analysis

      cmp [ProgName], 0
      jz NoOptions

      ; analyse the entry file
      PRINT Stage1Txt
      call AnalyseFichier

      or ax, ax
      jne @@1

      ERROR TxtCantProceed, 1

@@1:  call CreateTempFile

      ; free unused memory to be able to load another program
      ; some variables and procedures are no longer available
      call FreeMem

      call ReadExeFile
      call ChangeTermination

      ; compute the starting code paragraph
      mov ax, [ParPSP]         ; PSP
      add ax, 10h              ; start of the code
      mov [ParCode], ax

      PRINT Stage2Txt

      ; initialise FakeInt array
      mov ax, cs
      mov [FakeInt+2], ax
      mov [FakeInt+6], ax
      mov [FakeInt+10], ax
      mov [FakeInt+14], ax

      ; catch INT 0
      mov ax, 3500h
      int 21h
      mov [word int_00h_entry_old], bx
      mov [word int_00h_entry_old+2], es
      mov ax, 2500h
      lea dx, [NewInt00]
      int 21h

      ; catch INT 1
      mov ax, 3501h
      int 21h
      mov [word int_01h_entry_old], bx
      mov [word int_01h_entry_old+2], es
      mov ax, 2501h
      lea dx, [NewInt01]
      int 21h

      ; catch INT 6
      mov ax, 3506h
      int 21h
      mov [word int_06h_entry_old], bx
      mov [word int_06h_entry_old+2], es
      mov ax, 2506h
      lea dx, [NewInt00]       ; the same as int 00
      int 21h

      ; initialize some datas
      movzx eax, [ParCode]
      shl eax, 4
      add eax, [CodeSize]
      mov [AdrSup], eax

      ; get son's segment
      push cs
      pop es
      lea bx, [param_block]    ; ptr on parameter block

      ; set the registers for the program
      mov cx, [word es:bx+0Eh]           ; SP
      mov ax, [word es:bx+10h]           ; SS
      mov ss, ax
      mov sp, cx

      ; to lauch the son, we use an iret instruction. So we must first push
      ; the 3 parameters (CS, IP and flags) on the stack.

      ; put flags on stack, with flag TRAP set
      pushf
      pop dx
      or dx, 0000000100000000b
      push dx

      movzx ecx, [word es:bx+12h]    ; IP in ax
      movzx eax, [word es:bx+14h]    ; CS in cx
      push ax
      push cx
      shl eax, 4
      add eax, ecx
      mov [RoutBegin], eax

      ; to run the program, some registers must have some specifc values.
      ; note that this particularity is used in IUP to know when the
      ; decompression code ends.
      mov ax, [ParPSP]
      mov ds, ax
      mov es, ax

      xor ax, ax
      xor bx, bx
      xor cx, cx
      xor dx, dx
      xor di, di
      xor si, si

      iret                     ; launch the son program

NoOptions:
      PRINT UsageTxt
      EXIT 1

; ***
; Datas and internal stack
; ***
      ALIGN 4

; old interrupt vectors
int_entry equ int_00h_entry_old
int_00h_entry_old  dd ?        ; ancien point d'entr‚e de l'int 00h
int_01h_entry_old  dd ?        ; ancien point d'entr‚e de l'int 01h
int_06h_entry_old  dd ?        ; ancien point d'entr‚e de l'int 06h

; some programs call int 01 and 03 without redirect them.
; so we initialise their offset correctly, and the segment value
; will be set at initialisation.
; by default, they point to an IRET
FakeInt dw (4*2) dup(FakeIntNowhere, ?)

; Infos on original file
FileSize dd 0                  ; total size
HeaderSize dd 0                ; size of the header
CodeSize dd 0                  ; size of the code
OverlaySize dd 0               ; size of the overlay

; infos on new file
AdrSup dd 0                    ; superior adress used
TailleCodeEcrit dd 0           ; sizeof the written code

      ALIGN 2
; save registers when an interrupt happen
; _DON'T_ change order. Order _is_ very important
SaveES dw ?
SaveCS dw ?                    ; return CS
SaveSS dw ?
SaveDS dw ?
RegSeg equ SaveES              ; pointer on segment registers

SaveAX dw ?
SaveCX dw ?
SaveDX dw ?
SaveBX dw ?
SaveSP dw ?
SaveBP dw ?
SaveSI dw ?
SaveDI dw ?
RegStd equ SaveAX              ; pointer on standard registers


SaveIP dw ?                    ; return IP
SaveFG dw ?                    ; return flags

; datas used to know size of program in memory
ParPSP dw 0                    ; PSP paragraph of son
ParCode dw 0                   ; paragraph where the code starts
TempFile db 100 dup (0)        ; new filename

      ALIGN 2
RelocOfs dw 0               ; DO NOT split ....
RelocSeg dw 0               ; ... them
NumHandle dw 0              ; file handle
OldHAndle dw 0              ; handle of old file
NbreParEcrits dw 0          ; number of paragraph in code
FichierCree dw 0            ; 1 if destination file is already created

NbPrefix dw 0               ; number of prefixes found
Prefix dw 0                 ; code of the last prefix
SizeModRM dw 0              ; number of bytes used by the modR/M
TailleInstruction dw 0      ; numbers of bytes used by current instruction


; EXE header
struc StructEnteteFichier
  Identificateur dw "ZM"
  LongFicMod dw 0         ; file length MOD 512
  LongFicDiv dw 0         ; file length DIV 512
  NbReloc dw 0            ; relocations numbers
  TailleHeader dw 0       ; hedear size in paragraphs
  ParMin dw 0             ; paragraphs minimal number
  ParMax dw 0ffffh        ; paragraph maximal number
  RegSS dw 0              ; initial SS
  RegSP dw 0              ; initial SP
  Checksum dw 0           ; EXE header checksum
  RegIP dw 0              ; initial IP
  RegCS dw 0              ; initial CS
  AdrRelocEXE dw 0
  NumOvl dw 0
  Sig db "IUP*"           ; a signature (might be another, just to align)
ends StructEnteteFichier

EntNouvFichier StructEnteteFichier <>     ; header copy of the original file
EntAncFichier StructEnteteFichier <>      ; header of the target file

; error messages
TxtErrorInProg db "Couldn't find decompression code",0dh, 0ah, "$"
TxtInt00Error db "An int 00h occured, program halted",0dh, 0ah, "$"
TxtCantRename db "Can not rename temporary file", 0dh, 0ah, "$"
TxtCantDelete db "Can not delete old file", 0dh, 0ah, "$"
TxtTrapInt db "IUP: Attempt to trap int $"
CarReturn db 0dh, 0ah, "$"
TxtCallInt db "Call of int $"


; datas used for jumps
IsJump dw 0
AddrStartJmp dd 0              ; jump begin adress
RoutBegin dd 0                 ; adress in bytes where the routine starts

; miscs
ChaineVide db 16 dup(0)        ; empty string
InstCount dw 0                 ; instructions numbers since the last
                               ; time the light state changed

; datas used to load the program in memory
param_block dw 24h dup(0)      ; block parameters to read the source file
ProgName db 256 dup (0)        ; program source name

; options line for program to load
OptionLine db 100 dup(0dh)    ; only line return


Stage2Txt db 0dh, 0ah, "Stage 2 : Unpacking",0dh, 0ah
	  db "-  - --========-- -  -",0dh, 0ah, "$"
TxtEndUnpacking db "  Terminated",0dh, 0ah, "$"

Stage3Txt db 0dh, 0ah, "Stage 3 : Relocations", 0dh, 0ah
	  db "-  - --=======-- -  -",0dh, 0ah, "$"

Stage4Txt db 0dh, 0ah, "Stage 4 : Adjustements", 0dh, 0ah
	  db "-  - --=======-- -  -",0dh, 0ah, "$"
DebutEXETxt db "  Creating an EXE file",0dh, 0ah, "$"
DebutCOMTxt db "  Creating a COM file",0dh, 0ah, "$"
RelocSSSPTxt db "    SS:SP       = $"
RelocCSIPTxt db "    CS:IP       = $"
NbreReloc db    "    Relocations = $"
EnregistreCodeTxt db "  Writting program body",0dh, 0ah, "$"

MyStack db 256 dup(0)          ; 256 bytes should be enough
TSREnd db 0                    ; end of memory occupied by the TSR

testmisha db 0
;ЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫЫ

include "options.inc"
include "fileanal.inc"

; error messages
ErrorPasParamTxt   db "no parameters",0dh, 0ah, "$"
TxtInternalError   db "internal error",0dh, 0ah, "$"
TxtInvalidFormat   db "invalid exe/com format",0dh, 0ah, "$"
TxtCantProceed     db "can't unpack",0dh, 0ah, "$"
TxtLoadError       db "can't load file",0dh, 0ah, "$"
TxtCantCreate      db "can't create temporary file", 0d, 0ah, "$"

; miscs messages
DebutProgTxt db "IUP v0.6.7 Intelligent (?) EXE/COM UnPacker",0dh, 0ah
	     db "By Frank Zago, copyleft 1996",0dh, 0ah, 0dh, 0ah,"$"

UsageTxt db "usage: IUP <source file name>",0dh, 0ah, "$"

Stage1Txt db "Stage 1: source file analysis",0dh, 0ah
	  db "-  - --===============-- -  -",0dh, 0ah, "$"
TxtOldFileStruct db "  Structure : $"
FileSizeTxt      db "  Size      : $"
TxtCOMFileStruct db "COM",0dh, 0ah, "$"
TxtEXEFileStruct db "EXE ", "$"
StructureDonneesTxt db "Datas",0dh, 0ah, 0dh, 0ah, "$"
TailleHeaderTxt db  "Header = $"
TailleCodeTxt db    " , Code = $"
TailleOverlayTxt db " , Overlay = $"


ends IUPSEG

       end start
