; Blink NumLock
proc Blink
      xor ax,ax
      mov es,ax                          ; ES=0
      test [byte es:417h], 20h           ; 0:417h, or 40h:17h
      jnz short LightIsOn                ; jump if light is on

      ; light is off
      mov al, 0edh
      out 60h, al

      mov cx, 0b000h
@@1:
      loop @@1                 ; stupid timing :(

      mov al, 2
      out 60h, al
      or [byte es:417h], 20h   ; light is now on
      jmp @@3

LightIsOn:
      ; light is on
      mov al, 0edh
      out 60h, al

      mov cx, 0b000h
@@2:
      loop @@2                 ; again, stupid timing :((

      mov al, 0
      out 60h, al
      and [byte es:417h], 0DFh ; light is now off

@@3:
      ret
endp Blink