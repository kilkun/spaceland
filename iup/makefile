# Makefile for IUP
# Compile with Borland's tasm

AFLAGS  = -m -w2

SOURCE = iup.asm fileanal.inc numbers.inc keyboard.inc \
         options.inc jmptable.inc macro.inc modrm.inc \
         files.inc ng_misha.inc

EXENAME = iup.com
PACKNAME = iup067

$(EXENAME): iup.obj
         tlink /x /t iup.obj

iup.obj: $(SOURCE)

clean:
        del *.obj
        del *.bak
        del $(EXENAME)
        del $(PACKNAME).zip

pack: $(SOURCE) $(EXENAME) makefile iup.doc
        zip $(PACKNAME) $(SOURCE)
        zip $(PACKNAME) makefile iup.doc $(EXENAME)
