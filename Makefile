JWASM_PATH = c:\devel\jwasm

OFILES = &
gwinit.obj &
advgrp.obj &
biboot.obj &
bimisc.obj &
biprtu.obj &
biptrg.obj &
bistrs.obj &
call86.obj &
dskcom.obj &
fiveo.obj &
gengrp.obj &
gio86.obj &
giocas.obj &
giocom.obj &
giocon.obj &
giodsk.obj &
giokyb.obj &
giolpt.obj &
gioscn.obj &
giotbl.obj &
gwdata.obj &
gweval.obj &
gwlist.obj &
gwmain.obj &
gwram.obj &
gwsts.obj &
ibmres.obj &
itsa86.obj &
kanj86.obj &
maclng.obj &
math.obj &
next86.obj &
scndrv.obj &
scnedt.obj &
stubs.obj

EXE = gw-basic.exe

JWASM = $(JWASM_PATH)\jwasmd.exe

$(EXE) : $(OFILES)
	@echo Linking...
	@echo system dos >objs.txt
	@for %x in ($(OFILES)) do echo file %x >>objs.txt
	@echo option map >>objs.txt
	@echo name $(EXE) >>objs.txt
	@wlink @objs.txt

advgrp.obj : advgrp.asm
	$(JWASM) advgrp.asm

biboot.obj : biboot.asm
	$(JWASM) biboot.asm

bimisc.obj : bimisc.asm
	$(JWASM) bimisc.asm

biprtu.obj : biprtu.asm
	$(JWASM) biprtu.asm

biptrg.obj : biptrg.asm
	$(JWASM) biptrg.asm

bistrs.obj : bistrs.asm
	$(JWASM) bistrs.asm

call86.obj : call86.asm
	$(JWASM) call86.asm

dskcom.obj : dskcom.asm
	$(JWASM) dskcom.asm

fiveo.obj : fiveo.asm
	$(JWASM) fiveo.asm

gengrp.obj : gengrp.asm
	$(JWASM) gengrp.asm

gio86.obj : gio86.asm
	$(JWASM) gio86.asm

giocas.obj : giocas.asm
	$(JWASM) giocas.asm

giocom.obj : giocom.asm
	$(JWASM) giocom.asm

giocon.obj : giocon.asm
	$(JWASM) giocon.asm

giodsk.obj : giodsk.asm
	$(JWASM) giodsk.asm

giokyb.obj : giokyb.asm
	$(JWASM) giokyb.asm

giolpt.obj : giolpt.asm
	$(JWASM) giolpt.asm

gioscn.obj : gioscn.asm
	$(JWASM) gioscn.asm

giotbl.obj : giotbl.asm
	$(JWASM) giotbl.asm

gwdata.obj : gwdata.asm
	$(JWASM) gwdata.asm

gweval.obj : gweval.asm
	$(JWASM) gweval.asm

gwinit.obj : gwinit.asm
	$(JWASM) gwinit.asm

gwlist.obj : gwlist.asm
	$(JWASM) gwlist.asm

gwmain.obj : gwmain.asm
	$(JWASM) gwmain.asm

gwram.obj : gwram.asm
	$(JWASM) gwram.asm

gwsts.obj : gwsts.asm
	$(JWASM) gwsts.asm

ibmres.obj : ibmres.asm
	$(JWASM) ibmres.asm

itsa86.obj : itsa86.asm
	$(JWASM) itsa86.asm

kanj86.obj : kanj86.asm
	$(JWASM) kanj86.asm

maclng.obj : maclng.asm
	$(JWASM) maclng.asm

math.obj : math.asm
	$(JWASM) math.asm
math.asm : math1.asm
	@type math1.asm math2.asm >math.asm

next86.obj : next86.asm
	$(JWASM) next86.asm

scndrv.obj : scndrv.asm
	$(JWASM) scndrv.asm

scnedt.obj : scnedt.asm
	$(JWASM) scnedt.asm

stubs.obj : stubs.asm
	$(JWASM) stubs.asm

clean: .SYMBOLIC
	-del *.obj
	-del $(EXE)
	-del *.map
	-del objs.txt
	-del math.asm
