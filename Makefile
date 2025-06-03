JWASM_PATH = c:\devel\jwasm

OFILES = &
gwdata.obj &
gwmain.obj &
oem.obj &
gweval.obj &
gwlist.obj &
ibmres.obj &
bimisc.obj &
dskcom.obj &
biptrg.obj &
biprtu.obj &
bistrs.obj &
fiveo.obj &
gengrp.obj &
advgrp.obj &
maclng.obj &
gwsts.obj &
gio86.obj &
giodsk.obj &
giokyb.obj &
gioscn.obj &
giolpt.obj &
giocom.obj &
giocon.obj &
giotbl.obj &
scnedt.obj &
scndrv.obj &
call86.obj &
next86.obj &
math.obj &
kanj86.obj &
giocas.obj &
itsa86.obj &
gwram.obj &
gwinit.obj &
biboot.obj

EXE = gwbasic.exe

JWASM = $(JWASM_PATH)\jwasmd.exe

$(EXE) : $(OFILES)
	@echo Linking...
	@echo system dos >objs.txt
	@echo option packdata=0 >>objs.txt
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
math.asm: math1.asm math2.asm
	@type math1.asm math2.asm >math.asm

next86.obj : next86.asm
	$(JWASM) next86.asm

scndrv.obj : scndrv.asm
	$(JWASM) scndrv.asm

scnedt.obj : scnedt.asm
	$(JWASM) scnedt.asm

oem.obj : oem.asm
	$(JWASM) oem.asm

clean: .SYMBOLIC
	-del *.obj
	-del $(EXE)
	-del *.map
	-del objs.txt
	-del math.asm
