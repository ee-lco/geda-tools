###############################################################################
PCB            ?= $(PCB_DIR)$(PROJECT).pcb
SCHS           ?= $(SCH_DIR)$(PROJECT).sch
BOM            ?= $(BOM_DIR)$(PROJECT).csv
ELECROW        ?= $(GERBER_DIR)$(PROJECT)-elecrow/$(PROJECT).zip
OSHPARK        ?= $(GERBER_DIR)$(PROJECT)-oshpark/$(PROJECT).zip

PCB_DIR        ?= ./pcb/
SCH_DIR        ?= ./sch/
GERBER_DIR     ?= ./gerber/
BOM_DIR        ?= ./bom/

SYM_DIRS       ?= $(LIB_DIR)sym/
DEV_DIRS       ?= $(LIB_DIR)dev/
FP_DIRS        ?= $(LIB_DIR)fp/
