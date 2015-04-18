###############################################################################
PCB_CMD         = $(basename $(PCB)).cmd
PCB_SCHS        = $(addprefix $(PCB_DIR),$(notdir $(SCHS)))

GSCHEM2PCB_DIR  = $(GEDATOOLS_DIR)gschem2pcb/
LIBGEDARUBY_DIR = $(GEDATOOLS_DIR)libgedaruby/bin/src/.libs/

GSCH2PCB        = gsch2pcb
GSCH2PCB_FLAGS  = $(foreach dir,$(FP_DIRS),-d $(dir))

GSCHLAS         = gschlas

COND            = ruby -I $(LIBGEDARUBY_DIR) $(GSCHEM2PCB_DIR)cond/cond.rb
COND_FLAGS      = $(foreach def,$(COND_DEFS),-D $(def))

EXPR            = ruby -I $(LIBGEDARUBY_DIR) $(GSCHEM2PCB_DIR)expr/expr.rb
EXPR_FLAGS      =

DEVMAP          = ruby -I $(LIBGEDARUBY_DIR) $(GSCHEM2PCB_DIR)devmap/devmap.rb
DEVMAP_FLAGS    = $(foreach dir,$(DEV_DIRS),-P $(dir))

PINMAP          = ruby -I $(LIBGEDARUBY_DIR) $(GSCHEM2PCB_DIR)pinmap/pinmap.rb
PINMAP_FLAGS    =

MKGERBER        = $(GEDATOOLS_DIR)mkgerber/mkgerber.sh

###############################################################################
.SECONDARY:

.DELETE_ON_ERROR:

.PHONY: all
all: pcb gerber bom

.PHONY: clean
clean:
	cd $(PCB_DIR) && rm -f *.gsch2pcb *.new.pcb *.cmd* *.net *~ *~tmp*

.PHONY: distclean
distclean: clean
	cd $(PCB_DIR) && rm -f *.pcb.bak* *.pcb- *.sch

###############################################################################
.PHONY: pcb
pcb: $(PCB_CMD)

$(PCB_CMD): $(PCB_SCHS)

%.cmd: %.pcb
	$(GSCH2PCB) $(GSCH2PCB_FLAGS) -o $(basename $@) $(filter %.sch,$^)
	@mv $@ $@~tmp~
	@( \
	    echo "LoadFrom(Layout, $<)"; \
	    if [ -e $(basename $@).new.pcb ]; then \
	        echo "PasteBuffer(1)"; \
	        echo "PasteBuffer(Clear)"; \
	        echo "Select(All)"; \
	        echo "PasteBuffer(AddSelected)"; \
	        echo "RemoveSelected()"; \
	        echo "PasteBuffer(2)"; \
	        echo "PasteBuffer(Clear)"; \
	        echo "LoadFrom(LayoutToBuffer, $(basename $@).new.pcb)"; \
	        echo "PasteBuffer(ToLayout, 0, 0, mm)"; \
	        echo "Select(All)"; \
	        echo "DisperseElements(Selected)"; \
	        echo "PasteBuffer(1)"; \
	        echo "PasteBuffer(ToLayout, +0, +0, mm)"; \
	        echo "Unselect(All)"; \
	    fi; \
	    echo "LoadFrom(Netlist, $(basename $@).net)"; \
	    cat $@~tmp~; \
	    echo "SaveTo(Layout)"; \
	    echo "Quit"; \
	) > $@
	pcb --action-script $@
	@touch $@

$(PCB_DIR)%.sch: $(SCH_DIR)%.sch
	@cp $< $@
	gschlas -e $@
	@mv $@ $@~tmp~
	< $@~tmp~ \
	      $(COND) $(COND_FLAGS) \
	    | $(EXPR) $(EXPR_FLAGS) \
	    | $(DEVMAP) $(DEVMAP_FLAGS) \
	    | $(PINMAP) $(PINMAP_FLAGS) \
	    > $@

.PHONY: tag-pcb
tag-pcb: $(PCB)
	sed -i "s/git: [0-9a-f]\{16\}/git: `git rev-parse HEAD | head -c16`/" $<

.PHONY: untag-pcb
untag-pcb: $(PCB)
	sed -i "s/git: [0-9a-f]\{16\}/git: 0000000000000000/" $<


###############################################################################
.PHONY: gerber
gerber: elecrow oshpark

.PHONY: elecrow
elecrow: $(ELECROW)

.PHONY: oshpark
oshpark: $(OSHPARK)

$(ELECROW): $(PCB) $(PCB_CMD)
	@mkdir -p $(dir $@)
	$(MKGERBER) -f elecrow -o $(basename $@) $<

$(OSHPARK): $(PCB) $(PCB_CMD)
	@mkdir -p $(dir $@)
	$(MKGERBER) -f oshpark -o $(basename $@) $<

###############################################################################
.PHONY: bom
bom: $(BOM)

$(BOM): $(SCHS)
	@mkdir -p $(dir $@)
	gnetlist -g bom2 -o - $(filter %.sch,$^) | sed 's/\(^\|:\)unknown\>/\1/g' > $@
