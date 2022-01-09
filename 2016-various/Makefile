DAYS = $(wildcard */Makefile)

all: build_all run_all

build_all:
	@$(foreach day, $(DAYS), $(MAKE) -C $(dir $(day)) build;)

run_all:
	@$(foreach day, $(DAYS), $(MAKE) -C $(dir $(day)) run;)

clean_all:
	@$(foreach day, $(DAYS), $(MAKE) -C $(dir $(day)) clean;)
