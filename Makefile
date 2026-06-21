LIBDIR := lib/github.com/pzel/jsonCvt
MLCOMP ?= polymlb
MLB_PATH := -mlb-path-var "SMLPKG $(shell pwd)/lib"

ifeq ($(MLCOMP), polymlb)
MLCOMP_FLAGS=-ann "ignoreFiles call-main.sml"
endif

.PHONY: all
all:	 test

.PHONY: clean
clean:
	-@rm -f bin/*

.PHONY: test
test:
	@$(MLCOMP) $(MLCOMP_FLAGS) $(MLB_PATH) -output ./bin/test $(LIBDIR)/test/test.mlb
	./bin/test
