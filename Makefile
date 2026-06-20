LIBDIR := lib/github.com/pzel/jsonCvt
MLB_PATH := -mlb-path-var 'SMLPKG $(shell pwd)/lib'

.PHONY: all
all:	 clean test

.PHONY: clean
clean:
	rm -f bin/* tmp/*

.PHONY: test
test: bin/runTests $(shell find $(LIBDIR) | grep *.sql)
	./bin/runTests

bin/runTests: $(shell find $(LIBDIR))
	@polymlb $(MLB_PATH) \
	-output bin/runTests \
	$(LIBDIR)/test/runTests.mlb

