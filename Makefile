
.PHONY: all test doc install uninstall clean example retest

build:
	dune build wktxt_cmdline.exe

test: build
	_build/default/wktxt_cmdline.exe < test/test.wikitext | head -n 100

clean:
	dune clean

retest: clean test
