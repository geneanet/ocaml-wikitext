
.PHONY: all test doc install uninstall clean example

build:
	dune build wktxt_cmdline.exe

test: build
	_build/default/wktxt_cmdline.exe < test.wikitext

clean:
	dune clean
