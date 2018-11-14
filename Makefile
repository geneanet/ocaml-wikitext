
.PHONY: all test doc install uninstall clean example retest

build:
	dune build wktxt_cmdline.exe

test: build
	echo "\ntest wikitext :" ; _build/default/wktxt_cmdline.exe < test/test.wikitext; echo "\ntest bold and italic :\n" ; _build/default/wktxt_cmdline.exe < test/test.boldital | head -n 100;


clean:
	dune clean

retest: clean test
