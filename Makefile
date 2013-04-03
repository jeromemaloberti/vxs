PREFIX?=/usr/local

.DEFAULT: all
all: dist/setup
	obuild build

dist/setup:
	obuild configure

clean:
	obuild clean

install: all
	ocamlfind install xsops lib/META dist/build/lib-xsops/*.cma dist/build/lib-xsops/*.cmi dist/build/lib-xsops/*.cmxa dist/build/lib-xsops/*.a
	cp dist/build/vxs_fe/vxs_fe $(PREFIX)/bin/

uninstall:
	ocamlfind remove xsops
	rm $(PREFIX)/bin/vxs_fe
