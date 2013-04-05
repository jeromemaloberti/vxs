PREFIX?=/usr/local

.DEFAULT: all
all: dist/setup lib/template.ml
	obuild build

dist/setup:
	obuild configure

lib/template.ml: lib/template.mlify
	cd lib && ocamlify template.mlify --output template.ml

clean:
	obuild clean

install: all
	ocamlfind install xsops lib/META dist/build/lib-xsops/*.cma dist/build/lib-xsops/*.cmi dist/build/lib-xsops/*.cmxa dist/build/lib-xsops/*.a
	cp dist/build/vxs/vxs $(PREFIX)/bin/

uninstall:
	ocamlfind remove xsops
	rm $(PREFIX)/bin/vxs_fe
