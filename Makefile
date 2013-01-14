.PHONY: all clean install build
all: build doc

J=4

export OCAMLRUNPARAM=b

setup.ml: _oasis
	oasis setup

setup.bin: setup.ml
	@ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	@rm -f setup.cmx setup.cmi setup.o setup.cmo

build: setup.bin
	./setup.bin -configure --enable-tests
	./setup.bin -build -j $(J)

setup.data: setup.bin
	@./setup.bin -configure

build: setup.data setup.bin
	@./setup.bin -build -j $(J)

doc: setup.data setup.bin
	@./setup.bin -doc -j $(J)

install: setup.bin
	@./setup.bin -install

clean:
	@ocamlbuild -clean
	@rm -f setup.data setup.log setup.bin
