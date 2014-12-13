PKG=gpio
PREFIX=`opam config var prefix`
BUILDOPTS=native=true native-dynlink=true

all: build

test: test.native
	./test.native -shards 1 -runner sequential

test.native: build

build:
	ocaml pkg/build.ml $(BUILDOPTS)

install: build
	opam-installer --prefix=$(PREFIX) $(PKG).install

uninstall: $(PKG).install
	opam-installer -u --prefix=$(PREFIX) $(PKG).install

PHONY: clean

clean:
	ocamlbuild -clean
