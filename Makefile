# This Makefile is mostly a wrapper around Setup.hs for people who
# just want to type make.

all: build

# remove 'haddock' if you module does not provide haddock documentation
doc: haddock
  # Add additional rules for documentation generation here

configure: .setup-config
.setup-config:
	./Setup.hs configure

build: configure
	./Setup.hs build

install: build
	./Setup.hs install

test:
	runhaskell tests/Main.hs

dist:
	./Setup.hs sdist

haddock: configure
	./Setup.hs haddock

clean:
	-./Setup.hs clean
	-rm -rf dist
#	$(MAKE) -C test clean

.PHONY: all configure build install dist haddock doc clean
