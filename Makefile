EXE=longboye

help:
	@cat Makefile

build:
	stack build

clean:
	\rm -rf .stack-work

hlint:
	stack exec hlint .

install:
	stack install .

run:
	stack exec $(EXE) $(ARGS)

setup:
	stack setup

test: build
	cp src/Longboye/Imports/Cracker.hs /tmp/foo.hs
	stack exec longboye -- imports /tmp/foo.hs
	head -10 /tmp/foo.hs

b: build
hl: hlint
i: install
r: run
s: setup
t: test

