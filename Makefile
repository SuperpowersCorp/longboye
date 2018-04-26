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

longboye-all:
	longboye imports app
	longboye imports src
	longboye imports test
	longboye pragmas app
	longboye pragmas src
	longboye pragmas test

run:
	stack exec $(EXE) $(ARGS)

setup:
	stack setup

test:
	stack test

watch:
	stack exec -- ghcid -c 'stack ghci' --restart stack.yaml

watch-test:
	stack test --fast --file-watch

b: build
hl: hlint
i: install
lba: longboye-all
r: run
s: setup
t: test
w: watch
wt: watch-test

.PHONY: test
