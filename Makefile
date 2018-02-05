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

test:
	stack test

watch:
	stack exec -- ghcid -c 'stack ghci' --restart stack.yaml

b: build
hl: hlint
i: install
r: run
s: setup
t: test
w: watch

.PHONY: test
