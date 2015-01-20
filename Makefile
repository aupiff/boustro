ELM_SRC=boustrophedon.elm

default: build

build:
	elm-make $(ELM_SRC) --output=bous.html
