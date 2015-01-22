ELM_SRC=frontend/boustrophedon.elm

default: build

build:
	elm-make $(ELM_SRC) --output=frontend/elm.js
