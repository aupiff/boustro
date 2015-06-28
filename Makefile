ELM_SRC=frontend/Boustrophedon.elm

default: build

build:
	elm-make $(ELM_SRC) --output=static/elm.js
