ELM_SRC=frontend/Boustrophedon.elm
DEMO_SRC=frontend/TypographyDemos.elm

default: build

build:
	elm-make $(ELM_SRC) --output=frontend/elm.js

demos:
	elm-make $(DEMO_SRC) --output=frontend/demo.html
