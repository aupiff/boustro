ELM_SRC=frontend/Boustrophedon.elm
MENU_SRC=frontend/Menu.elm

default: build

build:
	elm-make $(ELM_SRC) --output=frontend/elm.js

menu:
	elm-make $(MENU_SRC) --output=frontend/menu.html
