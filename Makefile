.PHONY = default

public/elm.js: src/Main.elm
	elm make src/Main.elm --output=public/elm.js --debug

default: public/elm.js
