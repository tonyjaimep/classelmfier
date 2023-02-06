.PHONY = default

public/elm.js: src/Main.elm
	elm make src/Main.elm --output=public/elm.js

default: public/elm.js
