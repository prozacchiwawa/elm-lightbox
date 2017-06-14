all: out/index.js

out:
	mkdir -p out

out/index.js: out src/*.elm
	elm-make --output $@ src/Main.elm

clean:
	rm -rf out
