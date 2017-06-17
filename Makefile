all: out/index.js CodeMirror/lib/codemirror.js

CodeMirror/lib/codemirror.js: CodeMirror
	cd CodeMirror && npm install

out:
	mkdir -p out

out/index.js: out src/*.elm
	elm-make --output $@ src/Main.elm

clean:
	rm -rf out
