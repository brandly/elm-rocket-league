.DEFAULT_GOAL := all

elm = ./node_modules/.bin/elm
gh-pages = ./node_modules/.bin/gh-pages
uglifyjs = ./node_modules/.bin/uglifyjs
elm-analyse = ./node_modules/.bin/elm-analyse

all: dist/ dist/elm.js dist/index.html dist/static

dist/:
	mkdir -p $@

node_modules/:
	npm install

dist/elm.js: $(shell find src -type f) package.json elm.json node_modules/
	$(elm) make src/Main.elm --output $@

dist/index.html: src/index.html
	cp src/index.html dist/

dist/static: dist/ $(shell find src/static -type f)
	mkdir -p dist/static
	rsync -rupE src/static dist/

production: all
	$(elm) make src/Main.elm --output dist/elm.js --optimize
	$(uglifyjs) dist/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | $(uglifyjs) --mangle --output dist/elm.js

deploy: production
	elm-to-dot src/Main.elm --include-external \
		| dot -Tsvg \
		| svgo --input - \
		> dist/dependency-graph.svg
	$(gh-pages) --dist dist

test: node_modules/
	$(elm-analyse)

clean:
	rm -Rf elm-stuff
	rm -Rf node_modules
	rm -Rf dist
