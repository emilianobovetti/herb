base-dir := $(CURDIR)
node-bin := $(base-dir)/node_modules/.bin
elm-make := $(node-bin)/elm-make
elm-analyse := $(node-bin)/elm-analyse
elm-pages := $(wildcard elm/src/Page/*)

all : dist

.PHONY: check-elm-make
check-elm-make :
ifeq ("$(wildcard $(elm-make))", "")
	@yarn
endif

.PHONY: elm
elm : check-elm-make
	@mkdir -p build
	@cd elm && $(elm-make) $(subst elm/,,$(elm-pages)) \
		--output=$(base-dir)/build/compiled-elm.js --warn --yes

.PHONY: dist
dist : elm
	@mkdir -p dist
ifeq ("$(shell uname)", "")
	@yarn run windows-dist
else
	@yarn run linux-dist
endif

.PHONY: dist-env
dist-env : dist
	@rm -rf dist/electron-builder.yaml $(wildcard dist/*-unpacked)
	@cp -rf settings.json database dist

.PHONY: run
run : elm
	@echo "Running electron..."
	@yarn run electron >/dev/null &

.PHONY: elm-analyse
elm-analyse : elm
	@cd elm && $(elm-analyse)

.PHONY: test
test : check-elm-make
	@cd elm && elm-test

.PHONY: clean
clean :
	@rm -rf build dist node_modules yarn.lock yarn-error.log
	@rm -rf elm/elm-stuff elm/tests/elm-stuff
