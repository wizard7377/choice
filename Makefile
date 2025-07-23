cg ?= chez
pack ?=
opts ?= 

build: banner
	@echo ""
	@echo "==========="
	@echo "Building..."
	@echo "==========="
	@echo ""
	pack $(pack) --cg $(cg) --extra-args="$(opts)" build choice.ipkg

install: build
	@echo ""
	@echo "============="
	@echo "Installing..."
	@echo "============="
	@echo ""
	idris2 --cg $(cg) --install choice.ipkg

test: install
	@echo ""
	@echo "================"
	@echo "Running tests..."
	@echo "================"
	@echo ""
	pack $(pack) --cg $(cg) --extra-args="$(opts)" build test/test.ipkg
	@echo ""
	pack $(pack) --cg $(cg) --extra-args="$(opts)" run test/test.ipkg

bench : opts += --profile --timing 5
bench: install
	@echo ""
	@echo "================================"
	@echo "Running tests with benchmarks..."
	@echo "================================"
	@echo ""
	@rm choice-test.ss.html
	@rm profile.html
	@rm -rf profile 
	mkdir profile
	pack $(pack) --cg $(cg) --extra-args="$(opts)" build test/test.ipkg
	@echo ""
	pack $(pack) --cg $(cg) --extra-args="$(opts)" run test/test.ipkg
	mv choice-test.ss.html profile.html profile


docs: build
	@echo ""
	@echo "==================="
	@echo "Generating docs..."
	@echo "==================="
	@echo ""
	idris2 --mkdoc choice.ipkg
	@cp -r build/docs docs/files/docs
	@cp -r BUILDING.md CONTRIBUTING LICENSE paper/main.pdf docs/files
banner:
	@echo ""
	@echo "==================="
	@echo "Choice library"
	@echo "Code generation: $(cg)"
	@echo "==================="
	@echo ""
	# pack $(pack) --cg $(cg) run test/test.ipkg
clean: 
	rm -f -r build 
	rm -f -r test/build
	rm -f -r docs/files
	mkdir docs/files
	pack clean 
	

all: build install test