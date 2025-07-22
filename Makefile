cg ?= chez
pack ?=
all: build install test

build: banner
	@echo ""
	@echo "==========="
	@echo "Building..."
	@echo "==========="
	@echo ""
	pack $(pack) --cg $(cg) build choice.ipkg

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
	pack $(pack) --cg $(cg) build test/test.ipkg
	@echo ""
	pack $(pack) --cg $(cg) run test/test.ipkg

docs: build
	@echo ""
	@echo "==================="
	@echo "Generating docs..."
	@echo "==================="
	@echo ""
	idris2 --mkdoc choice.ipkg
	@cp -r build/docs pages/files/docs
	@cp -r BUILDING.md CONTRIBUTING LICENSE paper/main.pdf pages/files
banner:
	@echo ""
	@echo "==================="
	@echo "Choice library"
	@echo "Code generation: $(cg)"
	@echo "==================="
	@echo ""

clean: 
	rm -f -r build 
	rm -f -r test/build
	rm -f -r pages/files
	mkdir pages/files