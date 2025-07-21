cg ?= chez

all: build install test

build: banner
	@echo ""
	@echo "==========="
	@echo "Building..."
	@echo "==========="
	@echo ""
	idris2 --build choice.ipkg --cg $(cg)

install: build
	@echo ""
	@echo "============="
	@echo "Installing..."
	@echo "============="
	@echo ""
	idris2 --install choice.ipkg --cg $(cg)

test: install
	@echo ""
	@echo "================"
	@echo "Running tests..."
	@echo "================"
	@echo ""
	idris2 --build test/test.ipkg --cg $(cg)
	@echo ""
	pack --cg $(cg) run test/test.ipkg

docs: build
	@echo ""
	@echo "==================="
	@echo "Generating docs..."
	@echo "==================="
	@echo ""
	idris2 --mkdoc choice.ipkg
	@cp -r build/docs docs
banner:
	@echo ""
	@echo "==================="
	@echo "Choice library"
	@echo "Code generation: $(cg)"
	@echo "==================="
	@echo ""