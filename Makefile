build:
	idris2 --build choice.ipkg 

install: build
	idris2 --install choice.ipkg

buildRefc:
	idris2 --build choice.ipkg --cg refc

installRefc: build
	idris2 --install choice.ipkg --cg refc
test: install
	@echo ""
	@echo "Running tests... (Default)"
	@echo ""
	idris2 --build test/test.ipkg
	pack run test/test.ipkg

testRefc: installRefc
	@echo ""
	@echo "Running tests with reference counting... (RefC)"
	@echo ""
	idris2 --build test/test.ipkg --cg refc
	pack --cg refc run test/test.ipkg

testAll: test testRefc

docs: install 
	pack --with-docs install choice 
	cp -r build/docs docs
clean: 
	