
all: build/backend-exe

build: package.yaml $(shell find src -name "*.hs")
	mkdir build
	stack build backend

build/backend-exe: build
	echo "Copying from '$(shell stack exec whereis backend-exe | cut -d' ' -f2)'"
	cp "$(shell stack exec whereis backend-exe | cut -d' ' -f2)" "build/backend-exe"


clean:
	rm -rf build
	rm -f stack-work

.PHONY: clean
