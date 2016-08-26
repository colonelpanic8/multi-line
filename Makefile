CASK = cask

SRCS = $(shell find .  -maxdepth 1 -name '*.el')
OBJECTS = $(SRCS:.el=.elc)

.PHONY: test compile clean install

.cask:
	cask install

$(OBJECTS): .cask
	$(CASK) install
	$(CASK) build

compile: $(OBJECTS)

clean:
	rm -rf $(OBJECTS) .cask/

test: $(OBJECTS)
	cask exec ert-runner -L $(PWD)
