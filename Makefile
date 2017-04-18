CASK = cask

SRCS = $(shell find . -maxdepth 1 -name '*.el')
OBJECTS = $(SRCS:.el=.elc)
EMACS = $(shell sh -c 'type -P emacs')

.PHONY: test compile clean install

.cask:
	$(CASK) install

$(OBJECTS): .cask
	$(CASK) build

compile: $(OBJECTS)

install: compile

clean-elc:
	rm -f $(OBJECTS)

clean: clean-elc
	rm -rf .cask/

test: $(OBJECTS)
	cask exec ert-runner -L $(PWD)
