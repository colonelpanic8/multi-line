EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(shell find .  -maxdepth 1 -name '*.el')
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)
ERTSELECTOR = t

.PHONY: test compile clean

$(OBJECTS):
	$(CASK) install
	$(CASK) build

compile : $(OBJECTS)

clean:
	rm -rf $(OBJECTS) .cask/

test : $(OBJECTS)
	cask exec ert-runner -L $(PWD)
