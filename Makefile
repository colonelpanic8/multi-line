EMACS = emacs
EMACSFLAGS =
CASK = cask
ERTSELECTOR = t
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS = $(shell find .  -maxdepth 1 -name '*.el')
OBJECTS = $(SRCS:.el=.elc)

.PHONY: test compile

$(OBJECTS):
	$(CASK) install
	$(CASK) build

compile : $(OBJECTS)
