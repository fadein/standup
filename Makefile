CSC ?= csc
CSC_OPTIONS ?= -static
INSTALL ?= install
RM      ?= rm
RMDIR   ?= rmdir
SUDO    ?= sudo

BINDIR  ?= /usr/local/bin

standup: standup.scm
	$(CSC) $(CSC_OPTIONS) $^

install: standup
	$(SUDO) $(INSTALL) -d -m 775 $(BINDIR)
	$(SUDO) $(INSTALL) -p -m 755 -t $(BINDIR) $^

uninstall:
	-$(RM) $(BINDIR)/standup

clean:
	-rm -f standup *.o

configure:
	chicken-install -s getopt-long regex-case stty srfi-1 srfi-13 srfi-18


.PHONY: clean configure
