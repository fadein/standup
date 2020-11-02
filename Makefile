CSC          ?= csc
CSC_OPTIONS  ?= -static
CHICKEN-INST ?= chicken-install
INSTALL      ?= install
RM           ?= rm
RMDIR        ?= rmdir
SUDO         ?= sudo
BINDIR       ?= /usr/local/bin

standup: standup.scm
	$(CSC) $(CSC_OPTIONS) $^

clean:
	-$(RM) -f standup *.o standup.link

install-eggs:
	$(CHICKEN-INST) -s ansi-escape-sequences getopt-long regex-case srfi-1 srfi-13 srfi-18 stty

install: standup
	$(SUDO) $(INSTALL) -d -m 775 $(BINDIR)
	$(SUDO) $(INSTALL) -p -m 755 -t $(BINDIR) $^


.PHONY: clean install-eggs install
