.PHONY: clean install-eggs

standup: standup.scm
	csc $^

clean:
	rm -f standup *.o

install-eggs:
	chicken-install -s getopt-long regex-case stty srfi-1 srfi-13 srfi-18
