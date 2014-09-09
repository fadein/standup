.PHONY: clean

standup: standup.scm
	csc $^

clean:
	rm -f standup *.o
