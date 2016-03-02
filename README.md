Requires the regex-case and stty eggs.

Currently is a crappy console program that rings the terminal bell
to highlight the terminal's icon in the taskbar.  Eventually I'd
like this to be a GUI that pops up and steals focus so you can't miss
it!

Build instructions:
	csc standup.scm

Usage: 
	csi standup.scm (interpreted)
	or
	standup (compiled)

	Spacebar pauses the timer, Z, z or 0 zeroes the timer & triggers
	a state change between standing & sitting.

TODO:
	* Optionally right-justify text in terminal window (for viewing in terminal
	  on bottom-right side of screen)
	* Overtime counter which counts up after time has expired
