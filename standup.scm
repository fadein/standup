#!/usr/bin/csi -ns
(use ansi-escape-sequences)
(use regex-case)
(use srfi-1)
(use srfi-13)
(use srfi-18)
(use stty)

(define-syntax seconds
  (syntax-rules ()
	((_ n)
	 n)))

(define-syntax minutes
  (syntax-rules ()
	((_ n)
	 (* 60 n))))

(define *sit-time*   (minutes 25))
(define *stand-time* (minutes 10))
(define *interval*   0.125)

(define *stand*      "stand up!")
(define *sit*        "sit down")

(define (bell&title str)
  (regex-case (get-environment-variable "TERM")
			  ("screen.*|xterm.*|rxvt.*" _
			   (printf "~a~n]0;~a~!" str str))
			  (else
				(print "TERM=" (get-environment-variable "TERM")))))


(define (prettySeconds s)
  (let* ((hours   (inexact->exact (truncate (/ s 60 60))))
		 (hours_s (string-pad (number->string hours) 2 #\0))
		 (minut_s (string-pad
					(number->string
					  (inexact->exact
						(remainder (truncate (/ s 60)) 60))) 2 #\0))
		 (secon_s (string-pad
					(number->string
					  (inexact->exact (remainder s 60))) 2 #\0)))
	(if (zero? hours)
	  (sprintf "~a:~a" minut_s secon_s)
	  (sprintf "~a:~a:~a" hours_s minut_s secon_s))))


;; Big, sorta awkward group of Pomodoro banners

(define a-pomodoro
  (let ((texts (list
#<<POMO
 ____  ____  _      ____  ____  ____  ____  ____  _
/  __\/  _ \/ \__/|/  _ \/  _ \/  _ \/  __\/  _ \/ \
|  \/|| / \|| |\/||| / \|| | \|| / \||  \/|| / \|| |
|  __/| \_/|| |  ||| \_/|| |_/|| \_/||    /| \_/|\_/
\_/   \____/\_/  \|\____/\____/\____/\_/\_\\____/(_)
POMO

#<<POMO
d8888b.  .d88b.  .88b  d88.  .d88b.  d8888b.  .d88b.  d8888b.  .d88b.  db
88  `8D .8P  Y8. 88'YbdP`88 .8P  Y8. 88  `8D .8P  Y8. 88  `8D .8P  Y8. 88
88oodD' 88    88 88  88  88 88    88 88   88 88    88 88oobY' 88    88 YP
88~~~   88    88 88  88  88 88    88 88   88 88    88 88`8b   88    88
88      `8b  d8' 88  88  88 `8b  d8' 88  .8D `8b  d8' 88 `88. `8b  d8' db
88       `Y88P'  YP  YP  YP  `Y88P'  Y8888D'  `Y88P'  88   YD  `Y88P'  YP
POMO

#<<POMO
 _____                          _                 _
|  __ \                        | |               | |
| |__) |__  _ __ ___   ___   __| | ___  _ __ ___ | |
|  ___/ _ \| '_ ` _ \ / _ \ / _` |/ _ \| '__/ _ \| |
| |  | (_) | | | | | | (_) | (_| | (_) | | | (_) |_|
|_|   \___/|_| |_| |_|\___/ \__,_|\___/|_|  \___/(_)
POMO

#<<POMO
 ____  _____  __  __  _____  ____  _____  ____  _____ /\
(  _ \(  _  )(  \/  )(  _  )(  _ \(  _  )(  _ \(  _  ))(
 )___/ )(_)(  )    (  )(_)(  )(_) ))(_)(  )   / )(_)( \/
(__)  (_____)(_/\/\_)(_____)(____/(_____)(_)\_)(_____)()
POMO
#<<POMO
 ______                          __                    __
|   __ \.-----.--------.-----.--|  |.-----.----.-----.|  |
|    __/|  _  |        |  _  |  _  ||  _  |   _|  _  ||__|
|___|   |_____|__|__|__|_____|_____||_____|__| |_____||__|
POMO

#<<POMO
8888888b.                                   888                       888
888   Y88b                                  888                       888
888    888                                  888                       888
888   d88P .d88b. 88888b.d88b.  .d88b.  .d88888 .d88b. 888d888 .d88b. 888
8888888P" d88""88b888 "888 "88bd88""88bd88" 888d88""88b888P"  d88""88b888
888       888  888888  888  888888  888888  888888  888888    888  888Y8P
888       Y88..88P888  888  888Y88..88PY88b 888Y88..88P888    Y88..88P "
888        "Y88P" 888  888  888 "Y88P"  "Y88888 "Y88P" 888     "Y88P" 888
POMO

#<<POMO
 ____    ___   ___ ___   ___   ___     ___   ____    ___   __
|    \  /   \ |   T   T /   \ |   \   /   \ |    \  /   \ |  T
|  o  )Y     Y| _   _ |Y     Y|    \ Y     Y|  D  )Y     Y|  |
|   _/ |  O  ||  \_/  ||  O  ||  D  Y|  O  ||    / |  O  ||__j
|  |   |     ||   |   ||     ||     ||     ||    \ |     | __
|  |   l     !|   |   |l     !|     |l     !|  .  Yl     !|  T
l__j    \___/ l___j___j \___/ l_____j \___/ l__j\_j \___/ l__j
POMO

#<<POMO
 _______                         __                  __
|   _   .-----.--------.-----.--|  .-----.----.-----|  |
|.  1   |  _  |        |  _  |  _  |  _  |   _|  _  |__|
|.  ____|_____|__|__|__|_____|_____|_____|__| |_____|__|
|:  |
|::.|
`---'
POMO

#<<POMO
______                         _                 _
| ___ \                       | |               | |
| |_/ /__  _ __ ___   ___   __| | ___  _ __ ___ | |
|  __/ _ \| '_ ` _ \ / _ \ / _` |/ _ \| '__/ _ \| |
| | | (_) | | | | | | (_) | (_| | (_) | | | (_) |_|
\_|  \___/|_| |_| |_|\___/ \__,_|\___/|_|  \___/(_)
POMO

#<<POMO
 _______  _______  _______  _______  ______   _______  _______  _______  _
(  ____ )(  ___  )(       )(  ___  )(  __  \ (  ___  )(  ____ )(  ___  )( )
| (    )|| (   ) || () () || (   ) || (  \  )| (   ) || (    )|| (   ) || |
| (____)|| |   | || || || || |   | || |   ) || |   | || (____)|| |   | || |
|  _____)| |   | || |(_)| || |   | || |   | || |   | ||     __)| |   | || |
| (      | |   | || |   | || |   | || |   ) || |   | || (\ (   | |   | |(_)
| )      | (___) || )   ( || (___) || (__/  )| (___) || ) \ \__| (___) | _
|/       (_______)|/     \|(_______)(______/ (_______)|/   \__/(_______)(_)
POMO

#<<POMO
'||'''|,                              ||`                      ||
 ||   ||                              ||                       ||
 ||...|' .|''|, '||),,(|,  .|''|, .|''||  .|''|, '||''| .|''|, ||
 ||      ||  ||  || || ||  ||  || ||  ||  ||  ||  ||    ||  ||
.||      `|..|' .||    ||. `|..|' `|..||. `|..|' .||.   `|..|' ||
POMO

#<<POMO
   ___                                  /7
  / o | _   _     _    _// _   _   _   //
 / _,','o| / \'\,'o| ,'o/,'o| //7,'o|
/_/   |_,'/_nn_/|_,'|__/ |_,'//  |_,'()
POMO

)))
(lambda ()
	(list-ref texts (random (length texts))))))

; How to tell that we're at the end of the circular list
(define *last-color* 'fg-red)

(define (sitdown)
  (bell&title *sit*)

  ;state = #t means "sit"
  ;state = #f means "stand"
  (let loop ((timer *sit-time*) (state #t) (paused #f)
								(colors (circular-list 'fg-blue 'fg-green 'fg-yellow *last-color*)))

	(thread-sleep! *interval*)

	(cond

	  ;handle input characters
	  ((char-ready?)
	   (let ((char (read-char)))
		 (case char
		   ((#\space) ;pause/resume on SPACE
			(cond
			  (paused
				(bell&title (if state *sit* *stand*))
				(loop (- timer *interval*) state (not paused) colors))
			  (else
				(bell&title "[PAUSED]")
				(loop (- timer *interval*) state (not paused) colors))))

		   ((#\0 #\Z #\z) ;zero the timer on 0, Z, or z
			(loop 0 state paused colors))

		   ((#\R #\r) ;reset the timer on R or r
			(loop
			  (if state *sit-time* *stand-time*)
			  state paused colors))

		   ((#\Q #\q) ;quit
			(exit))

		   (else
			 (loop (- timer *interval*) state paused colors)))))

	  ;timer is paused - NOOP
	  (paused
		(loop timer state paused colors))

	  ;transition from sitting to STANDING
	  ((and (<= timer 0) state)
	   (bell&title *stand*)
	   (cond
		 ;; when we're at the end of our list of colors, the cycle is almost complete.
		 ;; it's time for a big pomodoro break!!!
		 ((eq? (car colors) *last-color*)
		  (print (set-text `(bold ,(car colors)) (a-pomodoro)))
		  (print "\nPress [space] to continue...")
		  (loop *sit-time* #f #t colors))
		 (else
		   (print "Press [space] to continue...")
		   (loop *stand-time* #f #t colors))))

	  ;transition from STANDING to sitting
	  ((and (<= timer 0) (not state))
	   (bell&title *sit*)
	   (print "Press [space] to continue...")
	   (loop *sit-time* #t #t (cdr colors)))

	  ;print a periodic status msg to indicate where we are
	  ((and-let* ((t (truncate timer))
			  ((< (- timer t) *interval*)))
		 (printf "~a...\r~!" (set-text `(bold ,(car colors)) (prettySeconds t))))
		 (loop (- timer *interval*) state paused colors))

	  ;otherwise, decrement timer
	  (else (loop (- timer *interval*) state paused colors)))))

(with-stty '(not icanon echo) sitdown)

; vim:set ft=scheme:
