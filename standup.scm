#!/usr/bin/csi -ns

(use ansi-escape-sequences
	 getopt-long
	 regex-case
	 srfi-1
	 srfi-13
	 srfi-18
	 stty)

(define (cleanup signal)
  (print (show-cursor) (set-text '(fg-white) ""))
  (exit))
(set-signal-handler! signal/term  cleanup)
(set-signal-handler! signal/int   cleanup)
(set-signal-handler! signal/pipe  cleanup)
(set-signal-handler! signal/quit  cleanup)

; get dimensions of the screen
(define *COLUMNS* #f)
(define *LINES*   #f)
(define (get-screen-dimensions _)
  (let-syntax ((tput (syntax-rules ()
								   ((tput cmd)
									(let-values (((in out pid) (process (conc "tput " cmd))))
												(let ((ret (read-line in)))
												  (close-input-port in)
												  (close-output-port out)
												  ret))))))
  (let ((cols (tput "cols"))
		(lines (tput "lines")))
	(print "get-screen-dimensions():" cols "x" lines)
	(set! *COLUMNS* (if (not (equal? cols  "")) (string->number cols  ) 80))
	(set! *LINES*   (if (not (equal? lines "")) (string->number lines ) 24)))))
(set-signal-handler! signal/winch get-screen-dimensions)


(define-syntax seconds
  (syntax-rules ()
	((_ n)
	 n)))

(define-syntax minutes
  (syntax-rules ()
	((_ n)
	 (* 60 n))))

(define *sit-time*   (minutes 60))
(define *stand-time* (minutes 10))
(define *interval*   0.125)

(define *stand*      "stand up!")
(define *sit*        "sit down")

; How to tell that we're at the end of the circular list
(define *last-color* 'fg-red)

(define (strip-ansi-colors str)
  (string-delete char-set:iso-control
				 (if (char=? #\escape (string-ref str 0))
				   (let* ((first (string-index str #\m))
						  (end (string-index str #\escape first)))
					 (substring str (add1 first) end))
				   str)))


(define (bell) "\007")

;; ring the terminal bell, update the XTerm title,
;; and update the bottom line of the terminal's text
(define (bell&title str)
  (print* str (set-title (strip-ansi-colors str)) (bell)))

;; pretty-print '(hours minutes seconds) into hours:minutes:seconds
(define (prettySeconds hms)
  (string-join (map (lambda (s) (string-pad (number->string s) 2 #\0))
					(if (zero? (car hms)) (cdr hms) hms)) ":"))

;; convert seconds of time into h:m:s integer components
(define (seconds->hms s)
  (let ((hours   (inexact->exact (truncate (/ s 60 60))))
		(minutes (inexact->exact (remainder (truncate (/ s 60)) 60)))
		(seconds (inexact->exact (remainder s 60))))
	(list hours minutes seconds)))

(define (sitdown)
  (bell&title (set-text '(bold fg-blue) *sit*))
  (newline)

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
			(erase-line)
			(bell&title
			  (if paused
				(set-text `(bold ,(car colors)) (conc (if state *sit* *stand*) "\r"))
				(set-text (list (car colors)) "[PAUSED]\r")))
			(loop (- timer *interval*) state (not paused) colors))

		   ((#\0 #\Z #\z) ;zero the timer on 0, Z, or z
			(loop 0 state paused colors))

		   ((#\+) ; add a minute to a running timer
			(loop (if paused timer (+ timer 60)) state paused colors))

		   ((#\-) ; subtract a minute from the timer
			(loop (if paused timer (- timer 60)) state paused colors))

		   ((#\R #\r) ;reset the timer on R or r
			(loop
			  (if state *sit-time* *stand-time*)
			  state paused colors))

		   ((#\Q #\q) ;quit
			#f)

		   ((#\H #\h #\?) ;help
			(erase-line)
			(print (string-join '("Pomodoro timer usage:"
								  "\tH,h,?\tThis usage message"
								  "\tSPACE\tPause/Resume"
								  "\t0,Z,z\tZero the timer"
								  "\tR,r\tReset timer"
								  "\tQ,q\tQuit")
								"\n"))
			(loop (- timer *interval*) state paused colors))

		   (else
			 (loop (- timer *interval*) state paused colors)))))

	  ;timer is paused - NOOP
	  (paused
		(loop timer state paused colors))

	  ;transition from sitting to STANDING
	  ((and (<= timer 0) state)
	   (bell&title (set-text `(bold ,(car colors)) (conc *stand* "\r")))
	   (newline)
	   (cond
		 ;; when we're at the end of our list of colors, the cycle is almost complete.
		 ;; it's time for a big pomodoro break!!!
		 ((eq? (car colors) *last-color*)
		  (print (set-text `(bold ,(car colors)) (a-pomodoro)))
		  (print (set-text (list (car colors)) "\nPress [space] to continue..."))
		  (loop *sit-time* #f #t colors))
		 (else
		   (print (set-text (list (car colors)) "Press [space] to continue..."))
		   (loop *stand-time* #f #t colors))))

	  ;transition from STANDING to sitting
	  ((and (<= timer 0) (not state))
	   (bell&title (set-text `(bold ,(cadr colors)) (conc *sit* "\r")))
	   (newline)
	   (print (set-text (list (cadr colors)) "Press [space] to continue..."))
	   (loop *sit-time* #t #t (cdr colors)))

	  ;update time display once per second
	  ((and-let* ((t (truncate timer))
				  ((< (- timer t) *interval*))
				  (hms (seconds->hms t)))
				 (printf "~a...           \r~!" (set-text `(bold ,(car colors)) (prettySeconds hms)))
				 (print* (set-title
						   (string-join
							 (list (if state *sit* *stand*)
								   (if (zero? (car hms))
									 (conc (cadr hms) "m")
									 (conc (car hms) "h" (cadr hms) "m")))
							 " "))))
	   (loop (- timer *interval*) state paused colors))

	  ;otherwise, decrement timer
	  (else (loop (- timer *interval*) state paused colors)))))


;; Big, sorta awkward group of Pomodoro banners
(define a-pomodoro
  (let* ((vector-shuffle
		   (lambda (v)
			 (do ((n (vector-length v) (- n 1))) ((zero? n) v)
			   (let* ((r (random n)) (t (vector-ref v r)))
				 (vector-set! v r (vector-ref v (- n 1)))
				 (vector-set! v (- n 1) t)))))
		 (texts
		  (vector->list
			(vector-shuffle
			  #(
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

)))))

; make this list circular
(set-cdr! (last-pair texts) texts)
(lambda ()
  (set! texts (cdr texts))
	(car texts))))

(let ((grammar
		`((standup-time
			,(conc "Number of minutes for the standup interval (default " (/ *stand-time* 60) ")")
			(value #t)
			(single-char #\u))
		  (sitdown-time
			,(conc "Number of minutes for the sitdown interval (default " (/ *sit-time* 60) ")")
			(value #t)
			(single-char #\d))
		  (help
			"This usage message"
			(single-char #\h)))))

  (let ((opts (getopt-long (command-line-arguments) grammar
						   unknown-option-handler:
						   (lambda (l)
							 (printf "Unrecognized argument~a ~a~n"
									 (if (= 1 (length l)) "" "s")
									 l)
							 (print (usage grammar))
							 (exit 7)))))

	(when (assoc 'help opts)
	  (print "standup usage:")
	  (print (usage grammar))
	  (exit 1))

	(when (assoc 'standup-time opts)
	  (set! *stand-time* (minutes (string->number (cdr (assoc 'standup-time opts))))))
	(when (assoc 'sitdown-time opts)
	  (set! *sit-time* (minutes (string->number (cdr (assoc 'sitdown-time opts))))))))

(get-screen-dimensions #f)
(print *COLUMNS* "x" *LINES*)
(print* (hide-cursor))
(with-stty '(not icanon echo) sitdown)
(print (show-cursor))

; vim:set ft=scheme:
