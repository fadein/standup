#!/var/lib/bin/csi -ns
(use regex-case)
(use srfi-13)
(use srfi-18)
(use stty)

(define-syntax minutes
  (syntax-rules ()
	((_ n)
	 (* 60 n))))

(define *sit-time*   (minutes 30))
(define *stand-time* (minutes 5))
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


(define (sitdown)
  (bell&title *sit*)

  ;state = #t means "sit"
  ;state = #f means "stand"
  (let loop ((timer *sit-time*) (state #t) (paused #f))

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
				(loop (- timer *interval*) state (not paused)))
			  (else
				(bell&title "[PAUSED]")
				(loop (- timer *interval*) state (not paused)))))

		   ((#\0 #\Z #\z) ;zero the timer on 0, Z, or z
			(loop 0 state paused))

		   ((#\R #\r) ;reset the timer on R or r
			(loop
			  (if state *sit-time* *stand-time*)
			  state paused))

		   (else
			 (loop (- timer *interval*) state paused)))))

	  ;timer is paused - NOOP
	  (paused
		(loop timer state paused))

	  ;transition from sitting to STANDING
	  ((and (<= timer 0) state)
	   (bell&title *stand*)
	   (print "Press [space] to continue...")
	   (loop *stand-time* #f #t))

	  ;transition from STANDING to sitting
	  ((and (<= timer 0) (not state))
	   (bell&title *sit*)
	   (print "Press [space] to continue...")
	   (loop *sit-time* #t #t))

	  ;print a periodic status msg to indicate where we are
	  ((and-let* ((t (truncate timer))
			  ((< (- timer t) *interval*)))
		 (printf "~a...\r~!" (prettySeconds t)))
		 (loop (- timer *interval*) state paused))

	  ;otherwise, decrement timer
	  (else (loop (- timer *interval*) state paused)))))

(with-stty '(not icanon echo) sitdown)

; vim:set ft=scheme:
