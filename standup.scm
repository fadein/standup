#!/var/lib/bin/csi -ns
(use regex-case)
(use srfi-18)
(use stty)

(define *major* (* 60 20))
(define *minor* (* 60 2))
(define *interval*   0.125)

(define *stand* "stand up!")
(define *sit* "sit down")

(define (bell&title str)
  (regex-case (get-environment-variable "TERM")
	("screen.*|xterm.*|rxvt.*" _
	 ;(print _)  ;DELETE ME
	 (printf "~a~n]0;~a" str str))
	(else
	  (print "TERM=" (get-environment-variable "TERM")))))

(define (sitdown)
  (bell&title *sit*)
  (let loop ((timer *major*) (state #t) (paused #f))

	(thread-sleep! *interval*)

	(cond

	  ((char-ready?)
	   (let ((char (read-char)))
		 (if (char=? #\space char)
		   (cond
			 (paused
			   (bell&title (if state *sit* *stand*))
			   (loop (- timer *interval*) state (not paused)))
			 (else
			   (bell&title "[PAUSED]")
			   (loop (- timer *interval*) state (not paused))))
		   (loop (- timer *interval*) state paused))))

	  (paused
		(loop timer state paused))

	  ;transition from sitting to STANDING
	  ((and (<= timer 0) state)
	   (bell&title *stand*)
	   (loop *minor* #f paused))

	  ;transition from STANDING to sitting
	  ((and (<= timer 0) (not state))
	   (bell&title *sit*)
	   (loop *major* #t paused))

	  ;print a periodic status msg to indicate where we are
	  ;TODO: fix this up
	  ((zero? (remainder (truncate timer) 10))
	   (printf "~a...~n" timer)
	   (loop (- timer *interval*) state paused))

	  (else (loop (- timer *interval*) state paused)))))

(with-stty '(not icanon) sitdown)

; vim:set ft=scheme:
