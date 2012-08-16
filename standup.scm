#!/var/lib/bin/csi -s
(use regex-case)
(use posix)

(define *major* (* 60 20))
(define *minor* (* 60 2))

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
	(sleep 1)
	(cond

	  ((char-ready?)
	   (let ((char (read-char)))
		 (if (char=? #\space char)
		   (cond
			 (paused
			   (bell&title (if state *sit* *stand*))
			   (loop (sub1 timer) state (not paused)))
			 (else
			   (bell&title "[PAUSED]")
			   (loop (sub1 timer) state (not paused))))
		   (loop (sub1 timer) state paused))))

	  (paused
		(loop timer state paused))

	  ((and (zero? timer) state)
	   (bell&title *stand*)
	   (loop *minor* #f paused))

	  ((and (zero? timer) (not state))
	   (bell&title *sit*)
	   (loop *major* #t paused))

	  ((zero? (remainder timer 10))
	   (printf "~a...~n" timer)
	   (loop (sub1 timer) state paused))

	  (else (loop (sub1 timer) state paused)))))

(set-buffering-mode! (current-input-port) #:none)

(sitdown)

; vim:set ft=scheme:
