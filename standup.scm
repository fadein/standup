#!/var/lib/bin/csi -s
(use regex-case)
(use posix)

(define *major* (* 60 20))
(define *minor* (* 60 2))

(define (bell&title str)
  (regex-case (get-environment-variable "TERM")
	("screen.*|xterm.*|rxvt.*" _
	 (print _)  ;DELETE ME
	 (printf "~a~n]0;~a" str str))
	(else
	  (print "TERM=" (get-environment-variable "TERM")))))

(define (sitdown)
  (bell&title "sit down!")
  (let loop ((timer *major*) (state #t))
	(sleep 1)
	(cond

	  ((and (zero? timer) state)
	   (bell&title "stand up!")
	   (loop *minor* #f))

	  ((and (zero? timer) (not state))
	   (bell&title "sit down!")
	   (loop *major* #t))

	  ((zero? (remainder timer 10))
	   (printf "~a...~n" timer)
	   (loop (sub1 timer) state))

	  (else (loop (sub1 timer) state)))))

(sitdown)

; vim:set ft=scheme:
