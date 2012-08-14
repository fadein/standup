#!/var/lib/bin/csi -s

(define *major* (* 60 20))
(define *minor* (* 60 2))

(let loop ((timer *major*) (state #t))
  (sleep 1)
  (cond

	((and (zero? timer) state)
	 (print "stand up!")
	 (loop *minor* #f))

	((and (zero? timer) (not state))
	 (print "sit down!")
	 (loop *major* #t))

	((zero? (remainder timer 10))
	   (printf "~a...~n" timer)
	   (loop (sub1 timer) state))

	(else (loop (sub1 timer) state))))



