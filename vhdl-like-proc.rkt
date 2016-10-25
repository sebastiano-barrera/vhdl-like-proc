#lang racket

(struct signal (name current-value projected-value wait-list event) #:mutable)
(struct process (state continuation) #:mutable)
(struct timer (end-time waiting-process))

(define signals '())
(define timers '())

(define (process-active? process)
  (equal? (process-state process) 'active))

(define (add-signal name init-value)
  (let ([sig (signal name init-value init-value '() #f)])
    (set! signals (cons sig signals))
    sig))

(define (signal-update signal)
  (set-signal-current-value! signal
                             (signal-projected-value signal))
  (set-signal-event! signal #f))

(define (signal-add-observer signal proc)
  (set-signal-wait-list! signal (cons current-process
                                      (signal-wait-list signal))))

(define (add-timer end-time waiting-process)
  (let ([t (timer end-time waiting-process)])
    (set! timers (cons t timers))
    t))

(define (timer-expired? timer)
  (>= (current-inexact-milliseconds) (timer-end-time timer)))

(define (wake-process proc)
  (set-process-state! proc 'active))

(define current-process #f)
(define scheduler-continuation #f)

(define (scheduler processes)
  (define (run-process proc)
    (set! current-process proc)
    (call/cc (lambda (cc)
               (set! scheduler-continuation cc)
               ((process-continuation proc)))))

  (define (check-pending-timers)
    (let-values ([(expired-timers active-timers)
                  (partition timer-expired? timers)])
      (for ([t expired-timers])
        (wake-process (timer-waiting-process t)))
      (set! timers active-timers)))

  (define (wait-first-pending-timer timers)
    (let ([first-timer (argmin timer-end-time timers)])
      (unless (timer-expired? first-timer)
        (let* ([earliest-end-time (timer-end-time first-timer)]
               [interval-ms (- earliest-end-time (current-inexact-milliseconds))]
               [interval-secs (/ interval-ms 1000)])
          (when (positive? interval-secs)
            (sleep interval-secs))))
      (check-pending-timers)))

  (set! timers '())
  (let scheduler-cycle ()
    (let ([active-processes (filter process-active? processes)])
      (if (null? active-processes)
          ;; No active processes => If there are timers, let's wait
          ;; the one that timeouts the first and then start a new
          ;; cycle; otherwise, all jobs are done and we can quit.
          (if (null? timers)
              (printf " ~ Ended\n")
              (begin (wait-first-pending-timer timers)
                     (scheduler-cycle)))

          (begin
            (for-each run-process active-processes)
            (for-each signal-update signals)
            (check-pending-timers)
            (scheduler-cycle))))))

(define (run-system processes-funcs)
  (let ([processes (for/list ([func processes-funcs])
                     (process 'active
                              (lambda ()
                                (func)
                                (set-process-state! current-process 'dead))))])
    (scheduler processes)))

(define (save-process-and-call next-call)
  (call/cc
   (lambda (proc-cc)
     (set-process-continuation! current-process proc-cc)
     (next-call))))

(define (signal-set! signal value)
  (set-signal-projected-value! signal value)
  (set-signal-event! signal #t)
  (for-each wake-process (signal-wait-list signal))
  ;; (printf " :: ~a <= ~a  [woken ~a procs]\n"
  ;;         (signal-name signal) value
  ;;         (length (signal-wait-list signal)))
  (set-signal-wait-list! signal '())
  ; (save-process-and-call scheduler-continuation)
  (void))

(define (wait-on signal)
  (signal-add-observer signal current-process)
  (set-process-state! current-process 'waiting)
  (save-process-and-call scheduler-continuation)
  ;; On resume, return the signal's new value
  (signal-current-value signal))

(define (wait-interval interval-seconds)
  (let* ([interval-ms (* interval-seconds 1000)]
         [end-time (+ (current-inexact-milliseconds) interval-ms)])
    (add-timer end-time current-process)
    (set-process-state! current-process 'waiting)
    (save-process-and-call scheduler-continuation)))

;; Example

(define (make-clock clock-signal clock-frequency)
  (define clock-period (/ 1 clock-frequency))
  (define (clock-process)
    (let ([clock-half-period (/ clock-period 2)])
      (let cycle ()
        (displayln " --------- clock cycle")
        (signal-set! clock-signal 0)
        (wait-interval clock-half-period)
        (signal-set! clock-signal 1)
        (wait-interval clock-half-period)
        (cycle))))
  clock-process)


(define (make-controller transition
                         input-sig output-sig
                         current-state-sig
                         next-state-sig)
  (lambda ()
    (let loop ()
      ;;; TODO Events on both current-state and input should wake the process
      ;;; The wait-on primitive needs to be modified for this case
      (let*-values ([(current-state) (wait-on current-state-sig)]
                    [(input) (signal-current-value input-sig)]
                    [(next-state output) (transition input current-state)])
        (printf "controller: ~a -> ~a (~a)\n" current-state next-state output)
        (signal-set! next-state-sig next-state)
        (signal-set! output-sig output)
        (loop)))))

(define (make-state-reg clock-sig reset-sig
                        current-state-sig next-state-sig)
  (lambda ()
    (let loop ()
      (when (= 1 (wait-on clock-sig))
        (signal-set! current-state-sig
                     (if (= 1 (signal-current-value reset-sig))
                         (begin (displayln "state-reg: reset") 'off)
                         (begin (let ([value (signal-current-value next-state-sig)])
                                  (printf "stored state: ~a\n" value)
                                  value)))))
      (loop))))

(define (make-monitor output-prefix . signals)
  (lambda ()
    (let loop ()
      (printf "~a~a\n" output-prefix (wait-on . signals))
      (loop))))

(define (test)
  (let ([input (add-signal "input" 0)]
        [output (add-signal "output" 0)]
        [current-state (add-signal "current-state" 'off)]
        [next-state (add-signal "next-state" 'off)]
        [clock (add-signal "clock" 0)]
        [reset (add-signal "reset" 0)]

        [transition-func (lambda (input current-state)
                           (case current-state
                             [(off) (values 'A 1)]
                             [(A)   (values 'B 1)]
                             [(B)   (values 'C 1)]
                             [else  (values 'off 0)]))])
    (run-system
     (list (make-clock clock 2)
           (make-state-reg clock reset current-state next-state)
           (make-controller transition-func input output
                            current-state next-state)
           (make-monitor "output current-state: " output current-state)))))
