(require net/http-easy)
(require json)
(require dotenv)

(dotenv-load!)

(define base-url (~a "http://192.168.1.186/api/" (getenv "HUE_SECRET") "/"))

(define light-ids '(|1| |2| |5| |7| |8|))

(define req/get
  (lambda (endpoint)
    (string->jsexpr
     (bytes->string/utf-8
      (response-body
       (get (~a base-url endpoint)))))))

(define req/put
  (lambda (endpoint value)
    (put (~a base-url endpoint)
         #:data value)))

(define lights
  (lambda ()
    (req/get "lights")))

(define light-on?
  (lambda (l id)
    (hash-ref
     (hash-ref
      (hash-ref l id)
      'state)
     'on)))

(define light-reachable?
  (lambda (l id)
    (hash-ref
     (hash-ref
      (hash-ref l id)
      'state)
     'reachable)))

(define all-lights-on?
  (lambda ()
    (letrec ({l (lights)}
             {alo? (lambda (lid)
                     (cond
                       [(null? lid) #t]
                       [else (and (light-on? l (car lid))
                                  (light-reachable? l (car lid))
                                  (alo? (cdr lid)))]))})
          (alo? light-ids))))

(define all-lights-off?
  (lambda ()
    (letrec ({l (lights)}
             {alo? (lambda (lid)
                     (cond
                       [(null? lid) #t]
                       [else (and (or (not (light-on? l (car lid)))
                                      (not (light-reachable? l (car lid))))
                                  (alo? (cdr lid)))]))})
          (alo? light-ids))))

(define light-off
  (lambda (light-id)
    (req/put (~a "lights/" light-id "/state")
             (jsexpr->string (hasheq 'on #f)))))

(define light-on
  (lambda (light-id)
    (req/put (~a "lights/" light-id "/state")
             (jsexpr->string (hasheq 'on #t)))))

(define all-lights-on
  (lambda ()
    (letrec ({alo (lambda (lid)
                    (cond
                      [(null? lid) '()]
                      [else (light-on (car lid))
			    (sleep 0.7)
                            (alo (cdr lid))]))})
      (alo light-ids))))

(define all-lights-off
  (lambda ()
    (letrec ({alo (lambda (lid)
                    (cond
                      [(null? lid) '()]
                      [else (light-off (car lid))
			    (sleep 1.0)
                            (alo (cdr lid))]))})
      (alo light-ids))))

(define synchronize-lights
  (lambda ()
    (letrec ({a (lambda ()
                  (cond
                    [(all-lights-on?) (sleep 10)
                                      (a)]
                    [else (all-lights-off)
                          (displayln "turning off")
                          (b)]))}
             {b (lambda ()
                  (cond
                    [(all-lights-off?) (sleep 2)
                                       (b)]
                    [else (all-lights-on)
                          (displayln "turning on")
                          (a)]))})
      (a))))
