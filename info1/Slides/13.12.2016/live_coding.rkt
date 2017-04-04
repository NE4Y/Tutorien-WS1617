;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname live_coding) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm")))))
; Project euler problem 1
; Multiples of 3 and 5
; Sum of all multiples of 3 or 5 below 1000
(: multiples-of-3-5 (natural -> natural))
(check-expect (multiples-of-3-5 10) 23)
(define multiples-of-3-5
  (lambda (n)
    (letrec
        ((worker
          (lambda (n acc)
            (cond
              ((= n 0) acc)
              ((or (= (modulo n 5) 0) (= (modulo n 3) 0))
               (worker (- n 1) (+ acc n)))
              (else
               (worker (- n 1) acc))))))
      (worker (- n 1) 0))))

; Project euler problem 2
; Even fibonacci numbers
; Sum of even-valued terms, where single value does not exceed
; four million
(: get-nth-element (natural (list-of natural) -> natural))
(check-expect (get-nth-element 0 (list 0 1)) 0)
(define get-nth-element
  (lambda (n ls)
    (cond
      ((and (pair? ls) (= n 0)) (first ls))
      ((and (not (pair? ls)) (= n 0)) (violation "Dieses Element existiert nicht"))
      (else
       (get-nth-element (- n 1) (rest ls))))))

; performant fib implementation
(: fib (natural -> natural))
(check-expect (fib 5) 5)
(check-expect (fib 0) 0)
(define fib
  (lambda (n)
    (letrec
        ((worker
          (lambda (n s acc)
            (cond
              ((< n s) (get-nth-element n acc))
              (else
               (worker n (+ s 1)
                          (append acc
                                  (list (+ (get-nth-element (- s 1) acc)
                                           (get-nth-element (- s 2) acc)))))
                       )))))
      (worker n 2 (list 0 1)))))

; Calculate even-valued sum of fibonacci
; which do not exceed upperBound
(define even-fib-sum
  (lambda (upperBound)
    (letrec
        ((worker
          (lambda (n upperBound acc)
            (let
                ((fib-value (fib n)))
              (if
                (<= fib-value upperBound)
                (if (even? fib-value)
                     (worker (+ n 1) upperBound (+ acc fib-value))
                     (worker (+ n 1) upperBound acc))
                acc)))))
      (worker 0 upperBound 0))))

; Problem 3
; Largest prime factor
; checks whether a number n is prime
(: prime? (natural -> boolean))
(check-expect (prime? 2) #t)
(check-expect (prime? 3) #t)
(check-expect (prime? 17) #t)
(check-expect (prime? 28) #f)
(define prime?
  (lambda (n)
    (letrec
        ((worker
          (lambda (n s)
            (cond
              ((> s (sqrt n)) #t)
              ((= (modulo n s) 0) #f)
              (else
               (worker n (+ s 1)))))))
      (worker n 2))))

; get the next prime number after number n
(: get-next-prime (natural -> natural))
(check-expect (get-next-prime 2) 3)
(check-expect (get-next-prime 29) 31)
(check-expect (get-next-prime 307) 311)
(check-expect (get-next-prime 523) 541)
(define get-next-prime
  (lambda (n)
    (if
     (prime? (+ n 1))
     (+ n 1)
     (get-next-prime (+ n 1)))))

; get prime factors
(: get-largest-prime-factor (natural -> natural))
(check-expect (get-largest-prime-factor 13195) 29)
(define get-largest-prime-factor
  (lambda (n)
    (letrec
        ((worker
          (lambda (n s acc)
            (if (= n 1)
                acc
                (if
                 (= (modulo n s) 0)
                 (worker (/ n s) s s)
                 (worker n (get-next-prime s) acc))))))
      (worker n 2 1))))
                
                 




              