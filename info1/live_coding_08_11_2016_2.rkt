;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname live_coding_08_11_2016_2) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
(: imply (boolean boolean -> boolean))
(check-expect (imply #f #f) #t)
(check-expect (imply #t #f) #f)
(define imply
  (lambda (a b)
    (or (not a) b)))

(: xor (boolean boolean -> boolean))
(check-expect (xor #t #t) #f)
(check-expect (xor #t #f) #t)
(check-expect (xor #f #t) #t)
(check-expect (xor #f #f) #f)
(define xor
  (lambda (a b)
    (or
     (and a (not b))
     (and (not a) b))))

(: pow (real natural -> real))
(check-within (pow 2 10) 1024 0.1)
(check-within (pow 4 2) 16 0.1)
(define pow
  (lambda (x y)
    (expt x y)))

(: pow2 (real natural -> real))
(check-within (pow2 2 10) 1024 0.1)
(check-within (pow2 2 0) 1 0.1)
(check-within (pow2 4 2) 16 0.1)
(define pow2
  (lambda (x y)
    (cond
      ((= y 1) x)
      ((= y 0) 1)
      (else (* x (pow2 x (- y 1)))))))

(: sum (natural -> natural))
(check-expect (sum 3) 6)
(define sum
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

(: sum2 (natural -> natural))
(check-expect (sum2 3) 6)
(define sum2
  (lambda (n)
    (if (= n 0) 0
        (+ n (sum2 (- n 1))))))

; (+ 1 (+ 2 (+ 3 (+ 4 ...))))

(: sum3 (natural -> natural))
(define sum3
  (lambda (n)
    (letrec
        ((worker
          (lambda (n acc)
            (if (= n 0) acc
                (worker (- n 1) (+ n acc))))))
      (worker n 0))))
