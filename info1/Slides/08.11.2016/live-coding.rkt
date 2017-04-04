;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
; Vergleiche die Zahl a ob sie größer, kleiner oder gleich 0 ist
(: compare (real -> String))
(check-expect (compare 5) "a ist größer 0")
(check-expect (compare -5) "a ist kleiner 0")
(check-expect (compare 0) "a ist gleich 0")
(define compare
  (lambda (a)
    (cond ((< a 0) "a ist kleiner 0")
          ((> a 0) "a ist größer 0")
          (else "a ist gleich 0"))))

; Vergleiche die Zahl a ob sie keliner gleich 0 oder größer ist
(: compare1 (real -> String))
(check-expect (compare1 -5) "a ist kleiner gleich 0")
(check-expect (compare1 5) "a ist größer 0")
(define compare1
  (lambda (a)
    (if (<= a 0)
        "a ist kleiner gleich 0"
        "a ist größer 0")))

; Helper funktion für pow
(: pow-helper (real real real -> real))
(check-expect (pow 2 2) 4)
(check-expect (pow 2 10) 1024)
(define pow-helper
  (lambda (x y acc)
    (if (= 0 y) acc
        (pow-helper x (- y 1) (* 2 acc)))))

; Berechnet x^y
; endrekursiv
(: pow (real real -> real))
(check-expect (pow 2 2) 4)
(check-expect (pow 2 10) 1024)
(define pow
  (lambda (x y)
    (pow-helper x y 1)))

; Berchnet x^y nicht endrekursiv
(: pow2 (real real -> real))
(check-expect (pow2 2 2) 4)
(check-expect (pow2 2 10) 1024)
(define pow2
  (lambda (x y)
    (if (= 1 y) x
        (* x (pow2 x (- y 1))))))

; Berchnet die Summe von 1 bis n über die Gaußformel (kleiner Gauß)
(: sum (real -> real))
(check-expect (sum 3) 6)
(define sum
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

; Sum2 helper um Ergebnis aufzubauen
; Später werden solche Funktionen mtitels letrec überflüssig
(: sum2-helper (real real -> real))
(check-expect (sum2-helper 3 0) 6)
(define sum2-helper
  (lambda (n acc)
    (if (= n 0) acc
        (sum2-helper (- n 1) (+ n acc)))))

; Berechnet die Summe von 1 bis n über Rekursion
; Endrekursiv
(: sum2 (real -> real))
(check-expect (sum2 3) 6)
(define sum2
  (lambda (n)
    (sum2-helper n 0)))

; Berechnet die Summe von 1 bis n über Rekursion ohne Helper funktion
; Nicht endrekursiv
(: sum3 (real -> real))
(check-expect (sum3 3) 6)
(define sum3
  (lambda (n)
    (if (= n 1) n
        (+ n (sum3 (- n 1))))))

; Implementiert die logische Funktion Implikation von a -> b
; Es gilt a -> b = (not a) or b
(: imply (boolean boolean -> boolean))
(check-expect (imply #t #f) #f)
(check-expect (imply #f #t) #t)
(check-expect (imply #t #t) #t)
(check-expect (imply #f #f) #t)
(define imply
  (lambda (a b)
    (or (not a) b)))

; Implementiert die logische Funktion XOR
; Es gilt a XOR b = (a and (not b)) or ((not a) and b))
(: xor (boolean boolean -> boolean))
(check-expect (xor #t #t) #f)
(check-expect (xor #t #f) #t)
(check-expect (xor #f #f) #f)
(check-expect (xor #f #t) #t)
(define xor
  (lambda (a b)
    (or (and a (not b)) (and (not a) b))))