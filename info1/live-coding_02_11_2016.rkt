;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aktuelles Jahr
(: current-year natural)
(define current-year 2016)

; Errechnet das Alter in Abhängikeit des Geburtsjahres birth
(: get-age (natural -> natural))
(check-expect (get-age 1996) 20)
(check-expect (get-age 2000) 16)
(define get-age
  (lambda (birth)
    (- current-year birth)))

; Prüft Anhang des Geburtsjahres birth ob die Person volljährig ist
(: volljährig? (natural -> boolean))
(check-expect (volljährig? 1996) #t)
(check-expect (volljährig? 2000) #f)
(define volljährig?
  (lambda (birth)
    (>= (get-age birth) 18)))

; Berechnet das Geburtsjahr anhand des Alters age
(: get-birth-year (natural -> natural))
(check-expect (get-birth-year 20) 1996)
(check-expect (get-birth-year 2) 2014)
(define get-birth-year
  (lambda (age)
    (- current-year age)))

; Prüft ob eine Zahl num positiv ist
; 0 zählt zum positiven Bereich
(: positiv? (real -> boolean))
(check-expect (positiv? 10) #t)
(check-expect (positiv? -5) #f)
(define positiv?
  (lambda (num)
    (>= num 0)))

; Prüft ob eine Zahl num negativ ist
(: negativ? (real -> boolean))
(check-expect (negativ? 10) #f)
(check-expect (negativ? -5) #t)
(define negativ?
  (lambda (num)
    (not (positiv? num))))

; Gibt die n-te Fibonacci Zahl aus
(: nfib (natural -> natural))
(check-expect (nfib 0) 1)
(check-expect (nfib 1) 1)
(check-expect (nfib 2) 2)
(define nfib
  (lambda (n)
    (if (>= n 2)
        (+ (nfib (- n 1)) (nfib (- n 2)))
        1)))

; F(X) = 3/2 X - 5
(: fx (real -> real))
(check-within (fx 2) -2 0.1)
(define fx
  (lambda (x)
    (- (* (/ 3 2) x) 5)))