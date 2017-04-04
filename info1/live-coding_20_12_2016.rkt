;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname live-coding_20_12_2016) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ())))
; Filter definition analog zu Vorlesung
(: filter ((%a -> boolean) (list-of %a) -> (list-of %a)))
(define filter
  (lambda (p? xs)
    (cond ((empty? xs) empty)
          ((pair? xs)  (if (p? (first xs))
                           (make-pair (first xs)
                                      (filter p? (rest xs)))  
                           (filter p? (rest xs)))))))

; Gibt die Liste alle natürlichen Zahlen von a bis b
(: from-to (natural natural -> (list-of natural)))
(check-expect (from-to 2 4) (list 2 3 4))
(define from-to
  (lambda (a b)
    (letrec
        ((worker
          (lambda (a b acc)
            (if
             (> a b)
             acc
             (worker (+ a 1) b (append acc (list a)))))))
      (worker a b empty))))

; Addiert alle Zahlen von 1 bis n, die durch 3 teilbar sind
(: sum-dividable-by-3 (natural -> natural))
(check-expect (sum-dividable-by-3 6) 9)
(define sum-dividable-by-3
  (lambda (n)
    (let
        ((numbers (from-to 1 n)))
      (fold 0 +
             (filter (lambda (x)
                       (= (modulo x 3) 0)) numbers)))))

; Project Euler Problem 6

; Quadriert die gegebene Zahl
(: sqr (number -> number))
(check-expect (sqr 4) 16)
(define sqr
  (lambda (x)
    (* x x)))

; Berechnet die Difference der Summe der Quadrate der
; ersten n natürlichen Zahlen und dem Quadrat der Summe dieser
(: difference (natural -> natural))
(check-expect (difference 10) 2640)
(define difference
  (lambda (n)
    (let
        ((numbers (from-to 1 n)))
      (- (sqr (fold 0 + numbers))
         (fold 0 + (map sqr numbers))
         ))))

; Project Euler Problem 16
(: digit-sum (natural -> natural))
(check-expect (digit-sum 15) 26)
(define digit-sum
  (lambda (n)
    (fold 0 +
          (map (lambda (c) (string->number c))
                   (string->strings-list (number->string (expt 2 n)))))))