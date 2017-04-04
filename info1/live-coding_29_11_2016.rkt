;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #t #t none explicit #f ())))
; Paardefinition aus der Vorlesung
(: make-pair (%a %b -> (pair-of %a %b)))
(: pair? (any -> boolean))
(: first ((pair-of %a %b) -> %a))
(: rest ((pair-of %a %b) -> %b))
(define-record-procedures-parametric pair pair-of
  make-pair
  pair?
  (first
   rest))

(define list-of
    (lambda (t)
      (signature (mixed empty-list
                        (pair-of t (list-of t))))))

; Bildet die Summe einer Liste ls
(: list-sum ((list-of number) -> number))
(check-expect (list-sum (make-pair 1 (make-pair 3 empty))) 4)
(check-expect (list-sum empty) 0)
(define list-sum
  (lambda (ls)
    (if (empty? ls)
        0
        (+ (first ls) (list-sum (rest ls))))))

; Addiert die geraden Elemente einer Liste
(: list-even-sum ((list-of number) -> number))
(check-expect (list-even-sum (make-pair 1 (make-pair 3 empty))) 0)
(check-expect (list-even-sum (make-pair 2 (make-pair 3 (make-pair 4 empty)))) 6)
(define list-even-sum
  (lambda (ls)
    (cond
      ((empty? ls) 0)
      ((= (modulo (first ls) 2) 0)
       (+ (first ls) (list-even-sum (rest ls))))
      (else
       (+ 0 (list-even-sum (rest ls)))))))

; Addiert jedes zweite Element einer Liste
(: list-each-second-sum ((list-of number) -> number))
(check-expect (list-each-second-sum (make-pair 1 (make-pair 2 empty))) 2)
(check-expect (list-each-second-sum (make-pair 1 (make-pair 1 (make-pair 3 (make-pair 1 empty))))) 2)
(define list-each-second-sum
  (lambda (ls)
    (cond
      ((empty? ls) 0)
      ((empty? (rest ls)) 0)
      (else
       (+ (first (rest ls)) (list-each-second-sum (rest (rest ls))))))))

; Berechnet die Länge einer Liste
(: list-length ((list-of number) -> number))
(check-expect (list-length empty) 0)
(check-expect (list-length (make-pair 1 (make-pair 2 empty))) 2)
(define list-length
  (lambda (ls)
    (if (empty? ls)
        0
        (+ 1 (list-length (rest ls))))))

; Berechnet den Erwartungswert einer Liste
(: list-mean ((list-of number) -> number))
(check-within (list-mean (make-pair 1 (make-pair 2 empty))) 1.5 0.1)
(check-expect (list-mean empty) 0)
(define list-mean
  (lambda (ls)
    (if (empty? ls)
        0
        (/ (list-sum ls) (list-length ls)))))

; Quadriert die Elemente einer Liste und addiert sie
(: list-quadrat-sum ((list-of number) -> number))
(check-expect (list-quadrat-sum (make-pair 1 (make-pair 1 empty))) 2)
(check-expect (list-quadrat-sum (make-pair 1 (make-pair 2 empty))) 5)
(define list-quadrat-sum
  (lambda (ls)
    (if (empty? ls)
        0
        (+ (expt (first ls) 2) (list-quadrat-sum (rest ls))))))

; Helper prozedur für die Varianz
(: list-varianz-helper ((list-of number) number -> number))
(check-expect (list-varianz-helper (make-pair 1 (make-pair 1 empty)) 1) 0)
(define list-varianz-helper
  (lambda (ls mean)
    (if (empty? ls)
        0
        (+
         (expt (- (first ls) mean) 2)
         (list-varianz-helper (rest ls) mean)))))

; Berechnet die Varianz
(: list-varianz ((list-of number) -> number))
(check-expect (list-varianz (make-pair 1 (make-pair 1 empty))) 0)
(check-expect (list-varianz (make-pair 1 empty)) 0)
(define list-varianz
  (lambda (ls)
    (if (= (list-length ls) 1)
        0
        (* (/ (- (list-length ls) 1)) (list-varianz-helper ls (list-mean ls))))))

; Teilt jedes Element einer Liste durch n
(: list-divide-n (number (list-of number) -> (list-of number)))
(check-expect (list-divide-n 2 (make-pair 4 (make-pair 8 empty))) (make-pair 2 (make-pair 4 empty)))
(define list-divide-n
  (lambda (n ls)
    (if (empty? ls)
        empty
        (make-pair
         (/ (first ls) n)
         (list-divide-n n (rest ls))))))



