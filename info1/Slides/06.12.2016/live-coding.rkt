;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm")))))
; Länge einer Liste
(: list-length ((list-of %a) -> natural))
(check-expect (list-length (list 1 2 3)) 3)
(check-expect (list-length (list 1)) 1)
(check-expect (list-length empty) 0)
(define list-length
  (lambda (ls)
    (letrec
        ((worker
          (lambda (ls acc)
            (cond
              ((empty? ls) acc)
              ((pair? ls)
               (worker (rest ls) (+ acc 1)))))))
      (worker ls 0))))

; Summe einer Liste ls
(: list-sum ((list-of number) -> number))
(check-expect (list-sum (list 1 2 3)) 6)
(check-expect (list-sum empty) 0)
(define list-sum
  (lambda (ls)
    (letrec
        ((worker
          (lambda (ls acc)
            (cond
              ((empty? ls) acc)
              ((pair? ls)
               (worker (rest ls) (+ (first ls) acc)))))))
      (worker ls 0))))


; Gibt jedes zweite Element zurück
(: list-second ((list-of %a) -> (list-of %a)))
(check-expect (list-second (list 1 2 3)) (list 2))
(check-expect (list-second (list 1 2 3 4 5)) (list 2 4))
(check-expect (list-second empty) empty)
(define list-second
  (lambda (ls)
    (letrec
        ((worker
          (lambda (ls acc)
            (cond
              ((empty? ls) acc)
              ((empty? (rest ls)) acc)
              (else
               (worker
                (rest (rest ls))
                (append  acc (list (first (rest ls))))))))))
      (worker ls empty))))

; Erstellt eine Liste von a bis b
(: a-to-b (natural natural -> (list-of natural)))
(check-expect (a-to-b 2 4) (list 2 3 4))
(define a-to-b
  (lambda (a b)
    (letrec
        ((worker
          (lambda (a b acc)
            (if
             (= a b) (append acc (list b))
             (worker (+ a 1) b (append acc (list a)))))))
      (worker a b empty))))


; Fügt die fehlenden Elemente einer Liste hinzu
(: append-missing ((list-of natural) -> (list-of natural)))
(check-expect (append-missing (list 1 2 5)) (list 1 2 3 4 5))
(check-expect (append-missing (list 1 3 4)) (list 1 2 3 4))
(check-expect (append-missing (list 1 2 4 6)) (list 1 2 3 4 5 6))
(check-expect (append-missing empty) empty)
(define append-missing
  (lambda (ls)
    (letrec
        ((worker
          (lambda (ls acc)
            (cond
              ((empty? ls)
               acc)
              ((empty? (rest ls))
               (append acc (list (first ls))))
              (else
               (if (= (+ 1 (first ls)) (first (rest ls)))
                   (worker (rest ls) (append acc (list (first ls))))
                   (worker (append (list (+ 1 (first ls))) (rest ls)) (append acc (list (first ls))))))))))
      (worker ls empty))))




