;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-beginner-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm")))))
(define-record-procedures-parametric pair pair-of
    make-pair
    pair?
    (first
     rest))

(define list-of
  (lambda (t)
    (signature (mixed empty-list (pair-of t (list-of t))))))

(: nth-element (integer (list-of %a) ->  %a))
(check-expect (nth-element 3
                           (make-pair 1
                                      (make-pair 2
                                                 (make-pair 3 empty)))) 3)
(define nth-element
  (lambda (n ls)
    (cond
      ((= n 1) (first ls))
      (else
       (nth-element (- n 1) (rest ls))))))

(define birthyear
  (signature
   (predicate 
    (lambda (x)
      (and (> x 1900) (<= x 2016))))))

; Ein Mensch (human) besteht aus
; einem Namen (name)
; einem Geburstjahr (birth-year)
; einer Körpergröße (size)
(: make-human (string birthyear integer -> human))
(: human? (any -> boolean))
(: human-name (human -> string))
(: human-birth-year (human -> natural))
(: human-size (human -> integer))
(check-expect (human-name (make-human "Steffen" 1996 175)) "Steffen")
(check-expect (human-birth-year (make-human "Steffen" 1996 175)) 1996)
(check-expect (human-size (make-human "Steffen" 1996 175)) 175)
(check-expect (human? (make-human "Steffen" 1996 175)) #t)

(define-record-procedures human
  make-human
  human?
  (human-name
   human-birth-year
   human-size))

; Aktuelles jahr
(: current-year natural)
(define current-year 2016)

; Berechnet das Alter eines Menschen
(: human-age (human -> natural))
(check-expect (human-age (make-human "Steffen" 1996 175)) 20)
(define human-age
  (lambda (hm)
    (- current-year (human-birth-year hm))))

; Macht einen Menschen um 10 Jahr älter
(: human-aging (human integer -> human))
(check-expect (human-aging (make-human "Steffen" 1996 175) 10) (make-human "Steffen" 1986 175))
(define human-aging
  (lambda (human years)
    (make-human
     (human-name human)
     (- (human-birth-year human) years)
     (human-size human))))
               