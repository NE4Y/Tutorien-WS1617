;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname live-coding-2) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "image2.rkt" "teachpack" "deinprogramm")))))
; Ein Rechteck (rectangle) besteht aus
; einer Seitenlänge a (length-a)
; einer Seitenlänge b (length-b)
(: make-rectangle (real real -> rectangle))
(: rectangle-length-a (rectangle -> real))
(: rectangle-length-b (rectangle -> real))
(: rectangle? (any -> boolean))
(check-property
 (for-all ((a real)
           (b real))
   (and (= (rectangle-length-a (make-rectangle a b)) a)
        (= (rectangle-length-b (make-rectangle a b)) b)
        (rectangle? (make-rectangle a b)))))
(define-record-procedures rectangle
  make-rectangle
  rectangle?
  (rectangle-length-a
   rectangle-length-b))

; Berechnet die Fläche eines Rechtecks rect
(: rectangle-area (rectangle -> real))
(check-expect (rectangle-area (make-rectangle 5 2)) 10)
(check-expect (rectangle-area (make-rectangle 0 0 )) 0)
(define rectangle-area
  (lambda (rect)
    (* (rectangle-length-a rect) (rectangle-length-b rect))))

; Berechnet den Umfang eines Rechtecks rect
(: rectangle-perimeter (rectangle -> real))
(check-expect (rectangle-perimeter (make-rectangle 5 2)) 14)
(check-expect (rectangle-perimeter (make-rectangle 0 0)) 0)
(define rectangle-perimeter
  (lambda (rect)
    (+
     (* 2 (rectangle-length-a rect))
     (* 2 (rectangle-length-b rect)))))

; Prüft ob es sich bei einem Rechteck rect um ein Quadrat handelt
(: rectangle-quadrat? (rectangle -> boolean))
(check-expect (rectangle-quadrat? (make-rectangle 5 2)) #f)
(check-expect (rectangle-quadrat? (make-rectangle 2 2)) #t)
(define rectangle-quadrat?
  (lambda (rect)
    (= (rectangle-length-a rect) (rectangle-length-b rect))))

; Berechnet die Diagonale eines Rechtecks rect
(: rectangle-diagonal (rectangle -> real))
(check-expect (rectangle-diagonal (make-rectangle 0 0)) 0)
(check-within (rectangle-diagonal (make-rectangle 9 3)) 9.486 0.01)
(define rectangle-diagonal
  (lambda (rect)
    (sqrt
     (+ 
      (expt (rectangle-length-a rect) 2)
      (expt (rectangle-length-b rect) 2)))))