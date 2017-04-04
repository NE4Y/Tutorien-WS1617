;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname live-coding) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "image2.rkt" "teachpack" "deinprogramm")))))
; Ein Auto (auto) besteht aus
; - Einem Model (model)
; - Einer Anzahl an ps (ps)
; - Einem Spitznamen (spitzname)
(: make-auto (string natural string -> auto))
(: auto-model (auto -> string))
(: auto-ps (auto -> natural))
(: auto-spitzname (auto -> string))
(check-property
 (for-all ((m string)
           (p natural)
           (s string))
   (and
    (= (auto-ps (make-auto m p s)) p)
    (string=? (auto-model (make-auto m p s)) m)
    (string=? (auto-spitzname (make-auto m p s)) s)
    (auto? (make-auto m p s)))))
(define-record-procedures auto
  make-auto
  auto?
  (auto-model
   auto-ps
   auto-spitzname))