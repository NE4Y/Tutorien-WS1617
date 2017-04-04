;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname btree-parser) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Binärbäume

; -------- TOOLS ------

; ---------------------

; Ein Knoten (node) besitzt
; - einen linken Zweig (left-branch),
; - eine Markierung (label) und
; - einen rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch  ((node-of %a %b %c) -> %a))
(: node-label        ((node-of %a %b %c) -> %b))
(: node-right-branch ((node-of %a %b %c) -> %c))

(define-record-procedures-parametric node node-of
  make-node
  node?
  (node-left-branch
   node-label
   node-right-branch))

; Ein leerer Baum (empty-tree) besitzt
; keine weiteren Eigenschaften
(: make-empty-tree (-> empty-tree))

(define-record-procedures empty-tree
  make-empty-tree
  empty-tree?
  ())

; Der leere Baum 
(: the-empty-tree empty-tree)
(define the-empty-tree (make-empty-tree))

; Vertrag für Binärbäume (btree-of v) mit Markierungen des Vertrags v
; (im linken/rechten Zweig jedes Knoten findet sich jeweils wieder
; ein Binärbaum)
(define btree-of
  (lambda (v)
    (signature (mixed empty-tree
                     (node-of (btree-of v) v (btree-of v))))))

; Konstruiere ein Blatt mit Markierung x 
(: make-leaf (%a -> (btree-of %a)))
(define make-leaf
  (lambda (x)
    (make-node the-empty-tree x the-empty-tree)))

; Filtert eine Liste gegeben entsprechendem Prädikat
(: filter ((%a -> %b) (list %a) -> (list %b)))

(define filter
  (lambda (p xs)
    (cond
      ((empty? xs) empty)
      ((pair? xs)
       (if (p (first xs))
           (make-pair (first xs) (filter p (rest xs)))
           (filter p (rest xs)))))))

(define-record-procedures-parametric parse parse-of
  make-parse
  parse?
  (parse-input
   parse-output))

; Liest eine Liste von Zeichen ein und konstruiert den entsprechenden Baum
(: parser ((list string) -> (parse-of (list string) (btree-of string))))
(define parser
  (lambda (cs)
    (cond ((string=? (first cs) "_") 
           (make-parse (rest cs) the-empty-tree))
          ((string=? (first cs) "(")
           (let* ((lt (parser (rest cs)))
                  (rt (parser (rest (parse-input lt)))))             
             (make-parse (rest (parse-input rt))
                         (make-node (parse-output lt)
                                    (first (parse-input lt))
                                    (parse-output rt)))))
          (else                        
           (violation "parse error")))))

; Wrapper für parse
; Zerlegt den String in eine Liste von Zeichen und entfernt zudem noch alle
; Leerzeichen
(: btree-parse (string -> (btree-of string)))

(check-expect (btree-parse "_") the-empty-tree)
(check-expect (btree-parse "(_1_)") (make-leaf "1"))
(check-expect (btree-parse "((_1_)3(_  2_))") (make-node
                                               (make-leaf "1")
                                               "3"
                                               (make-leaf "2")))
(check-expect (btree-parse "(_ 1(  _2  (_  3_)))") (make-node
                                                    the-empty-tree
                                                    "1"
                                                    (make-node
                                                     the-empty-tree
                                                     "2"
                                                     (make-leaf "3"))))

(define btree-parse
  (lambda (s)
    (parse-output
     (parser
      (filter (lambda (x) (not (string=? x " "))) 
              (string->strings-list s))))))
