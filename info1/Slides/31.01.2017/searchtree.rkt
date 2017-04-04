;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname searchtree) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "image2.rkt" "teachpack" "deinprogramm")))))
; Aufgabe: Suchbäume

; ------- Prelude -------

(require racket/base)

; Binärbäume

; Ein Knoten (node) besteht aus
; - einem linken Zweig (left-branch)
; - einer Markierung (label) und
; - einem rechten Zweig (right-branch)
(: make-node (%a %b %c -> (node-of %a %b %c)))
(: node-left-branch ((node-of %a %b %c) -> %a))
(: node-label ((node-of %a %b %c) -> %a))
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

; Vetrag für Binärbäume (btree-of v) mit Markierungen des Vetrages v
; (im linken/rechten Zweig jedes Knotens findet sich jeweils wieder ein
; Binärbaum)
(define btree-of
  (lambda (v)
    (signature (mixed empty-tree
                      (node-of (btree-of v) v (btree-of v))))))

; Konstruiere ein Blatt mit Markierung x
(: make-leaf (%a -> (btree-of %a)))
(define make-leaf
  (lambda (x)
    (make-node the-empty-tree x the-empty-tree)))

; Falte Baum t bzgl. z und c
(: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))
(define btree-fold
  (lambda (z c t)
    (cond ((empty-tree? t) 
           z)
          ((node? t)
           (c (btree-fold z c (node-left-branch t))
              (node-label t)
              (btree-fold z c (node-right-branch t)))))))

; ---- Prelude End -------

; Exemplarischer Suchbaum
(: t1 (btree-of integer))
(define t1
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 3))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 20))))

; Kaputter Suchbaum (-4 < 1 und rechts(1) = -4)
(: t2 (btree-of integer))
(define t2
  (make-node (make-node (make-leaf 10)
                        1
                        (make-leaf -4))
             12
             (make-node (make-leaf 13)
                        16
                        (make-leaf 200))))

; Kaputter Suchbaum (10 doppelt)
(: t3 (btree-of integer))
(define t3
  (make-node (make-node (make-leaf -4)
                        1
                        (make-leaf 10))
             12
             (make-node (make-leaf 10)
                        16
                        (make-leaf 200))))



; a)

(: within-bounds? (real real (btree-of real) -> boolean))
(define within-bounds?
  (lambda (lo hi t)
    (if (empty-tree? t)
        #t
        (let ((root (node-label t)))
          (and (< lo root hi)
               (within-bounds? lo root (node-left-branch t))
               (within-bounds? root hi (node-right-branch t)))))))

; Ist t ein Suchbaum?
(: search-tree? ((btree-of real) -> boolean))

(check-expect (search-tree? t1) #t)
(check-expect (search-tree? t2) #f)
(check-expect (search-tree? t3) #f)

(define search-tree?
  (lambda (t)
    (within-bounds? -inf.0 +inf.0 t)))




; b)

; Enthält ein Suchbaum t die Markierung y?
(: searchtree-member? (integer (searchtree-of integer) -> boolean))

(check-expect (searchtree-member? 3 t1) #t)
(check-expect (searchtree-member? 15 t1) #f)

(define searchtree-member?
  (lambda (x t)
    (cond ((empty-tree? t) #f)
          ((= x (node-label t)) #t)
          ((< x (node-label t)) (searchtree-member? x (node-left-branch t)))
          ((> x (node-label t)) (searchtree-member? x (node-right-branch t))))))



; c)

(define searchtree-of
  (lambda (t)
    (signature (combined (btree-of t) (predicate search-tree?)))))

; Füge Markierung y in Suchbaum t ein
(: searchtree-insert (integer (searchtree-of integer) -> (searchtree-of integer)))
(define searchtree-insert
  (lambda (y t)
    (cond 
      ((empty-tree? t) (make-leaf y))
      ((node? t)
       (let
           ((x (node-label t)))
         (cond 
           ((= y x) t)
           ((< y x)
            (make-node (searchtree-insert y (node-left-branch t))
                       x
                       (node-right-branch t)))
           ((> y x)
            (make-node (node-left-branch t)
                       x
                       (searchtree-insert y (node-right-branch t))))))))))


; d)

; Füge die Liste der Markierungen xs in einen leeren Suchbaum ein
(: list->searchtree ((list-of integer) -> (searchtree-of integer)))
(define list->searchtree
  (lambda (xs)
    (fold the-empty-tree
          searchtree-insert
          xs)))



; e)

; Liefere den Knoten linksaußen im Baum t (oder empty, falls t leeer ist)
; Siehe Vorlesung.
(: leftmost ((btree-of integer) -> (list integer)))
(define leftmost
  (lambda (t)
    (btree-fold empty
                (lambda (l1 x l2)
                  (if (empty? l1)  ;  <--  ◯x ≡ l1
                      (list x)     ;      /  \
                      l1))         ;     ◯  △  
                t)))


; Service für searchtree-delete:
; Lösche die Wurzel des nicht-leeren Suchbaums t
(: searchtree-delete-root ((searchtree-of integer) -> (searchtree-of integer)))
(define searchtree-delete-root
  (lambda (t)
    (let ((l (node-left-branch t))
          (r (node-right-branch t)))
      (cond ((empty-tree? r) l)                                  ; ➌a
            ((node? r)                                           ; ➌b
             ; r ist nicht-leer => 
             ; (leftmost r) liefert nicht-leere Liste
             (let ((z (first (leftmost r))))
               (make-node l
                          z
                          (searchtree-delete z r))))))))


; Lösche Markierung y aus Suchbaum t
(: searchtree-delete (integer (searchtree-of integer) -> (searchtree-of integer)))

(check-expect (searchtree-delete 42 the-empty-tree) the-empty-tree)
(check-expect (searchtree-delete 42 (make-leaf 42)) the-empty-tree)
(check-expect (searchtree-member? 12 (searchtree-delete 12 t1)) #f)

(define searchtree-delete
  (lambda (y t)
    (cond 
      ((empty-tree? t) the-empty-tree)
      ((node? t)
       (let ((x (node-label t)))
         (cond 
           ((= y x)                   
            (searchtree-delete-root t))             
           ((< y x)                   
            (make-node (searchtree-delete y (node-left-branch t))
                       x
                       (node-right-branch t)))
           ((> y x)                   
            (make-node (node-left-branch t)
                       x
                       (searchtree-delete y (node-right-branch t))))))))))

