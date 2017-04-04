;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-advanced-reader.ss" "deinprogramm")((modname examples) (read-case-sensitive #f) (teachpacks ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #t #t none datum #f ((lib "image2.rkt" "teachpack" "deinprogramm") (lib "universe.rkt" "teachpack" "deinprogramm")))))
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

; Der leere Baum (the-empty-tree) besitzt
; keine weiteren Eigenschaften
(: make-empty-tree (-> the-empty-tree))

(define-record-procedures the-empty-tree
  make-empty-tree
  empty-tree?
  ())

; Der leere Baum 
(: empty-tree the-empty-tree)
(define empty-tree (make-empty-tree))

; Signatur für Binärbäume (btree-of t) mit Markierungen der Signatur t
; (im linken/rechten Zweig jedes Knotens findet sich jeweils wieder
; ein Binärbaum)
(define btree-of
  (lambda (t)
    (signature (mixed the-empty-tree
                      (node-of (btree-of t) t (btree-of t))))))
;                              \__________/   \__________/
;                                  ↑               ↑
;                                 zweifache Rekursion, s. (list-of t)


 ; Konstruiere ein Blatt mit Markierung x 
  (: make-leaf (%a -> (btree-of %a)))
  (define make-leaf
    (lambda (x)
      (make-node empty-tree x empty-tree)))


; Binärer Suchbaum
(: t1 (btree-of natural))
(define t1
  (make-node
   (make-node
    (make-leaf 3)
    4
    (make-leaf 5))
   9
   (make-node
    (make-leaf 6)
    7
    (make-leaf 8))))

; checks whether a label is represented in a binary tree using
; binary search
(: inTree? (natural (btree-of natural) -> boolean))
(check-expect (inTree? 10 t1) #f)
(check-expect (inTree? 3 t1) #t)
(define inTree?
  (lambda (n t)
    (cond
      ((empty-tree? t) #f)
      ((= (node-label t) n) #t)
      ((< n (node-label t))
       (inTree? n (node-left-branch t)))
      (else
       (inTree? n (node-right-branch t))))))


 ; Falte Baum t bzgl. z und f
  (: btree-fold (%b (%b %a %b -> %b) (btree-of %a) -> %b))
  (define btree-fold
    (lambda (z f t)
      (cond ((empty-tree? t) 
             z)
            ((node? t)
             (f (btree-fold z f (node-left-branch t))
                (node-label t)
                (btree-fold z f (node-right-branch t)))))))

(: inTree2? (natural (btree-of natural) -> boolean))
(check-expect (inTree2? 10 t1) #f)
(check-expect (inTree2? 3 t1) #t)
(define inTree2?
  (lambda (n t)
    (btree-fold
     #f
     (lambda (l x r)
       (or l (= x n) r))
     t)))
       
    
              
