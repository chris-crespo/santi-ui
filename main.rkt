#lang racket 

(require racket/gui/base)
(require racket/generic)
(require racket/match)

(provide divide)

(define lst (list 8 6 2 2 1))

(define (sum lst) (apply + lst))
(define (divide lst)
  (let helper ([l1 (list (car lst))] [l2 (cdr lst)])
    (let ([s1 (sum l1)] [s2 (sum l2)])
      (if (or (null? l2) (>= (+ s1 (car l2)) s2))
          (values (reverse l1) l2)
          (helper (cons (car l2) l1) (cdr l2))))))

(define-generics tree
  (tree-value tree))

(struct leaf (value)
  #:methods gen:tree
  [(define (tree-value leaf) (leaf-value leaf))])

(struct node (value left right)
  #:methods gen:tree
  [(define (tree-value node) (node-value node))])

(define/match (list->tree lst)
  [((list)) (leaf 0)]
  [((list x)) (leaf x)]
  [(_) 
   (define-values (left right) (divide lst))
   (define left-tree (list->tree left))
   (define right-tree (list->tree right))
   (node (+ (tree-value left-tree) (tree-value right-tree))
         left-tree right-tree)])

(define (paint canvas dc)
  (send dc set-brush "blue" 'solid)
  (send dc set-pen "white" 2 'solid)
  (draw-tree (list->tree (sort lst >)) dc))

(define (draw-tree tree dc)
  (define-values (width height) (send frame get-client-size))
  (let helper ([tree tree] [width width] [height height] [x 0] [y 0])
    (match tree
      [(leaf _) 
       (send dc draw-rectangle x y width height)]
      [(node value left right) 
       (define left-size (/ (tree-value left) value))
       (define right-size (/ (tree-value right) value))
       (cond [(> height width)
              (helper left width (* height left-size) x y)
              (helper right width (* height right-size) 
                      x (+ y (* height left-size)))]
             [else 
              (helper left (* width left-size) height x y)
              (helper right (* width right-size) height
                      (+ x (* width left-size)) y)])])))

(define frame (new frame% [label "Santi"] [width 300] [height 300]))
(new canvas% [parent frame] [paint-callback paint])
(send frame show #t)
