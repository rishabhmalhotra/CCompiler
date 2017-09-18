#lang racket

;; Building parse tree for next 2 functions

;; Structure of the parseTree (Node)
(define-struct node (lhs rhs lstKids) #:transparent)

(define (retnode1 t lhs)
  (cond
    [(equal? (node-lhs t) lhs) t]
    [else
     (map (lambda(x) (retnode1 x lhs)) (node-lstKids t))]))

(define (build-tree)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [(equal? (first (string-split line)) "BOF")
     (make-node "BOF" "BOF" '())]
    [(equal? (first (string-split line)) "EOF")
     (make-node "EOF" "EOF" '())]
    [(not (member (first (string-split line)) (hash-values T)))
     (make-node (first (string-split line)) (rest (string-split line))
                (map (lambda(x) (build-tree)) (rest (string-split line))))]
    [else
     (make-node (first (string-split line)) (rest (string-split line)) '())]))

;; T is a hash-table for terminals
(define T '#hash((0 . "ID") (1 . "NUM") (2 . "LPAREN") (3 . "RPAREN") (4 . "LBRACE") (5 . "RBRACE") (6 . "RETURN") (7 . "IF") (8 . "ELSE") (9 . "WHILE")
                            (10 . "PRINTLN") (11 . "WAIN") (12 . "BECOMES") (13 . "INT") (14 . "EQ") (15 . "NE") (16 . "LT") (17 . "GT") (18 . "LE") (19 . "GE") (20 . "PLUS")
                            (21 . "MINUS") (22 . "STAR") (23 . "SLASH") (24 . "PCT") (25 . "COMMA") (26 . "SEMI") (27 . "NEW") (28 . "DELETE") (29 . "LBRACK") (30 . "RBRACK")
                            (31 . "AMP") (32 . "NULL")))


;; N is a hash-table for nonterminals
(define N '#hash((0 . "procedures") (1 . "procedure") (2 . "main") (3 . "params") (4 . "paramlist") (5 . "type") (6 . "dcl") (7 . "dcls") (8 . "statements") (9 . "lvalue")
                                    (10 . "expr") (11 . "statement") (12 . "test") (13 . "term") (14 . "factor") (15 . "arglist")))


(define t (build-tree))
(display t)
(display "\n")
(flatten (retnode1 t "statements"))