#lang racket

(require srfi/13)

;;
;; Helper functions
;;


;; Reads in the sequence (redirected input) to be used
(define (readSeq lst)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [else
     (append lst (readSeq (list (string-split line))))]))


;;
;; Core functions
;;

;; Build symbol table without parse-tree
(define (make-symt lst decl? symbol table track nomore?)
  (cond
    [(not nomore?) (display "ERROR\n" [current-error-port])]
    [(empty? lst) ;(when (not nomore?) (display "ERROR\n" [current-error-port]))]
     (cond
       [nomore? table]
        ;(for-each (lambda(x) (display x [current-error-port])
         ;            (display "\n" [current-error-port])) table)]
       [else
        (display "ERROR\n" [current-error-port])])]
    [decl?
     (cond
       [(equal? (first lst) '("type" "INT")) (make-symt (rest (rest lst)) true (string-append symbol " int") table track true)]
       [(equal? (first lst) '("type" "INT" "STAR")) (make-symt (rest (rest (rest lst))) true (string-append symbol " int*") table track true)]
       [(equal? (first (first lst)) "ID")
        (cond
          [(empty? (filter (lambda(x) (equal? x (second (first lst)))) track))
           (make-symt (rest lst) false ""
                      (append table (list (string-append (second (first lst)) symbol))) (append track (list (second (first lst)))) true)]
          [else
           (make-symt (rest lst) false ""
                      (append table (list (string-append (second (first lst)) symbol))) (append track (list (second (first lst)))) false)])])]
    
    [(equal? (first lst) '("dcl" "type" "ID")) (make-symt (rest lst) true symbol table track true)]
    
    [else
     (if (> (length lst) 1)
       (cond
         [(equal? (first (first (rest lst))) "dcl") (if (or (equal? (first (first lst)) "LPAREN") (equal? (first (first lst)) "dcls")
                                                            (equal? (first (first lst)) "LBRACE") (equal? (first (first lst)) "COMMA"))
                                                        (make-symt (rest lst) false "" table track true)
                                                        (make-symt (rest lst) false "" table track false))]
         [(equal? (first (first (rest lst))) "dcls") (if (or (equal? (first (first lst)) "LBRACE") (equal? (first (first lst)) "dcls"))
                                                         (make-symt (rest lst) false "" table track true)
                                                         (make-symt (rest lst) false "" table track false))]
         [(equal? (first lst) '("factor" "ID")) (if (empty? (filter (lambda(x) (equal? x (second (first (rest lst))))) track))
                                                    (make-symt (rest lst) false "" table track false)
                                                    (make-symt (rest lst) false "" table track true))]
         [else
          (make-symt (rest lst) false "" table track true)])
     (make-symt (rest lst) false "" table track true))]))


;; Type checking for symantics:

(define (type st lst)
  (cond
    [(and (equal? (first lst) '("RETURN" "return")) (equal? (third lst) '("expr" "expr" "MINUS" "term")))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
          "int")
         (when (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (first (list-tail lst 10))))) st))))
              "int*")
             (display "ERROR\n" [current-output-port]))
         (display "ERROR\n" [current-error-port]))]
    [(and (equal? (first lst) '("RETURN" "return")) (equal? (third lst) '("expr" "expr" "PLUS" "term")))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
          "int")
         (when (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (first (list-tail lst 10))))) st))))
              "int*")
             (display "ERROR\n" [current-output-port]))
         (display "ERROR\n" [current-error-port]))]
    [(and (equal? (first (first lst)) "ID") (equal? (first (second lst)) "BECOMES") (equal? (third lst) '("expr" "expr" "PLUS" "term")))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
          "int*")
         (if (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                      (second (first (list-tail lst 10))))) st))))
              "int*")
             (display "ERROR\n" [current-error-port])
             (when (equal?
                    (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (first lst)))) st))))
                    "int")
               (display "ERROR\n" [current-error-port])))
         (if (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                      (second (first (list-tail lst 10))))) st))))
              "int")
             (display "ERROR\n" [current-error-port])
             (when (equal?
                    (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (first lst)))) st))))
                    "int")
               (display "ERROR\n" [current-error-port]))))]
    [(and (equal? (first (first lst)) "ID") (equal? (first (second lst)) "BECOMES") (equal? (third lst) '("expr" "expr" "MINUS" "term")))
     (cond
       [(equal?
         (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
         "int")
        (if (equal?
             (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                     (second (first (list-tail lst 10))))) st))))
             "int*")
             (display "ERROR\n" [current-error-port])
             (when (equal?
                    (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                            (second (first lst)))) st))))
                    "int*")
               (display "ERROR\n" [current-error-port])))]
       [else
        (if (equal?
             (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                     (second (first (list-tail lst 10))))) st))))
             "int")
            (when (equal?
                   (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                           (second (first lst)))) st))))
                   "int")
              (display "ERROR\n" [current-error-port]))
            (when (equal?
                   (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                           (second (first lst)))) st))))
                   "int*")
              (display "ERROR\n" [current-error-port])))])]
    [(equal? (first lst) '("term" "term" "STAR" "factor"))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (fourth lst)))) st))))
          "int")
         (when (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
              "int*")
             (display "ERROR\n" [current-output-port]))
         (display "ERROR\n" [current-error-port]))]
    [(equal? (first lst) '("term" "term" "SLASH" "factor"))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (fourth lst)))) st))))
          "int")
         (when (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
              "int*")
             (display "ERROR\n" [current-output-port]))
         (display "ERROR\n" [current-error-port]))]
    [(equal? (third lst) '("term" "term" "PCT" "factor"))
     (if (equal?
          (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (fourth lst)))) st))))
          "int")
         (when (equal?
              (second (string-split (first (filter (lambda(x) (equal? (first (string-split x))
                                                                 (second (seventh lst)))) st))))
              "int*")
             (display "ERROR\n" [current-output-port]))
         (display "ERROR\n" [current-error-port]))]
    [else
     (type st (rest lst))]))
    
             
             
         


(define (find-initial-errors lst)                                                                 ; no wain / wain at wrong place (only wain allowed)
  (if (or (not (equal? (third lst) '("procedures" "main")))
          (not (equal? (fifth lst) '("INT" "int")))
          (not (equal? (sixth lst) '("WAIN" "wain"))))
      (display "ERROR: did not find proper definiton of WAIN" [current-error-port])
      (make-tree lst)))

;; Building parse table for next 2 functions

(define (read-many rhs lst)
  (when (and (not (empty? rhs)) (> (length lst) 1))
    (list (make-tree (rest lst)) (read-many (rest rhs) (rest lst)))))


;; Structure of the parseTree (Node)
;; (list lhs rhs rule listofKids)

(define (make-tree lst)
  (when (not (empty? lst))
    (if (> (length (first lst)) 1)
      (cond
        [(not (member (first (first lst)) (hash-values T)))                                             ; lhs is a non-Terminal
         (list (first (first lst)) (rest (first lst)) (read-many (rest (first lst)) lst))]
        [else                                                                                           ; lhs is the terminal
         (list (first (first lst)) (rest (first lst)) '())])
      (list (first (first lst))))))

;;
;; Main program
;;

(define T '#hash((0 . "ID") (1 . "NUM") (2 . "LPAREN") (3 . "RPAREN") (4 . "LBRACE") (5 . "RBRACE") (6 . "RETURN") (7 . "IF") (8 . "ELSE") (9 . "WHILE")
                            (10 . "PRINTLN") (11 . "WAIN") (12 . "BECOMES") (13 . "INT") (14 . "EQ") (15 . "NE") (16 . "LT") (17 . "GT") (18 . "LE") (19 . "GE") (20 . "PLUS")
                            (21 . "MINUS") (22 . "STAR") (23 . "SLASH") (24 . "PCT") (25 . "COMMA") (26 . "SEMI") (27 . "NEW") (28 . "DELETE") (29 . "LBRACK") (30 . "RBRACK")
                            (31 . "AMP") (32 . "NULL")))

;; T is a hash-table for terminals

(define N '#hash((0 . "procedures") (1 . "procedure") (2 . "main") (3 . "params") (4 . "paramlist") (5 . "type") (6 . "dcl") (7 . "dcls") (8 . "statements") (9 . "lvalue")
                                    (10 . "expr") (11 . "statement") (12 . "test") (13 . "term") (14 . "factor") (15 . "arglist")))

;; N is a hash-table for nonterminals

(define readLines (readSeq '()))

;(define parseTree (make-tree readLines))

;(find-initial-errors readLines)
(define symbolTable (make-symt readLines false "" '("wain") '() true))

(type symbolTable readLines)