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

;; Building parse tree for next 2 functions

;; Structure of the parseTree (Node)
(define-struct node (lhs rhs lstKids) #:transparent)

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



;; Build symbol table from parse tree

(define sy '("wain"))

(define (dcl t symt)
  (if (equal? (length (node-rhs (first (node-lstKids t)))) 2)
      (if (not (member (list (string-append "int* " (first (node-rhs (second (node-lstKids t)))))) symt))
          (set! sy (append sy (list (string-append "int* " (first (node-rhs (second (node-lstKids t))))))))
          (display "ERROR:duplicate decl\n" [current-error-port]))
      (if (not (member (list (string-append "int " (first (node-rhs (second (node-lstKids t)))))) symt))
          (set! sy (append sy (list (string-append "int " (first (node-rhs (second (node-lstKids t))))))))
          (display "ERROR:duplicate decl\n" [current-error-port]))))

(define (helper lst symt)
  (cond
    [(empty? lst) (void)]
    [else
     (helper (rest lst) (append symt (list (make-symt (first lst) symt))))]))

(define (make-symt t symt)
  (cond
    [(not (equal? (node-lhs t) "dcl"))
     (helper (node-lstKids t) symt)]
    [(equal? (node-lhs t) "dcl") (dcl t symt)]))
      
(define (has-dup? lst bool)
  (cond
    [(empty? lst) bool]
    [(not (not (member (first lst) (rest lst)))) true]
    [else
     (has-dup? (rest lst) bool)]))

(define (lst-index lst e pos)
  (cond
    [(empty? lst) pos]
    [else
     (if (equal? e (first lst)) pos
         (lst-index (rest lst) e (add1 pos)))]))

(define (modst st loc)
  (cond
    [(empty? st) empty]
    [else
     (cons (list (first st) loc) (modst (rest st) (- loc 4)))]))


(define (retnode2 t lhs rhs)
  (cond
    [(and (equal? (node-lhs t) lhs) (equal? (node-rhs t) rhs)) t]
    [else
     (map (lambda(x) (retnode2 x lhs rhs)) (node-lstKids t))]))

(define (retnode1 t lhs)
  (cond
    [(equal? (node-lhs t) lhs) t]
    [else
     (map (lambda(x) (retnode1 x lhs)) (node-lstKids t))]))

(define (code-statement t stmod lst)
  (cond
    [(equal? (node-rhs (first lst)) '("PRINTLN" "LPAREN" "expr" "RPAREN" "SEMI"))
     (code-expr t stmod (third (node-lstKids (first lst))))
     (display "add $1, $3, $0\n")
     (display "lis $10\n")
     (display ".word print\n")
     (display "jalr $10\n")]))

(define (code-statements t stmod lst)
  (cond
    [(equal? (node-rhs (first lst)) '())
     (code-expr t stmod (flatten (retnode1 t "expr")))]
    [(equal? (node-rhs (first lst)) '("statements" "statement"))
     (code-statements t stmod (second (node-lstKids (first lst))))
     (code-statement t stmod (first (node-lstKids (first lst))))]))

(define (code-term t stmod lst)
  (cond
    [(equal? (node-rhs (first lst)) '("factor"))
     (factor-code t stmod (node-lstKids (first lst)))]
    [(equal? (node-rhs (first lst)) '("term" "STAR" "factor"))
     (code-term t stmod (list (first (node-lstKids (first lst)))))
     (push "$3")
     (factor-code t stmod (list (third (node-lstKids (first lst)))))
     (pop "$5")
     (display "mult $3, $5\nmflo $3\n")]
    [(equal? (node-rhs (first lst)) '("term" "SLASH" "factor"))
     (code-term t stmod (list (first (node-lstKids (first lst)))))
     (push "$3")
     (factor-code t stmod (list (third (node-lstKids (first lst)))))
     (pop "$5")
     (display "div $5, $3\nmflo $3\n")]
    [(equal? (node-rhs (first lst)) '("term" "PCT" "factor"))
     (code-term t stmod (list (first (node-lstKids (first lst)))))
     (push "$3")
     (factor-code t stmod (list (third (node-lstKids (first lst)))))
     (pop "$5")
     (display "div $5, $3\nmfhi $3\n")]))

(define (push id)
  (display (string-append "sw " id ", -4($30)\nsub $30,$30,$4\n")))

(define (pop popinto)
  (display (string-append "lw " popinto ", 0($30)\nadd $30,$30,$4\n")))

(define (code-expr t stmod lst)
  (cond
    [(equal? (node-rhs (first lst)) '("term"))
     (code-term t stmod (node-lstKids (first lst)))]
    [(equal? (node-rhs (first lst)) '("expr" "PLUS" "term"))
     (code-expr t stmod (list (first (node-lstKids (first lst)))))
     (push "$3")
     (code-term t stmod (list (third (node-lstKids (first lst)))))
     (pop "$5")
     (display "add $3, $3, $5\n")]

    [(equal? (node-rhs (first lst)) '("expr" "MINUS" "term"))
     (code-expr t stmod (list (first (node-lstKids (first lst)))))
     (push "$3")
     (code-term t stmod (list (third (node-lstKids (first lst)))))
     (pop "$5")
     (display "sub $3, $5, $3\n")]))

(define (code-ID t stmod node)
  (cond
    [(equal? (first (node-rhs node)) (second (string-split (first (first stmod)))))
     (display "lw $3, 0($29)\n")]
    [(equal? (first (node-rhs node)) (second (string-split (first (second stmod)))))
     (display "lw $3, -4($29)\n")]
    [else
     (for-each (lambda(x) (when (equal? (second (string-split (first x))) (first (node-rhs node)))
                            (display (string-append "lw $3, " (number->string (second x)) "($29)\n"))))
               stmod)]))


(define (factor-code t stmod lst)
  (cond
    [(equal? (node-rhs (first lst)) '("ID"))
     (code-ID t stmod (first (node-lstKids (first lst))))]
    [(equal? (node-rhs (first lst)) '("LPAREN" "expr" "RPAREN"))
     (code-expr t stmod (list (second (node-lstKids (first lst)))))]
    [(equal? (node-rhs (first lst)) '("NUM"))
     (display (string-append "lis $3\n.word " (first (node-rhs (first (node-lstKids (first lst))))) "\n"))]))


(define (code stmod t regin)
  ;prolog/all var decls in body:
  ;(display ".import print\n")
  ;(push "$31")
  (display "lis $4\n.word 4\nsub $29,$30,$4\n")
  (for-each (lambda(x)
                  (display (string-append "sw $" (number->string (lst-index stmod x 1)) ", -4($30)\nsub $30,$30,$4\n"))) stmod)
  
  ;body:
  (display (string-append "sw $0, -4($30)\nsub $30,$30,$4\n"))
  (code-statements t stmod (flatten (retnode1 t "statements")))
  ;epilogue:
  ;restore $31
  ;(display "lw $31, 0($29)\n")
  (display "add $30,$29,$4\njr $31\n"))


;;
;; Main program
;;

;; T is a hash-table for terminals
(define T '#hash((0 . "ID") (1 . "NUM") (2 . "LPAREN") (3 . "RPAREN") (4 . "LBRACE") (5 . "RBRACE") (6 . "RETURN") (7 . "IF") (8 . "ELSE") (9 . "WHILE")
                            (10 . "PRINTLN") (11 . "WAIN") (12 . "BECOMES") (13 . "INT") (14 . "EQ") (15 . "NE") (16 . "LT") (17 . "GT") (18 . "LE") (19 . "GE") (20 . "PLUS")
                            (21 . "MINUS") (22 . "STAR") (23 . "SLASH") (24 . "PCT") (25 . "COMMA") (26 . "SEMI") (27 . "NEW") (28 . "DELETE") (29 . "LBRACK") (30 . "RBRACK")
                            (31 . "AMP") (32 . "NULL")))


;; N is a hash-table for nonterminals
(define N '#hash((0 . "procedures") (1 . "procedure") (2 . "main") (3 . "params") (4 . "paramlist") (5 . "type") (6 . "dcl") (7 . "dcls") (8 . "statements") (9 . "lvalue")
                                    (10 . "expr") (11 . "statement") (12 . "test") (13 . "term") (14 . "factor") (15 . "arglist")))

;(define readLines (readSeq '()))

(define t (build-tree))
;(display t)
;(display "\n")
(make-symt t '())
;; display the sy symbol table
(when (has-dup? sy false) (display "ERROR\n" [current-output-port]))

;; modify symbol table
(define stmod (modst (rest sy) 0))
;(display stmod)
;(display "\n")
;; display code
(code stmod t 1)