#lang racket

;; scan is the main function provided, which uses the data definitions
;; and helper functions that follow

(define (scan str)
  (scan-func str asmtrlst 'start asmfinal))


;; scan-func: (listof char) trans-table symbol (listof symbol) -> (listof token)

(define (scan-func str trans start final)
  (scan-acc (string->list str) trans start final empty empty))


;; Next we specify the data definitions for tokens and the various components
;; of an FSM.


(define-struct token (kind lexeme) #:transparent)
;; A token is a (make-token k l), where k is a symbol
;;  and l is (union (list char) int).

(define-struct transition (state charset next) #:transparent)
;; A transition table is a list of transitions.
;; A transition is a (make-transition s cs ns), where s and ns are symbols,
;;  and cs is a function char->boolean that tests whether the transition applies.


;; The sample FSM provided is defined by (asmtrlst, 'start, asmfinal).
;; Definitions of asmtrlst and asmfinal follow.

;; functions used in defining sample transition table

(define (num? ch)
  (and (char<=? #\1 ch) (char<=? ch #\9)))

(define (strings? ch)
  (char-alphabetic? ch))
  #|(or
   (and (char<=? #\a ch) (char<=? ch #\f))
   (and (char<=? #\A ch) (char<=? ch #\F))))|#

(define (chartest ch)
  (lambda (x) (char=? x ch)))

(define (braces? ch)
  (or
   (char=? ch #\()
   (char=? ch #\))
   (char=? ch #\[)
   (char=? ch #\])
   (char=? ch #\{)
   (char=? ch #\})))

(define (slash? ch)
  (char=? ch #\/))

(define (math? ch)
  (or
   (char=? ch #\-)
   (char=? ch #\+)
   (char=? ch #\*)
   (char=? ch #\/)
   (char=? ch #\%)))

(define (symbols? ch)
  (or
   (char=? ch #\;)
   (char=? ch #\,)
   (char=? ch #\&)))

(define (isNot? ch)
  (char=? ch #\!))

(define (comparison? ch)
  (or
   (char=? ch #\<)
   (char=? ch #\>)))

(define (isEqual? ch)
  (char=? ch #\=))

(define (isZero? ch)
  (char=? ch #\0))


;; sample transition table

(define asmtrlst
  (list
   (make-transition 'start isZero? 'zero)
   (make-transition 'start char-whitespace? 'whitespace)
   (make-transition 'start num? 'num)
   (make-transition 'num num? 'num)
   (make-transition 'start strings? 'strings)
   (make-transition 'strings strings? 'strings)
   (make-transition 'start braces? 'braces)
   (make-transition 'start math? 'math)
   (make-transition 'start symbols? 'symbols)
   (make-transition 'start isNot? 'not)
   (make-transition 'not isEqual? 'comparisonTwo)
   (make-transition 'start comparison? 'comparisonOne)
   (make-transition 'start isEqual? 'equal)
   (make-transition 'equal isEqual? 'comparisonTwo)
   (make-transition 'comparisonOne isEqual? 'comparisonTwo)
   ;; handle comments:
   (make-transition 'start slash? 'slash)
   (make-transition 'slash slash? 'comment)                 
   ))

;; sample list of final states

(define asmfinal
  (list
    'zero
    'strings
    'braces
    'num
    'comparisonOne
    'comparisonTwo
    'equal
    'math
    'symbols
    'whitespace
    'comment
    ))

;; scan-acc is the main workhorse of the lexer. It uses accumulative recursion
;; to run the FSM specified by (trans, state, final) on the list of characters cl.
;; acc accumulates the characters of the current token in reverse order, and
;; tacc accumulates the token list in reverse order.

;; scan-acc: (listof char) trans-table symbol (listof symbol) (listof char) (listof token) -> (listof token)

(define (scan-acc cl trans state final acc tacc)
  (cond
    [(empty? cl)
       (if (member state final)
           (if (or (symbol=? state 'whitespace) (symbol=? state 'comment))
               (reverse tacc)
               (reverse (cons (finalize-token state (reverse acc)) tacc)))
           (error 'ERROR "unexpected end of string\n"))]
    [else
      (let ([trl (memf (lambda (x) (found-trans? state (first cl) x)) trans)])
        (cond
          [(and (boolean? trl) (member state final))
             (if (symbol=? state 'whitespace)
                 (scan-acc cl trans 'start final empty tacc)
                 (scan-acc cl trans 'start final empty (cons (finalize-token state (reverse acc)) tacc)))]
          [(boolean? trl)
             (error 'ERROR "left to parse:~a ~a\n" state (list->string cl))]
          [(symbol=? state 'comment)
             (reverse tacc)]
          [else
             (scan-acc (rest cl) trans (transition-next (first trl)) final (cons (first cl) acc) tacc)]))]))

;; helper functions for scan-acc

(define (found-trans? state ch tr)
  (and (symbol=? state (transition-state tr))
       ((transition-charset tr) ch)))

;; finalize-token symbol (listof char) -> token
(define (finalize-token state l)
  (cond
    ;[(symbol=? state 'comment) (make-token 'comment 0)]
    [(symbol=? state 'num) (make-token 'INT (check-int-range (list->number l)))]
    [(symbol=? state 'zero) (make-token 'INT 0)]
    [(symbol=? state 'strings)
     (cond
       [(equal? (list->string l) "return") (make-token 'RETURN (list->string l))]
       [(equal? (list->string l) "int") (make-token 'INT (list->string l))]
       [(equal? (list->string l) "NULL") (make-token 'NULL (list->string l))]
       [(equal? (list->string l) "new") (make-token 'NEW (list->string l))]
       [(equal? (list->string l) "delete") (make-token 'DELETE (list->string l))]
       [(equal? (list->string l) "if") (make-token 'IF (list->string l))]
       [(equal? (list->string l) "else") (make-token 'ELSE (list->string l))]
       [(equal? (list->string l) "while") (make-token 'WHILE (list->string l))]
       [(equal? (list->string l) "println") (make-token 'PRINTLN (list->string l))]
       [(equal? (list->string l) "wain") (make-token 'WAIN (list->string l))]
       [else (make-token 'ID (list->string l))])]
    [(symbol=? state 'braces)
     (cond
       [(equal? (first l) #\() (make-token 'LPAREN (first l))]
       [(equal? (first l) #\)) (make-token 'RPAREN (first l))]
       [(equal? (first l) #\{) (make-token 'LBRACE (first l))]
       [(equal? (first l) #\}) (make-token 'RBRACE (first l))]
       [(equal? (first l) #\[) (make-token 'LBRACK (first l))]
       [(equal? (first l) #\]) (make-token 'RBRACK (first l))]
       [else (error 'ERROR "incorrect bracket(s) spec")])]
    [(symbol=? state 'comaprisonOne)
     (cond
       [(and (equal? (length l) 1) (equal? (first l) #\<)) (make-token 'LT (first l))]
       [(and (equal? (length l) 1) (equal? (first l) #\>)) (make-token 'GT (first l))]
       [else (error 'ERROR "incorrect comaprison spec")])]
    [(symbol=? state 'comparisonTwo)
     (cond
       [(and (equal? (length l) 2) (equal? (first l) #\<)
             (equal? (second l) #\=)) (make-token 'LE (list->string l))]
       [(and (equal? (length l) 2) (equal? (first l) #\>)
             (equal? (second l) #\=)) (make-token 'GE (list->string l))]
       [(and (equal? (length l) 2) (equal? (first l) #\=)
             (equal? (second l) #\=)) (make-token 'EQ (list->string l))]
       [(and (equal? (length l) 2) (equal? (first l) #\!)
             (equal? (second l) #\=)) (make-token 'NE (list->string l))])]
    [(symbol=? state 'equal) (make-token 'BECOMES (first l))]
    [(symbol=? state 'math)
     (cond
       [(equal? (first l) #\+) (make-token 'PLUS (first l))]
       [(equal? (first l) #\-) (make-token 'MINUS (first l))]
       [(equal? (first l) #\*) (make-token 'STAR (first l))]
       [(equal? (first l) #\/) (make-token 'SLASH (first l))]
       [(equal? (first l) #\%) (make-token 'PCT (first l))])]
    [(symbol=? state 'symbols)
     (cond
       [(equal? (first l) #\,) (make-token 'COMMA (first l))]
       [(equal? (first l) #\;) (make-token 'SEMI (first l))]
       [(equal? (first l) #\&) (make-token 'AMP (first l))])]
    [else (error 'ERROR "incorrect input spec")]))


;; helper functions for finalize-token

(define (list->number lst) (string->number (list->string lst)))

; returns index by given element 'e' for a list 'lst':
(define (element-index e lst)
    (cond [(eqv? e (car lst)) 0]
          [else (+ (element-index e (cdr lst)) 1)]))


(define (check-int-range n)
  (cond
    [(<= -2147483648 n 4294967295) n]
    [else (error 'ERROR "integer out of range: ~a" n)]))

(define print? true)

; This file just uses scan to tokenize each line of the input
(define (scan-input)
  (define line (read-line))
  (cond
    [(eof-object? line) (void)]
    [(> (string-length line) 0) ; Ignore blank lines
        ; Ignore comment-only lines as well
        ; When a comment-only line is scanned, an empty struct is returned
        (define scanned (scan line))
        (cond
          [(empty? scanned) (scan-input)]
          [else (map (lambda(x) ;(when (not (equal? (token-kind x) 'comment))
                                  ;[(and print? (equal? (token-lexeme x) #\/) (equal? (token-lexeme (list-ref scanned (add1 (element-index x scanned)))) #\/)) (set! print? false)]
                                   
                                   (display (token-kind x) [current-output-port])
                                   (display " " [current-output-port])
                                   (display (token-lexeme x) [current-output-port])
                                   (display "\n" [current-output-port]))
                                  ;[(and (not print?) (equal? (token-lexeme x) #\\) (equal? (token-lexeme (list-ref scanned (add1 (element-index x scanned)))) #\n)) (set! print? true)]))
                     scanned)
                ;(display (token-kind (last scanned)) [current-output-port])
                ;(display " " [current-output-port])
                ;(display (token-lexeme (last scanned)) [current-output-port])
                ;(display "\n" [current-output-port])
                (scan-input)])]

    [else (scan-input)]))

(scan-input)
