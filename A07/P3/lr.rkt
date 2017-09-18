#lang racket

;; cfgrl -- Convert CFG-R to CFG Format
;; 
;; Author:      Gordon V. Cormack
;; Revised by:  P. L. Ragde and Gordon V. Cormack
;; Version:     20081106.4
;;
;; Input:  .cfg file with single derivation
;; Output: equivalent .cfg file
;;
;; Usage:  mzscheme cfgrl.ss < file.cfg-r > file.cfg
;;

(require srfi/13)

;;
;; Helper functions
;;

;; read n symbols / lines into hash-table t -> keys begin from beginKey
(define (readsyms n t beginKey)
  (cond 
    [(zero? n) (void)]
    [else
     (hash-set! t beginKey (read-line))
     (readsyms (sub1 n) t (+ beginKey 1))]))


;; read single line containing integer
(define (readln)
  (string->number (read-line)))


;; Read the transitions lines and put all of them into a hash-table called t
(define (readTrans t numberOfTrans)
   (when
     (> numberOfTrans 0)
     (define line (read-line))
     (hash-set! t (string-append (first (string-split line)) " " (second (string-split line)))
                (string-append (third (string-split line)) " " (fourth (string-split line))))
     (readTrans t (- numberOfTrans 1))))


;; The program that reads in lines until EOF now and puts these lines as strings into a list called lst
(define (readTillEnd skipLines)
  (define line (read-line))
  (cond
    [(eof-object? line) empty]
    [else
     (cons line (readTillEnd 0))]))


;; If hash found, then print to output-port (takes in a listofkeys and a hash-table t)
(define (printOut t str r)
  (if (not (hash-has-key? t str)) (display "error\n")
      (cond
        [(member "reduce" (string-split (hash-ref t str)))
         (define str1 (string-append "reduce " (hash-ref r (string->number (second
                                                                            (string-split (hash-ref t str)))))))
         (display str1)
         (display "\n")]
        [(member "shift" (string-split (hash-ref t str)))
         (define str2 (hash-ref t str))
         (display str2)
         (display "\n")])))

;; Apply printOut to each in lst
(define (forEach t r lst)
  (for-each (lambda(x) (printOut t x r)) lst))


;;
;; Main program
;;

(define T (make-hash))                      ;; T is a hash-table for terminals
(define N (make-hash))                      ;; N is a hash-table for nonterminals
(define R (make-hash))                      ;; R is a hash-table for production rules
(define TransTable (make-hash))             ;; TransTable is a hash-table for second last part of Lr1 file (transitions)
(define n1 (string->number (read-line)))
(readsyms n1 T 0)                                                       ;; read terminals into hashtable T
(define n2 (string->number (read-line)))
(readsyms n2 N 0)                                                       ;; read nonterminals into hashtable N
(define S (read-line))                                                  ;; start symbol S has the first line NOW
(define n3 (string->number(read-line)))
(readsyms n3 R 0)                                                       ;; read production rules (as strings)
(define NumberOfStatesString (read-line))                               ;; line at hand is the one containing the number of states in the LR(0) Automaton
(define NumberOfTransString (read-line))                                ;; line at hand is the one containing the number of transitions or the "reduce"s functions
(readTrans TransTable (string->number NumberOfTransString))
(define inputList (readTillEnd (string->number NumberOfTransString)))
(forEach TransTable R inputList)                                        ;; print out everything as required




#|

(define (traverse t d)  ;; output leftmost derivation of tree t with indentation d
  (printf "~a~a\n" (make-string d #\space) (first t)) ;; print root
  (map (lambda (x) (traverse x (+ 1 d))) (rest t)))   ;; print all subtrees

(define (dump h)        ;; print keys in hashtable h in .cfg file format
  (printf "~a\n" (hash-count h))
  (hash-for-each h (lambda (x y) (printf "~a\n" x))))

(define (popper stack rhs lhs node) ;; pop rhs and accumulate subtrees, push new node
  (if (null? rhs) 
    (cons (cons lhs node) stack)    ;; done pops, push node = (rule . subtrees) 
    (popper (rest stack) (rest rhs) lhs (cons (first stack) node)))) ;; pop some more

(define (lr-do s)        ;; build tree from remaining input using stack s
  (define f (read-line))
  (define L (first (string-tokenize f)))                         ;; LHS symbol
  (define r (rest (string-tokenize f)))                          ;; RHS symbols
  (define n (filter (lambda (x) (hash-ref N x false)) r))  ;; remove terminals
  (define t (popper s (reverse n) f '()))                         ;; reduce rule
  (if (equal? S L) 
         (first (popper s (list S) f '()))    ;; special case reduce S' -> |- S -|
         (lr-do t)))                         ;; general case, continue

(define (lr) (lr-do '())) ;; wrapper function to read leftmost derivation, build tree


(define parsetree (lr))       ;; read reverse rightmost derivation into parsetree
(void (dump T))               ;; write terminals in .cfg format
(void (dump N))               ;; write nonterminals
(void (printf "~a\n" S))      ;; write start symbol
(void (dump R))               ;; write production rules
(void (traverse parsetree 0)) ;; write forward leftmost derivation
|#