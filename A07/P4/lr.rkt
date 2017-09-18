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
;; Definitions and all helpers for Stack wrapped around list
;;

#|(struct Stack (nameOfStack listOfContents) #:mutable #:transparent)

(define (top stack)
  (make-Stack stack (first (Stack-listOfContents stack))))

(define (pop stack)
  (make-Stack stack (rest (Stack-listOfContents stack))))

(define (push stack thing)
  (make-Stack stack (append (list thing) (Stack-listOfContents stack))))

(define (stack-length stack)
  (length (Stack-listOfContents stack)))
|#

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


;; Function to read, parse and update stacks and everything from input
(define (readExp lstStringsReadLine symbolStack stateStack TransTable prodRules fromReduce fromReduceString printList wordsRead S)
  (cond
    [(empty? lstStringsReadLine)
     (for-each (lambda(x) (printf "~a\n" x)) printList)
     (define outputLastLine (string-append S " " (string-join (reverse symbolStack)) "\n"))
     (display outputLastLine [current-output-port])]
    [(and (hash-has-key? TransTable (string-append (number->string (first stateStack)) " " (first lstStringsReadLine))) (not fromReduce))
      (cond
        [(equal? (first (string-split (hash-ref TransTable (string-append (number->string (first stateStack)) " " (first lstStringsReadLine))))) "shift")
         (shiftCase lstStringsReadLine symbolStack stateStack TransTable prodRules
                    (string-append (number->string (first stateStack)) " " (first lstStringsReadLine)) fromReduce printList wordsRead S)]              ;; keyToLookup
        [(equal? (first (string-split (hash-ref TransTable (string-append (number->string (first stateStack)) " " (first lstStringsReadLine))))) "reduce")
         (reduceCase lstStringsReadLine symbolStack stateStack TransTable prodRules
                     (string-append (number->string (first stateStack)) " " (first lstStringsReadLine)) printList wordsRead S)])]                      ;; keyToLookup
    [(and (hash-has-key? TransTable (string-append (number->string (first stateStack)) " " fromReduceString)) fromReduce)
     (cond
        [(equal? (first (string-split (hash-ref TransTable (string-append (number->string (first stateStack)) " " fromReduceString)))) "shift")
         (shiftCase lstStringsReadLine symbolStack stateStack TransTable prodRules
                    (string-append (number->string (first stateStack)) " " fromReduceString) fromReduce printList wordsRead S)]                        ;; keyToLookup
        [(equal? (first (string-split (hash-ref TransTable (string-append (number->string (first stateStack)) " " fromReduceString)))) "reduce")
         (reduceCase lstStringsReadLine symbolStack stateStack TransTable prodRules
                     (string-append (number->string (first stateStack)) " " fromReduceString) printList wordsRead S)])]                                ;; keyToLookup
    [else
     (display (string-append "ERROR at " (number->string (+ wordsRead 1)) "\n") [current-error-port])]))

;; shift function
(define (shiftCase lstStringsReadLine symbolStack stateStack TransTable prodRules keyToLookup fromReduce printList wordsRead S)
  (define valFromTransTable (hash-ref TransTable keyToLookup))
  (if (not fromReduce)
      (readExp (rest lstStringsReadLine) (append (list (second (string-split keyToLookup))) symbolStack)
               (append (list (string->number (second (string-split valFromTransTable)))) stateStack) TransTable prodRules false "" printList (add1 wordsRead) S)
      (readExp lstStringsReadLine (append (list (second (string-split keyToLookup))) symbolStack)
               (append (list (string->number (second (string-split valFromTransTable)))) stateStack) TransTable prodRules false "" printList wordsRead S)))

;; reduce function
(define (reduceCase lstStringsReadLine symbolStack stateStack TransTable prodRules keyToLookup printList wordsRead S)
  (define valFromTransTable (hash-ref TransTable keyToLookup))
  (define prodRule (hash-ref prodRules (string->number (second (string-split valFromTransTable)))))         ; eg: expr expr - term
  (define lenToPop (- (length (string-split prodRule)) 1))
  (readExp lstStringsReadLine (list-tail symbolStack (- (length symbolStack) (- (length symbolStack) lenToPop)))
           (list-tail stateStack (- (length stateStack) (- (length stateStack) lenToPop))) TransTable
           prodRules true (first (string-split prodRule)) (append printList (list prodRule)) wordsRead S))

;; Reads in the sequence to be used for P4
(define (readSeq str)
  (define line (read-line))
  (cond
    [(eof-object? line) str]
    [else
     (string-append str " " (readSeq line))]))

;;
;; Main program
;;

(define T (make-hash))                      ;; T is a hash-table for terminals
(define N (make-hash))                      ;; N is a hash-table for nonterminals
(define R (make-hash))                      ;; R is a hash-table for production rules
(define TransTable (make-hash))             ;; TransTable is a hash-table for second last part of Lr1 file (transitions)
(define symbolStack '())
(define stateStack '(0))

(define n1 (string->number (read-line)))
(readsyms n1 T 0)                                                       ;; read terminals into hashtable T
(define n2 (string->number (read-line)))
(readsyms n2 N 0)                                                       ;; read nonterminals into hashtable N
(define S (read-line))                                                  ;; start symbol S has the first line NOW
(define n3 (string->number(read-line)))
(readsyms n3 R 0)                                                       ;; read production rules (as strings)
(define NumberOfStatesString (read-line))                               ;; line at hand is the one containing the number of states in the LR(0) Automaton
(define NumberOfTransString (read-line))                                ;; line at hand is the one containing the number of transitions or the "reduce"s functions
(readTrans TransTable (string->number NumberOfTransString))             ;; make a TransTable (hash table) of all the transitions

(define readLineString (readSeq ""))                                   ;; BOF id - ( id ) - id EOF (for example; the last line of the program)
(define lstStringsReadLine (string-split readLineString))               ;; list of strings from the readInput: '("BOF", "id", "-", "(",........)

; for P4
(readExp lstStringsReadLine symbolStack stateStack TransTable R false "" '() 0 S)

; for P3
;(define inputList (readTillEnd (string->number NumberOfTransString)))   ;; inputList is list of last 3 ....which we now have to generate
;(forEach TransTable R inputList)                                        ;; apply printOut func to each in inputList to print out everything as required




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