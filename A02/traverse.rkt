#lang Racket
(require compatibility/mlist)

;;
;; ***************************************************
;; Rishabh Malhotra (20609328)
;; CS 241 Winter 2017
;; Assignment 03, Problem 1
;; ****************************************************
;;


; key for each Node is the actual number in the tree at that node.
; val for each Node is the number of children it has.
; lstOfChildren is the list of all its children (from left to right)
; in other words, a list of nodes.
(define-struct node (key val lstOfChildren) #:transparent)

; lst_of_inputs is the list of inputs obtained after redirecting user input.
; read-all-input takes in the redirected output of scan-input and forms it into

(define (read-from-file file)
 (let ((p (open-input-file file)))
    (let f ((lst_of_inputs (read p)))
      (if (eof-object? lst_of_inputs)
        (begin
          (close-input-port p)
          '())
        (cons lst_of_inputs (f (read p)))))))

;(read-from-file "input.txt")


; Put the list of lists (of Nodes) inside lst_of_inputs
;(define lst_of_inputs empty)
;(set! lst_of_inputs (read-from-file "input.txt"))

#|
; make_Tree uses the lst_of_inputs and constructs the corresponding valid
; non-empty tree.

(define (make-tree input_list)
  (cond
    [(empty? input_list) empty]
    [(= (second input_list) 0) (list (make-node (first input_list)
                                          (second input_list)
                                          empty)
                                     (helper (make-node (first input_list)
                                          (second input_list)
                                          empty) input_list))]
                                          ;(make-tree (rest (rest input_list))))]
    [(list (make-node (first input_list) (second input_list)
                (make-tree (rest (rest input_list)))))]))
    

(define (helper node list)
  (cons node empty))

(make-tree (read-from-file "input1.txt"))
|#

(define l1 (5 0))
(define l2 (6 0))
(define l3 (2 2))
(define l4 (7 0))
(define l5 (3 1))
(define l6 (8 0))
(define l7 (9 0))
(define l8 (10 0))
(define l9 (12 0))
(define l10 (11 1))
(define l11 (4 4))
(define l12 (1 3))

(display l1 [current-output-port])
(display "\n" [current-output-port])
(display l2 [current-output-port])
(display "\n" [current-output-port])
(display l3 [current-output-port])
(display "\n" [current-output-port])
(display l4 [current-output-port])
(display "\n" [current-output-port])
(display l5 [current-output-port])
(display "\n" [current-output-port])
(display l6 [current-output-port])
(display "\n" [current-output-port])
(display l7 [current-output-port])
(display "\n" [current-output-port])
(display l8 [current-output-port])
(display "\n" [current-output-port])
(display l9 [current-output-port])
(display "\n" [current-output-port])
(display l10 [current-output-port])
(display "\n" [current-output-port])
(display l11 [current-output-port])
(display "\n" [current-output-port])
(display l12 [current-output-port])
(display "\n" [current-output-port])