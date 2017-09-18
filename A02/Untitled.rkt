#lang racket

(define (be d)
    (char->integer d))

(display (be
          #\A) [current-output-port])