;; -*- coding: utf-8 -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2.3.1 따옴표연산
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x) x)
         (else (memq item (cdr x))))))
