(defpackage :ugo
  (:use :cl))

(in-package :ugo)

(defparameter *size* 20)
(defparameter *dot*  '·)

(defparameter *white* '●)
(defparameter *black* '○)

(defparameter *board*
  (make-array `(,*size* ,*size*)
              :initial-element *dot*))

(defun setb (a b c)
  (setf (aref *board* a b) c))

(setb 0 0 0)
(setb 0 1 1)
(setb 0 2 2)
(setb 0 3 3)
(setb 0 4 4)
(setb 0 5 5)
(setb 0 6 6)
(setb 0 7 7)
(setb 0 8 8)
(setb 0 9 9)
(setb 0 10 'a)
(setb 0 11 'b)
(setb 0 12 'c)
(setb 0 13 'd)
(setb 0 14 'e)
(setb 0 15 'f)
(setb 0 16 'g)
(setb 0 17 'h)
(setb 0 18 'i)
(setb 0 19 'j)

(setb 1 0 1)
(setb 2 0 2)
(setb 3 0 3)
(setb 4 0 4)
(setb 5 0 5)
(setb 6 0 6)
(setb 7 0 7)
(setb 8 0 8)
(setb 9 0 9)
(setb 10 0 'a)
(setb 11 0 'b)
(setb 12 0 'c)
(setb 13 0 'd)
(setb 14 0 'e)
(setb 15 0 'f)
(setb 16 0 'g)
(setb 17 0 'h)
(setb 18 0 'i)
(setb 19 0 'j)

(defparameter transformation-table
  '((1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . 7)
    (8 . 8)
    (9 . 9)
    (a . 10)
    (b . 11)
    (c . 12)
    (d . 13)
    (e . 14)
    (f . 15)
    (g . 16)
    (h . 17)
    (i . 18)
    (j . 19)))

(defun transformation (index)
  (dolist (record transformation-table)
    (if (eq index (car record))
        (return (cdr record)))))

(defun move (element x y)
  (setf (aref *board* y x) element)
  *board*)

(defmacro w (x y)
  `(move *white*
         (transformation ',x)
         (transformation ',y)))

(defmacro b (x y)
  `(move *black*
         (transformation ',x)
         (transformation ',y)))

(defmacro d (x y)
  `(move *dot*
         (transformation ',x)
         (transformation ',y)))

*board*