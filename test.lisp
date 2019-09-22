(defpackage #:wadler-pprint-tests
  (:use #:cl #:fiveam #:wadler-pprint)
  (:export #:wadler-pprint-suite))

(in-package #:wadler-pprint-tests)

(def-suite wadler-pprint-suite
  :description "The tests for cl-wadler-pprint.")

(in-suite wadler-pprint-suite)

(defparameter +tree+
  '("aaa" ("bbbbb" ("ccc") ("dd")) ("eee") ("ffff" ("gg") ("hhh") ("ii"))))

(defparameter +tree-pretty-0+
  "aaa[bbbbb[ccc, dd], eee, ffff[gg, hhh, ii]]")

(defparameter +tree-pretty-1+
  "aaa[bbbbb[ccc, dd],
    eee,
    ffff[gg, hhh, ii]]")

(defun pp-tree-1 (tree)
  (destructuring-bind (s . ts) tree
    (group (list (text s)
                 (nest (length s) (pp-bracket-tree-1 ts))))))

(defun pp-bracket-tree-1 (list)
  (when list
    (list (text "[")
          (nest 1 (pp-trees-1 list))
          (text "]"))))

(defun pp-trees-1 (list)
  (if (null (cdr list))
      (pp-tree-1 (car list))
      (list (pp-tree-1 (car list))
            (text ",")
            +newline+
            (pp-trees-1 (cdr list)))))

(defparameter +tree-pretty-2+
  "aaa[
  bbbbb[
    ccc,
    dd
  ],
  eee,
  ffff[
    gg,
    hhh,
    ii
  ]
]")

(defun pp-tree-2 (tree)
  (destructuring-bind (s . ts) tree
    (list (text s) (pp-bracket-tree-2 ts))))

(defun pp-bracket-tree-2 (list)
  (when list
    (list (text "[")
          (nest 2 (list +newline+ (pp-trees-2 list)))
          +newline+
          (text "]"))))

(defun pp-trees-2 (list)
  (if (null (cdr list))
      (pp-tree-2 (car list))
      (list (pp-tree-2 (car list))
            (text ",")
            +newline+
            (pp-trees-2 (cdr list)))))

(def-test tree-1 ()
  (is (equal +tree-pretty-1+
             (pretty nil (pp-tree-1 +tree+) :width 30))))

(def-test tree-1-no-width ()
  (is (equal +tree-pretty-0+ (pretty nil (pp-tree-1 +tree+)))))

(def-test tree-2 ()
  (is (equal +tree-pretty-2+
             (pretty nil (pp-tree-2 +tree+) :width 30))))

(def-test tree-2-no-width ()
  ; The same still, since there aren't any groups present.
  (is (equal +tree-pretty-2+ (pretty nil (pp-tree-2 +tree+)))))
