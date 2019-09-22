(in-package #:wadler-pprint)

;;;
;;; Utilities
;;;

(defun join (list sep)
  (cdr (loop for x in list nconcing (list sep list))))

;;;
;;; The structs used to represent documents.
;;;

(defstruct nest
  (width (error "Must provide WIDTH") :type fixnum)
  (doc (error "Must provide DOC") :type doc))

(defstruct text
  (string (error "Must provide STRING") :type string))

(defconstant +newline+ '+newline+)

(defstruct union-doc
  (lhs (error "Must provide LHS") :type doc)
  (rhs (error "Must provide RHS") :type doc))

(defstruct flatten
  (doc (error "Must provide DOC") :type doc))

;;;
;;; Predicate and type for being a document.
;;;

(defun docp (doc)
  "Returns whether the given value is a document."
  ; TODO: Currently non-total on improper lists.
  (or (and (typep doc 'text)
           (not (find #\newline (text-string doc))))
      (eq doc '+newline+)
      (and (typep doc 'union-doc)
           (docp (union-doc-lhs doc))
           (docp (union-doc-rhs doc)))
      (and (typep doc 'flatten)
           (docp (flatten-doc doc)))
      (every #'docp doc)))

(deftype doc ()
  "An imprecise check for whether a value is a document."
  '(or text (eql '+newline+) union-doc flatten list))
 
;;;
;;; Creation of a document.
;;;

(defgeneric pretty-object (object)
  (:documentation "Converts an object to a document."))

(defun nest (width doc)
  (make-nest :width width :doc doc))

(defun text (str)
  (join
    (mapcar (lambda (str) (make-text :string str))
            (uiop:split-string str :separator #(#\newline)))
    +newline+))

(defun group (&rest doc)
  (make-union-doc
    :lhs (flatten doc)
    :rhs doc))

(defun flatten (doc)
  (cond
    ((null doc)         nil)
    ((consp doc)        (cons (flatten (car doc))
                              (flatten (cdr doc))))
    ((nest-p doc)       (make-nest :width (nest-width doc)
                                   :doc   (flatten (nest-doc doc))))
    ((text-p doc)       doc)
    ((eq doc +newline+) (make-text :string " "))
    ((union-doc-p doc)  (flatten (union-doc-lhs doc)))
    (t                  (error 'type-error :datum doc :expected-type 'doc))))

;;;
;;; Document construction helpers.
;;;

(defun bracket (l r &rest body)
  (group
    (list (text l)
          (nest 2 (cons +newline+ body))
          +newline+
          (text r))))

(defun spread (&rest docs)
  (join docs (text " ")))

(defun stack (&rest docs)
  (join docs +newline+))

;;;
;;; Layout of documents.
;;;

(defun best (width posn doc)
  (check-type doc doc)
  (be width posn (list (cons 0 doc))))

(defun be (width posn docs)
  (check-type width (or null fixnum))
  (check-type posn  fixnum)
  (check-type docs  list)
  ; TODO: Make this a loop; it's sufficiently tail-recursive-looking that it
  ; should be fairly possible.
  (when docs
    (destructuring-bind ((i . doc) . tl) docs
      (check-type i fixnum)
      (cond
        ((null doc)         (be width posn tl))
        ((consp doc)        (be width posn `((,i . ,(car doc))
                                             (,i . ,(cdr doc))
                                             ,@tl)))
        ((nest-p doc)       (be width posn (cons (cons (+ i (nest-width doc)) (nest-doc doc))
                                                 tl)))
        ((text-p doc)       (cons (text-string doc)
                                  (be width (+ posn (length (text-string doc))) tl)))
        ((eq doc +newline+) (cons i (be width i tl)))
        ((union-doc-p doc)  (better width posn
                                    (be width posn (cons (cons i (union-doc-lhs doc)) tl))
                                    (be width posn (cons (cons i (union-doc-rhs doc)) tl))))))))

(defun better (width posn x y)
  (if (fits (- width posn) x) x y))

(defun fits (width layout-doc)
  "Returns whether a given LAYOUT-DOC occupies no more than WIDTH bytes."
  ; TODO: This was optimized from the paper's version while tired... testing is
  ; needed! (Poking around from the REPL seemed to work, though.)
  (loop
    for hd = (car layout-doc)
    while (stringp hd)
    do (setf width      (- width (length hd))
             layout-doc (cdr layout-doc)))
  (>= width 0))

(defun pretty (stream doc &key width)
  "Pretty-prints a DOCument to the given STREAM, with the given WIDTH."

  (check-type stream (or null string (member t) stream))

  (unless (typep doc 'doc)
    (setf doc (pretty-object doc)))
  (check-type doc doc)

  (unless width
    (setf width (or *print-right-margin*
                    ; TODO: Actually test on one of these implementations, see
                    ; if others have this function as well.
                    #+(or allegro lispworks)
                    (stream-output-width stream))))
  (check-type width (or null fixnum))

  (let ((layout-doc (best width 0 doc)))
    (if stream
        (loop
          for part in layout-doc
          do (check-type part (or fixnum string))
          do (cond
               ((stringp part)
                (princ part stream))
               (t 
                (terpri stream)
                (dotimes (i part)
                  (princ #\space stream)))))
        (let ((out (make-array 10 :element-type 'character :adjustable t
                                  :fill-pointer 0)))
          (loop
            for part in layout-doc
            do (check-type part (or fixnum string))
            do (cond
                 ((stringp part)
                  (loop
                    for ch across part
                    do (vector-push-extend ch out)))
                 (t
                  (vector-push-extend #\newline out)
                  (dotimes (i part)
                    (vector-push-extend #\space out)))))
          out))))
