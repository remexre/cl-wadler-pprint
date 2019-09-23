(in-package #:wadler-pprint)

;;;
;;; Utilities
;;;

(defun join (list sep)
  (cdr (loop for x in list nconcing (list sep x))))

;;;
;;; The structs used to represent documents.
;;;

(defstruct nest
  (width (error "Must provide WIDTH") :type fixnum)
  (doc (error "Must provide DOC") :type doc))

(defstruct text
  (string (error "Must provide STRING") :type string))

(defstruct newline
  (string (error "Must provide STRING") :type string))

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
  (or (typep doc 'nest)
      (and (typep doc 'text)
           (not (find #\newline (text-string doc))))
      (typep doc 'newline)
      (and (typep doc 'union-doc)
           (docp (union-doc-lhs doc))
           (docp (union-doc-rhs doc)))
      (and (typep doc 'flatten)
           (docp (flatten-doc doc)))
      (every #'docp doc)))

(deftype doc ()
  "An imprecise check for whether a value is a document."
  '(or nest text newline union-doc flatten list))

;;;
;;; Creation of a document.
;;;

(defgeneric pretty-object (object)
  (:documentation "Converts an object to a document."))

; TODO: Support for circular lists
(defmethod pretty-object ((obj cons))
  (group
    (text "(")
    (nest 1
      (pretty-object (car obj))
      (pretty-tail (cdr obj)))
    (text ")")))

(defun pretty-tail (obj)
  (cond
    ((consp obj)
     (list
       (newline-or " ")
       (pretty-object (car obj))
       (pretty-tail (cdr obj))))
    ((null obj)
     nil)
    (t
     (list (newline-or " ")
           (text ". ")
           (pretty-object obj)))))

(defmethod pretty-object ((obj vector))
  (group
    (text "#(")
    (nest 2 (apply #'stack (map 'list #'pretty-object obj)))
    (text ")")))

(defmethod pretty-object ((obj t))
  (text (format nil "~s" obj)))

(defun nest (width &rest doc)
  (make-nest :width width :doc doc))

(defun text (str)
  (join
    (mapcar (lambda (str) (make-text :string str))
            (uiop:split-string str :separator #(#\newline)))
    (newline-or " ")))

(defun newline-or (str)
  (assert (not (find #\newline str)))
  (make-newline :string str))

(defun group (&rest doc)
  (make-union-doc
    :lhs (flatten doc)
    :rhs doc))

(defun flatten (doc)
  (cond
    ((null doc)        nil)
    ((consp doc)       (cons (flatten (car doc))
                             (flatten (cdr doc))))
    ((nest-p doc)      (make-nest :width (nest-width doc)
                                  :doc   (flatten (nest-doc doc))))
    ((text-p doc)      doc)
    ((newline-p doc)   (make-text :string (newline-string doc)))
    ((union-doc-p doc) (flatten (union-doc-lhs doc)))
    (t                 (error 'type-error :datum doc :expected-type 'doc))))

;;;
;;; Document construction helpers.
;;;

(defun bracket (l r &rest body)
  (group
    (list (text l)
          (nest 2 (cons (newline-or "") body))
          (newline-or "")
          (text r))))

(defun spread (&rest docs)
  (join docs (text " ")))

(defun stack (&rest docs)
  (join docs (newline-or " ")))

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
        ((null doc)        (be width posn tl))
        ((consp doc)       (be width posn `((,i . ,(car doc))
                                            (,i . ,(cdr doc))
                                            ,@tl)))
        ((nest-p doc)      (be width posn (cons (cons (+ i (nest-width doc)) (nest-doc doc))
                                                tl)))
        ((text-p doc)      (cons (text-string doc)
                                 (be width (+ posn (length (text-string doc))) tl)))
        ((newline-p doc)   (cons i (be width i tl)))
        ((union-doc-p doc) (better width posn
                                   (be width posn (cons (cons i (union-doc-lhs doc)) tl))
                                   (be width posn (cons (cons i (union-doc-rhs doc)) tl))))
        (t
         (error 'type-error :datum doc :expected-type 'doc))))))

(defun first-layout (docs)
  (check-type docs list)
  (let (doc i out)
    (tagbody
      continue

      ; Exit if we're at the end of the loop.
      (unless docs
        (go break))

      ; Set up the loop variables.
      (setf i (caar docs))
      (setf doc (cdar docs))
      (setf docs (cdr docs))

      redo
      (cond
        ; Documents that are consumed.
        ((null doc)
         (go continue))
        ((text-p doc)
         (push (text-string doc) out)
         (go continue))
        ((newline-p doc)
         (push i out)
         (go continue))
        ; Documents that decompose to a single document.
        ((nest-p doc)
         (incf i (nest-width doc))
         (setf doc (nest-doc doc))
         (go redo))
        ((union-doc-p doc)
         (setf doc (union-doc-lhs doc))
         (go redo))
        ; Conses also push a document to the queue.
        ((consp doc)
         (push (cons i (cdr doc)) docs)
         (setf doc (car doc))
         (go redo))
        ; If it's unknown, we wanna bail out.
        (t
         (error 'type-error :datum doc :expected-type 'doc)))

      ; The final return.
      break)
    (nreverse out)))

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

(defun pretty (stream value &key width)
  "Pretty-prints a VALUE to the given STREAM, with the given WIDTH."
  (pretty* stream (pretty-object value) :width width))

(defun pretty* (stream doc &key width)
  "Pretty-prints a DOCument to the given STREAM, with the given WIDTH."

  (check-type stream (or null string (member t) stream))
  (check-type doc doc)

  (unless width
    (setf width (or *print-right-margin*
                    ; TODO: Actually test on one of these implementations, see
                    ; if others have this function as well.
                    #+(or allegro lispworks)
                    (stream-output-width stream))))
  (check-type width (or null fixnum))

  (let ((layout-doc (if width
                        (best width 0 doc)
                        (first-layout (list (cons 0 doc))))))
    ; TODO: Is there some way to avoid the duplication here?
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

;;;
;;; Convenience macro
;;;

(defmacro def-pretty-object (class (&key print-object) (&rest slots))
  (check-type class symbol)
  (let (forms
        (object (gensym))
        (stream (gensym)))
    ; Define the pretty-object method.
    (let ((start (format nil "#<~a " class)))
      (push
        `(defmethod pretty-object ((,object ,class))
           (group
             (text ,start)
             (nest ,(length start)
               (stack
                 ,@(loop
                     for slot in slots
                     for name = (concatenate 'string ":" (symbol-name slot))
                     collect `(spread
                                (text ,name)
                                (nest ,(1+ (length name))
                                  (pretty-object (slot-value ,object ',slot)))))))
             (text ">")))
        forms))

    ; Define the print-object method, if requested.
    (when print-object
      (push
        `(defmethod print-object ((,object ,class) ,stream)
           (pretty ,stream (pretty-object ,object)))
        forms))

    ; Return the whole form.
    (cons 'progn (nreverse forms))))
