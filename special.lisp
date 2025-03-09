(in-package #:org.tymoonnext.lass)

;;; FUNCS
(macrolet ((define-properties (&rest rest)
             `(progn
                ,@(loop for (name args) on rest by #'cddr
                        collect `(define-simple-property-function ,name ,args)))))
  (define-properties
    rgb (red green blue)
    rgba (red green blue alpha)
    hsl (hue saturation lightness)
    hsla (hue saturation lightness alpha)))
(define-property-function uni (&rest codes) (format NIL "\"~{\\~x~}\"" codes))
(define-property-function hex (hex) (format NIL "#~6,'0d" hex))
(define-property-function url (url) (format NIL "url(~s)" url))
(define-property-function attr (attribute) (format NIL "attr(~a)" (resolve attribute)))

;;; https://www.w3.org/TR/css-variables-1/
(define-property-function var (name &rest fallback-vals)
  (format nil "var(~(~A~)~{~^, ~(~A~)~})" name fallback-vals))

(define-property-function calc (func)
  (with-output-to-string (out)
    (labels ((proc (func)
               (write-string "(" out)
               (if (or (not (listp func))
                       (property-function (first func)))
                   (write-string (resolve func) out)
                   (destructuring-bind (func first &rest rest) func
                     (proc first)
                     (loop :for arg :in rest
                           :do (format out " ~A " (resolve func))
                               (proc arg))))
               (write-string ")" out)))
      (write-string "calc" out)
      (proc func)))) 

(define-simple-property-function counter (var))

;;; BLOCKS

(define-special-block charset (charset)
  (list (list :property (format NIL "@charset ~a" (resolve charset)))))

(define-special-block document (selector &rest body)
  (list (make-superblock
         "document"
         (compile-media-query selector)
         (apply #'compile-sheet body))))

(define-special-block font-face (&rest body)
  (compile-block "@font-face" body))

(define-special-block import (url &rest media-queries)
  (list (make-property
         (format NIL "@import ~a~{ ~a~}"
                 (resolve url)
                 (mapcar #'resolve media-queries)))))

(define-special-block keyframes (identifier &rest body)
  (list (make-superblock
         "keyframes"
         (list (list :constraint :literal (resolve identifier)))
         (apply #'compile-sheet body))))

(define-special-block media (query &rest body)
  (list (make-superblock
         "media"
         (compile-media-query query)
         (apply #'compile-sheet body))))

(define-special-block namespace (prefix/namespace &optional namespace)
  (list (make-property
         (format NIL "@namespace ~a~@[ ~a~]"
                 (resolve prefix/namespace)
                 (when namespace (resolve namespace))))))

(define-special-block page (pseudo-class &rest body)
  (compile-block (format NIL "@page ~a"
                         (if (keywordp pseudo-class)
                             (format NIL ":~a" (string-downcase pseudo-class))
                             (resolve pseudo-class))) body))

(define-special-block supports (selector &rest body)
  (list (make-superblock
         "supports"
         (compile-selector selector)
         (apply #'compile-sheet body))))

(defmacro bind-vars (bindings &body body)
  `(let ((*vars* (let ((table (make-hash-table)))
                   (maphash #'(lambda (k v) (setf (gethash k table) v)) *vars*)
                   (loop for (k . v) in ,bindings
                         do (setf (gethash k table) 
                                  (if (rest v)
                                      v
                                      (resolve (car v)))))
                   table)))
     ,@body))

(define-special-block let (bindings &rest body)
  (bind-vars bindings
             (apply #'compile-sheet body)))

;;; SELECTORS

(defmacro define-attr-comparator (comp &optional (outcomp comp))
  "Helper macro to define an attribute comparator selector."
  `(define-special-selector ,comp (attr value)
     (loop with out = ()
           with values = (compile-selector value)
           for attr in (compile-selector attr)
           do (loop for value in values
                    do (push (list :constraint :attribute attr ,(string outcomp) value) out))
           finally (return (nreverse out)))))

(define-attr-comparator =)
(define-attr-comparator ~=)
(define-attr-comparator *=)
(define-attr-comparator $=)
(define-attr-comparator ^=)
(define-attr-comparator /= \|=)

(defmacro define-single-arg-selector (name)
  "Helper macro to define a single-argument pseudo-selector like NOT or NTH-CHILD."
  `(define-special-selector ,name (arg)
     (loop for arg in (compile-selector arg)
           collect (list :constraint :selector ,(string-downcase name) arg))))

(define-single-arg-selector dir)
(define-single-arg-selector lang)
(define-single-arg-selector nth-child)
(define-single-arg-selector nth-last-child)
(define-single-arg-selector nth-last-of-type)
(define-single-arg-selector nth-of-type)
(define-single-arg-selector is)
(define-single-arg-selector not)
(define-single-arg-selector where)
(define-single-arg-selector has)

;;; ATTRIBUTES

(define-special-property font-family (&rest faces)
  (list (make-property "font-family" (format NIL "~{~a~^, ~}" (mapcar #'resolve faces)))))

(define-special-property content (&rest content)
  (labels ((translate (content)
             (typecase content
               (string
                ;; Backwards compat with the usage of "'foo'" in LASS files
                (when (and (<= 2 (length content))
                           (char= #\' (char content 0))
                           (char= #\' (char content (1- (length content)))))
                  (setf content (subseq content 1 (1- (length content)))))
                (with-output-to-string (out)
                  (write-char #\" out)
                  (unwind-protect
                       (loop for char across content
                             do (when (char= char #\")
                                  (write-char #\\ out))
                                (write-char char out))
                    (write-char #\" out))))
               (T
                (resolve content)))))
    (list (make-property "content" (format NIL "~{~a~^ ~}" (mapcar #'translate content))))))

(defmacro define-browser-property (name args &body browser-options)
  "Helper macro to define properties that have browser-dependant versions.

NAME            --- The base name of the property name or value.
ARGS            --- Property arguments, see DEFINE-SPECIAL-PROPERTY.
BROWSER-OPTIONS ::= (OPTION (symbol) FORM*)
OPTION          ::= :MOZ | :O | :WEBKIT | :MS | :W3C | :DEFAULT

Each browser-option body should return a single property. The SYMBOL
in the option definition is bound to the computed property name
 (eg -moz-NAME for the :MOZ option).
You can define special handling of the browsers by defining options
specifically for them. If no handling is defined, the DEFAULT option
is used as a fallback."
  `(define-special-property ,name ,args
     (list ,@(loop for (browser prefix) in '((:moz "-moz-")
                                             (:o "-o-")
                                             (:webkit "-webkit-")
                                             (:ms "-ms-")
                                             (:w3c ""))
                   for body = (or (assoc browser browser-options)
                                  (assoc :default browser-options))
                   collect `(let ((,(caadr body) ,(format NIL "~a~a" prefix (string-downcase name))))
                              ,@(cddr body))))))

(indent:define-indentation define-browser-property (4 6 &rest (&whole 2 0 4 2)))

(define-browser-property text-stroke (width color)
  (:default (property)
    (make-property property (format NIL "~a ~a" (resolve width) (resolve color)))))

(define-browser-property linear-gradient (direction &rest colors)
  (:default (property)
    (make-property "background" (format NIL "~a(~a~{, ~a ~a~})"
                                        property (resolve direction) (mapcar #'resolve colors)))))

(define-browser-property radial-gradient (shape size position &rest colors)
  (:default (property)
    (make-property "background" (format NIL "~a(~a ~a at ~a~{, ~a ~a~})"
                                        property (resolve shape) (resolve size) (resolve position) (mapcar #'resolve colors)))))

(define-browser-property repeating-radial-gradient (shape size position &rest colors)
  (:default (property)
    (make-property "background" (format NIL "~a(~a ~a at ~a~{, ~a ~a~})"
                                        property (resolve shape) (resolve size) (resolve position) (resolve colors)))))

(define-browser-property transform (value/function &rest function-args)
  (:default (property)
    (make-property property (format NIL "~a~@[(~{~a~^, ~})~]"
                                    (resolve value/function) (mapcar #'resolve function-args)))))

(define-browser-property transform-origin (value/x &optional y z)
  (:default (property)
    (make-property property (format NIL "~a~@[ ~a~]~@[ ~a~]"
                                    (resolve value/x) (resolve y) (resolve z)))))

(define-browser-property transform-style (style)
  (:default (property)
    (make-property property (resolve style))))

(define-browser-property transition (value/property &optional duration timing-function &rest function-args)
  (:default (property)
    (make-property property (format NIL "~a~@[ ~a~]~:[~*~;~:* ~a~@[(~{~a~^, ~})~]~]"
                                    (resolve value/property) (resolve duration) (resolve timing-function) (mapcar #'resolve function-args)))))

(define-browser-property transition-delay (value)
  (:default (property)
    (make-property property (resolve value))))

(define-browser-property transition-duration (value)
  (:default (property)
    (make-property property (resolve value))))

(define-browser-property transition-property (value)
  (:default (property)
    (make-property property (resolve value))))

(define-browser-property transition-timing-function (value/function &rest function-args)
  (:default (property)
    (make-property property (format NIL "~a~@[(~{~a~^, ~})~]"
                                    (resolve value/function) (mapcar #'resolve function-args)))))

(define-browser-property user-select (value)
  (:default (property)
    (make-property property (resolve value))))

(define-browser-property appearance (value)
  (:default (property)
    (make-property property (resolve value))))

(define-simple-property-functions :filter
    (url (url))
  (blur (radius))
  (brightness (value))
  (contrast (value))
  (drop-shadow (x y &optional blur spread color))
  (grayscale (value))
  (hue-rotate (value))
  (invert (value))
  (opacity (value))
  (saturate (value))
  (sepia (value)))

(define-browser-property filter (&rest args)
  (:default (property)
    (make-property property (format NIL "~{~a~^ ~}" (mapcar #'resolve args)))))

(define-browser-property box-shadow (color &optional x y blur spread inset)
  (:default (property)
    (make-property property (format NIL "~a~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]"
                                    (resolve color) (resolve x) (resolve y) (resolve blur) (resolve spread) (resolve inset)))))
