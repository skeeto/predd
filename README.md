# Elisp Multimethods

This library provides [Clojure-style multimethods](http://clojure.org/multimethods) for Emacs Lisp, supporting multiple dispatch over an ad hoc type hierarchy.

See also: [Clojure-style Multimethods in Emacs Lisp](http://nullprogram.com/blog/2013/12/18/).

## Example Usage

```el
;; `vector' is the dispatch function for the multimethod `show'

(predd-defmulti show #'vector
  "Return how an animal should respond when shown another animal.")

(predd-defmethod show [:cat :mouse] (a b)
  :chase)

(predd-defmethod show [:cat :dog] (a b)
  :run)

(predd-defmethod show [:dog :cat] (a b)
  :bark)

;; Call the multimethod on some different types

(show :cat :dog)    ; => :run
(show :cat :mouse)  ; => :chase

;; Declare that a tabby is a specific type of cat.

(predd-derive :tabby :cat)

(show :dog :tabby)  ; => :bark
```
