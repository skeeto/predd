# Elisp Multimethods

This library provides [Clojure-style multimethods](http://clojure.org/multimethods) for Emacs Lisp, supporting multiple dispatch over an ad hoc type hierarchy.

## Example Usage

```el
;; `vector' is the dispatch function for the multimethod `show'

(multi-defmulti show #'vector
  "Return how an animal should respond when shown another animal.")

(multi-defmethod show [:cat :mouse] (a b)
  :chase)

(multi-defmethod show [:cat :dog] (a b)
  :run)

(multi-defmethod show [:dog :cat] (a b)
  :bark)

;; Call the multimethod on some different types

(show :cat :dog)    ; => :run
(show :cat :mouse)  ; => :chase

;; Declare that a tabby is a specific type of cat.

(multi-derive :tabby :cat)

(show :dog :tabby)  ; => :bark
```
