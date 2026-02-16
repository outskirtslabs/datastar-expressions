;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns starfederation.datastar.clojure.expressions
  (:require
   [starfederation.datastar.clojure.expressions.internal :as impl]))

(defmacro ->js
  "Compiles a clojure form into a datastar expression.
  Returns an object supporting Object.toString (or just `str`). These objects compose:

  (let [x (->js (.. evt -target -value))]
    (str (->js (set! $signal ~x))))
  ;; => $signal = evt.target.value"
  [& forms]
  `(impl/d*js ~@forms))

(defmacro ->js-str
  "Compiles a clojure form into a datastar expression, as a string.
  These strings can be difficult to compose:

  (let [x (->js-str (.. evt -target -value))]
    (->js-str (set! $signal ~x)))
  ;; => $signal = \"evt.target.value\"  ;; Note incorrectly quoted js expression.

  Where this matters, prefer `->js`."
  [& forms]
  `(impl/d*js-str ~@forms))
