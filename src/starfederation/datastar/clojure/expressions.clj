;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns starfederation.datastar.clojure.expressions
  (:require
   [starfederation.datastar.clojure.expressions.internal :as impl]))

(defmacro ->js
  "Compiles Clojure forms into a composable Datastar expression.

  Returns an object whose string representation is JavaScript. Prefer this
  when composing generated expressions into other expressions.

  Example:

  ```clojure
  (let [value (->js (.. evt -target -value))]
    (str (->js (set! $signal ~value))))
  ;; => $signal = evt.target.value
  ```"
  [& forms]
  `(impl/d*js ~@forms))

(defmacro ->js-str
  "Compiles Clojure forms into a Datastar expression string.

  Prefer [[->js]] when composing generated expressions; strings interpolate as
  string values.

  Example:

  ```clojure
  (let [value (->js-str (.. evt -target -value))]
    (->js-str (set! $signal ~value)))
  ;; => $signal = \"evt.target.value\"
  ```"
  [& forms]
  `(impl/d*js-str ~@forms))

(defmacro ->expr
  "Compiles Clojure forms into a Datastar expression string.

  Backwards-compatible alias for [[->js-str]]. Prefer [[->js]] when composing
  generated expressions into other expressions."
  [& forms]
  `(impl/d*js-str ~@forms))
