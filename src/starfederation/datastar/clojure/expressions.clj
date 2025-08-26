;; Copyright © 2025 Casey Link
;; SPDX-License-Identifier: MIT
(ns starfederation.datastar.clojure.expressions
  (:require
   [backtick         :refer [template]]
   [starfederation.datastar.clojure.expressions.internal :as impl]))

(defmacro ->expr
  "Compiles a clojure form into a datastar expression.

  TODO docs "
  [& forms]
  `(impl/d*js-str ~@forms))
