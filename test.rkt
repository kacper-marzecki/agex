

(defelixir Ecto.Changeset
  (def cast ([Any attrs] Integer) "Ecto.Changeset.cast" ))

(defmodule Kek
  (alias Ecto.Changeset)
  (defn increment 
    ([Integer] Integer) 
    [a] (Math.plus a amount)))

(defmodule Math
  (defn plus
    ([Integer, Integer] Integer)
    [a, b] (+ a b)))